library(here)
library(glue)
library(memoise)
library(rvest)
library(tidyverse)

cd <- cachem::cache_disk(here("cache"))

#' Get a table node for a given url and selector
#'
#' @param url A relative URL on hockey reference
#' @param selectors A list of CSS selectors to try.
#'
#' @return NA if the table doesn't exist, otherwise an html_node() with the table
construct_table_node <- function(url, selectors) {
  html <- glue("https://www.hockey-reference.com{url}") %>%
    read_html() %>%
    gsub("<!--", "", .) %>%
    gsub("-->", "", .) %>%
    read_html()

  valid_selectors <- selectors %>% discard(~is.na(html_node(html, .x)))
  if (length(valid_selectors) == 0) {
    return(NA)
  }
  selector <- valid_selectors[[1]]

  rows <- html %>% html_nodes(glue("{selector} tbody tr:not(.thead)"))
  header <- html %>% html_node(glue("{selector} thead tr:not(.over_header)"))
  rows_string <- rows %>% lapply(as.character) %>% unlist %>% paste0(collapse="")
  artif <- glue("<table>{header}{rows_string}</table>")
  artif %>%
    read_html() %>%
    html_node("table")
}

#' Gets the links for the worst teams of a given year
#'
#' @param year An int year, like 2021
#'
#' @return A list of chr strings, each a relative path on hockey-reference for a "worst team" from that year
worst_team_links_from_year <- memoise(function(year) {
  table_node <- construct_table_node(glue("/leagues/NHL_{year}.html"), "#stats")

  if (is.na(table_node)) {
    stop(glue("could not fetch {year}"))
  }

  pts <- table_node %>%
    html_table() %>%
    pull(PTS)

  links <- table_node %>%
    html_nodes("a") %>%
    html_attr("href")

  standings <- tibble(links = links, pts = pts)
  min_points <- standings %>% pull(pts) %>% min()
  worst_teams <- standings %>% filter(pts == min_points) %>% pull(links)
  worst_teams
}, cache = cd)

#' Gets the links for the players on a team
#'
#' @param year An relative path for a team's season.
#'
#' @return A tibble with Player, Pos, PlayerLink, TeamLink columns
players_from_link <- memoise(function(link) {
  table_node <- construct_table_node(link, "#roster")

  if (is.na(table_node)) {
    stop(glue("could not fetch {link}"))
  }

  players <- table_node %>%
    html_table() %>%
    select(Player, Pos)
  player_links <- table_node %>%
    html_nodes("a") %>%
    html_attr("href")
  players %>%
    mutate(PlayerLink = player_links, TeamLink = link)
}, cache = cd)

#' Gets the value for a given goalie
#'
#' @param goalie_link An relative path for a goalie.
#'
#' @return A double representing that goalie's value
value_from_goalie <- memoise(function(goalie_link) {
  table_node <- construct_table_node(goalie_link, c("#stats_basic_plus_nhl", "#stats_basic_nhl"))

  if (is.na(table_node)) {
    return (0.0)
  }

  ps <- table_node %>%
    html_table() %>%
    select(GPS) %>%
    pull(GPS)

  ps %>%
    sort(decreasing = TRUE) %>%
    .[1]
}, cache = cd)

#' Gets the value for a given player
#'
#' @param player_link An relative path for a player
#'
#' @return A double representing that player's value
value_from_player <- memoise(function(player_link) {
  table_node <- construct_table_node(player_link, "#stats_misc_plus_nhl")

  if (is.na(table_node)) {
    return (0.0)
  }

  ps <- table_node %>%
    html_table() %>%
    select(PS) %>%
    pull(PS)

  ps %>%
    sort(decreasing = TRUE) %>%
    .[1]
}, cache = cd)

#' Gets the players for a given year along with their values
#'
#' @param year An int year, like 2021
#'
#' @return A tibble with columns Player, Pos, PlayerLink, TeamLink, Value
valued_players_from_year <- function(year) {
  year %>%
    worst_team_links_from_year() %>%
    map_dfr(players_from_link) %>%
    rowwise() %>%
    mutate(Value = ifelse(Pos == "G", value_from_goalie(PlayerLink), value_from_player(PlayerLink))) %>%
    ungroup()
}

main <- function(year_range = 1975:2021) {
  all_players <- year_range %>%
    discard(~.x == 2005) %>% # :( 
    map_dfr(valued_players_from_year) %>%
    mutate(Player = str_trim(str_replace(Player, fixed("(C)"), ""))) %>%
    mutate(Pos = case_when(Pos == "F" | Pos == "C" | Pos == "LW" | Pos == "RW" ~ "F", TRUE ~ Pos)) %>%
    arrange(-Value) %>%
    distinct(Player, .keep_all = TRUE)
  best_forwards <- all_players %>%
    filter(Pos == "F") %>%
    slice_max(Value, n = 48, with_ties = FALSE)
  best_defensemen <- all_players %>%
    filter(Pos == "D") %>%
    slice_max(Value, n = 24, with_ties = FALSE)
  best_goalies <- all_players %>%
    filter(Pos == "G") %>%
    slice_max(Value, n = 12, with_ties = FALSE)
  wyshinski_roster <- c(
    "Pat LaFontaine",
    "Joe Sakic",
    "Nathan MacKinnon",
    "Mats Sundin",
    "Vincent Lecavalier",
    "Lanny McDonald",
    "Rick Nash",
    "Ryan O'Reilly",
    "Dany Heatley",
    "Owen Nolan",
    "Alexei Yashin",
    "Jeff Carter",
    "Ray Bourque",
    "Brian Campbell",
    "Ken Daneyko",
    "Glen Wesley",
    "Phil Housley",
    "Al Iafrate",
    "Jonathan Quick",
    "Ed Giacomin"
  )
  mcindoe_roster <- c(
    "Peter Forsberg",
    "Steve Yzerman",
    "Guy Lafleur",
    "Martin St. Louis",
    "Adam Oates",
    "Peter Stastny",
    "Daniel Alfredsson",
    "Dave Andreychuk",
    "Dino Ciccarelli",
    "Jonathan Huberdeau",
    "Jack Eichel",
    "Wendel Clark",
    "Zdeno Chara",
    "Doug Wilson",
    "Randy Carlyle",
    "Roman Hamrlik",
    "Morgan Rielly",
    "Marty McSorley",
    "Billy Smith",
    "Marc-Andre Fleury"
    )
  lambert_roster <- c(
    "Bobby Hull",
    "Mario Lemieux",
    "Jarome Iginla",
    "Ilya Kovalchuk",
    "Ron Francis",
    "Alexei Kovalev",
    "Taylor Hall",
    "Gilbert Perreault",
    "Eric Staal",
    "Keith Tkachuk",
    "Rod Brind'Amour",
    "Mark Stone",
    "Bjore Salming",
    "Mark Green",
    "Dan Boyle",
    "Dion Phaneuf",
    "Derian Hatcher",
    "Mark Streit",
    "Ryan Miller",
    "Robin Lehner"
    )
  rosters <- bind_rows(
    tibble(Player = wyshinski_roster, Roster = "Wyshinski"),
    tibble(Player = lambert_roster, Roster = "Lambert"),
    tibble(Player = mcindoe_roster, Roster = "McIndoe"),
  )
  scored_rosters <- all_players %>% inner_join(rosters, by = "Player")
  results <- scored_rosters %>%
    group_by(Roster) %>%
    summarize(
      F = sum(Value[Pos=="F"]),
      D = sum(Value[Pos=="D"]),
      G = sum(Value[Pos=="G"]),
      Value = sum(Value)
    )
}

