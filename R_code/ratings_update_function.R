##Loading in libraries
library(elo)
library(dplyr)
library(tidyverse)
library(cowplot)
library(ggrepel)
library(lubridate)
library(rvest)
library(here)
library(forcats)

##Function to load in schedule
scrape_men <- function(season = "20232024"){
  url_hockey <-paste("https://www.collegehockeynews.com/schedules/?season=", season, sep = "")
  tab_hockey <- read_html(url_hockey) |>
    html_nodes("table")
  
  stats_dirty <- tab_hockey[[3]] |> html_table()
  
  regex_date <- "October|November|December|January|February|March|April"
  regex_conference <- "Atlantic Hockey|Big Ten|CCHA|ECAC|Hockey East|NCHC|Ind|Exhibition|Non-Conference"
  stats_regex <- stats_dirty |> mutate(date = if_else(str_detect(X1, regex_date),
                                                      true = X1, false = NA_character_),
                                       conference = if_else(str_detect(X1, regex_conference), 
                                                            true = X1, false = NA_character_))
  
  stats_filled <-stats_regex |> fill(date, .direction = "down") |>
    fill(conference, .direction = "down") |> select(date, conference, everything())
  stats_filled_cleaner <- stats_filled |> filter(!str_detect(X1, regex_date) &
                                                   !str_detect(X1, regex_conference))
  print(head(stats_filled_cleaner))
  
  schedule_new <- stats_filled_cleaner |> select(date, conference, X1, X2, X3, X4, X5, X6) |>
    slice(-1 , -2) |> rename(game_type = conference, away_team = X1, away_score = X2, location_marker = X3, home_team = X4, home_score = X5, overtime = X6) |> 
    separate(col = date, into = c("weekday", "dm", "y"),
             sep = ", ") |> 
    unite("new_date", c(dm, y),
          sep = " ") |>
    select(-weekday) |>
    mutate(date = mdy(new_date)) |>
    select(-new_date) |> 
    select(date, everything()) |>
    filter(away_team != "") |>
    filter(game_type != "Exhibition") |>
    mutate(away_score = as.double(away_score)) |>
    mutate(home_score = as.double(home_score)) |>
    mutate(neutral_site = case_when(location_marker == "vs." ~ 1,
                                    location_marker == "at" ~ 0)) |>
    mutate(neutral_site = as.logical(neutral_site)) |>
    select(-location_marker) |>
    mutate(overtime = case_when(overtime == "" ~ 0,
                                overtime == "ot" ~ 1,
                                overtime == "2ot" ~ 1)) |>
    mutate(overtime = as.logical(overtime)) |>
    filter(!is.na(overtime))|>
    mutate(score_diff = home_score - away_score) |>
    mutate(outcome = 
             case_when(score_diff == 0 ~ "0.5",
                       score_diff > 0 ~ "1",
                       score_diff < 0 ~ "0")) |>
    mutate(outcome = as.double(outcome)) |>
    filter(game_type != "Non-Conference v. D3")
  
  return(schedule_new)
}

##Load in Schedule
schedule <- scrape_men("20232024")

##Function to update rankings
update_rankings <- function(season, game_date, ratings, k = 20){
  elo_ratings_update <- season |> filter(date == game_date) |>
    left_join(ratings, by = join_by(away_team == Team)) |>
    rename(away_elo = rating) |>  
    left_join(ratings, by = join_by(home_team == Team)) |>
    rename(home_elo = rating) |>
    mutate(outcome_away = abs(outcome - 1)) |> 
    mutate(exp_home = 1/(1 + 10^((away_elo - home_elo)/400))) |>
    mutate(exp_away = 1/(1 + 10^((home_elo - away_elo)/400))) |>
    mutate(elo_new_home = home_elo + k*(outcome - exp_home)) |>
    mutate(elo_new_away = away_elo + k*(outcome_away - exp_away))
  
  ratings$rating = replace(ratings$rating, ratings$Team %in% elo_ratings_update$home_team, elo_ratings_update$elo_new_home)
  
  ratings$rating = replace(ratings$rating, ratings$Team %in% elo_ratings_update$away_team, elo_ratings_update$elo_new_away)
  
  return(ratings)
}

##loop to get updated weekly ratings
dates_vec <- unique(schedule$date)
new_rankings = rankings
for (i in dates_vec) {
  new_rankings <- update_rankings(season = schedule, game_date = i, ratings = new_rankings, k = 20)
  
}
