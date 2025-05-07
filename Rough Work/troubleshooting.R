##Function to update rankings

library(tidyverse)
library(mosaic)
##update_rankings <- function(season, game_date, ratings, k = 20){
  
## Filters schedule to a specific date
  elo_ratings_update_test <- schedule |> 
    ## Joins the Elo ratings from our rating file to the schedule file. Puts updated ratings in the schedule
    left_join(X22Rankings, by = join_by(away_team == Team)) |>
    rename(away_elo = rating) |>  
    ## Updates ratings for home team in the schedule file
    left_join(X22Rankings, by = join_by(home_team == Team)) |>
    rename(home_elo = rating) |>
    ## Creating an away team outcome variable. Opposite of home team or same if tie.
    mutate(outcome_away = abs(outcome - 1)) |> 
    ## Calculating expected outcome variable for home and away team
    mutate(exp_home = 1/(1 + 10^((away_elo - home_elo)/400))) |>
    mutate(exp_away = 1/(1 + 10^((home_elo - away_elo)/400))) |>
    ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
    mutate(elo_new_home = home_elo + 100*(outcome - exp_home)) |>
    mutate(elo_new_away = away_elo + 100*(outcome_away - exp_away)) |>
    filter(date == "2024-10-04")
  
 ranked_home <- left_join(X22Rankings, elo_ratings_update_test, by = join_by(Team == home_team)) |>
   relocate(elo_new_home) |>
    mutate(rating = if_else(!is.na(elo_new_home),
                            true = elo_new_home,
                            false = rating)) |>
   select(Team, rating, date)
 
 ranked <- left_join(ranked_home, elo_ratings_update_test, by = join_by(Team == away_team)) |>
   relocate(elo_new_away) |>
   mutate(rating = if_else(!is.na(elo_new_away),
                           true = elo_new_away,
                           false = rating)) |>
   select(Team, rating, date.y)
 
  ## ERROR OCCURS IN THE REPLACE FUNCTION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
  ## Puts updated Elo ratings into our ratings file for home team
  ###return(ratings)

  ##}