update_rankings_add <- function(season, game_date, ratings, k = 20){
  ## Filters schedule to a specific date
  elo_ratings_update <- season |> filter(date == game_date) |>
    ## Joins the Elo ratings from our rating file to the schedule file. Puts updated ratings in the schedule
    left_join(ratings, by = join_by(away_team == Team)) |>
    rename(away_elo = rating) |>  
    ## Updates ratings for home team in the schedule file
    left_join(ratings, by = join_by(home_team == Team)) |>
    rename(home_elo = rating) |>
    ## Creating an away team outcome variable. Opposite of home team or same if tie.
    mutate(outcome_away = abs(outcome - 1)) |> 
    ## Calculating expected outcome variable for home and away team
    mutate(exp_home = 1/(1 + 10^((away_elo - (home_elo + 50)/400))) |>
    mutate(exp_away = 1/(1 + 10^(((home_elo + 50) - away_elo)/400))) |>
    ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
    mutate(elo_new_home = home_elo + k*(outcome - exp_home)) |>
    mutate(elo_new_away = away_elo + k*(outcome_away - exp_away))
  
  ranked_home <- left_join(ratings, elo_ratings_update, by = join_by(Team == home_team)) |>
    relocate(elo_new_home) |>
    mutate(rating = if_else(!is.na(elo_new_home),
                            true = elo_new_home,
                            false = rating)) |>
    select(Team, rating)
  
  ratings <- left_join(ranked_home, elo_ratings_update, by = join_by(Team == away_team)) |>
    relocate(elo_new_away) |>
    mutate(rating = if_else(!is.na(elo_new_away),
                            true = elo_new_away,
                            false = rating)) |>
    select(Team, rating)
  
  ## Puts updated Elo ratings into our ratings file for home team
  ##ratings$rating = replace(ratings$rating, ratings$Team %in% elo_ratings_update$home_team, elo_ratings_update$elo_new_home)
  
  ## Puts updated Elo ratings into our ratings file for away team
  ##ratings$rating = replace(ratings$rating, ratings$Team %in% elo_ratings_update$away_team, elo_ratings_update$elo_new_away)
  
  return(ratings)
}

##loop to get updated weekly ratings
## creating a vector for unique dates in a schedule dataframe
dates_vec <- unique(schedule$date)
## defining what new_rankings is going to be
new_rankings = rankings
## Creating a for loop with the update_rankings function to update rankings up to a specified date
for (i in dates_vec) {
  new_rankings <- update_rankings(season = schedule, game_date = i, ratings = new_rankings, k = 100)
  
}

# season = schedule
# end_date = "2024-01-07"
# ratings = rankings
# k = 100

## Iteration of update function, using a date filter. Old loop can't deal with NA Values
update_rankings_iter_addins <- function(season, end_date, ratings, k){
  season_cut <- season |> 
    ## Filter by a specified end date to deal with NA values (gmaes that have yet to be played)
    filter(date <= ymd(end_date))
  ## Creating a vector for unique dates in a season
  dates_vector <- unique(season_cut$date)
  ## Defining our rankings within the function
  new_rankings <- ratings
  ## Creating a for loop for the function to generate new ratings with the updtae_rankings function
  for (i in dates_vector) {
    new_rankings <- update_rankings(season = season_cut, game_date = i, ratings = new_rankings, k = k)
  }
  return(new_rankings)
}

try_rankings_addins = update_rankings_iter_addins(season = schedule2324, end_date = "2024-04-13", ratings = X22Rankings, k = 100)