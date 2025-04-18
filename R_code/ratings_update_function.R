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
library(progressr)
library(furrr)
library(vctrs)
library(purrr)

## Initial Rankings of all NCAA D1 Hockey teams
## NCAA_teams <- read_csv("datasets_dataframes/NCAA_teams.csv")
 ## rankings <- NCAA_teams |>
  ## Setting initial Elo rating to 1500 for all teams
  ## mutate(rating = 1500) |>
  ## Taking out the Conference variable to leave 2 columns, Team and Rating
  ## select(Team, rating)

#season = "20242025"

##Function to load in schedule
scrape_men <- function(season = "20232024"){
  ## URL for schedule data frame
  url_hockey <-paste("https://www.collegehockeynews.com/schedules/?season=", season, sep = "")
  ## Selecting which schedule table to grab
  tab_hockey <- read_html(url_hockey) |>
    html_nodes("table")
  
  ## The website likes to switch which table it uses. If function doesn't work try changing which table number you select
  stats_dirty <- tab_hockey[[1]] |> html_table()
  
  ## Creating regex for date, and conference to make date and conference columns in dataframe
  regex_date <- "October|November|December|January|February|March|April"
  regex_conference <- "Atlantic Hockey|Big Ten|CCHA|ECAC|Hockey East|NCHC|Ind|Exhibition|Non-Conference"
  ## Combining regexs with original table so that the original scraped dataframe has date and conferene as variables
  stats_regex <- stats_dirty |> mutate(date = if_else(str_detect(X1, regex_date),
                                                      true = X1, false = NA_character_),
                                       conference = if_else(str_detect(X1, regex_conference), 
                                                            true = X1, false = NA_character_))
  
  ## Filling in respective dates and conferences
  stats_filled <-stats_regex |> fill(date, .direction = "down") |>
    ## Selecting date and congference so they show up as X1 and X2 in the dataframe
    fill(conference, .direction = "down") |> select(date, conference, everything())
  ##filtering out anywhere that a conference value is undetected (Game category, not an actual game played)
  stats_filled_cleaner <- stats_filled |> filter(!str_detect(X1, regex_date) &
                                                   !str_detect(X1, regex_conference))
  print(head(stats_filled_cleaner))
  
  ## Dataframe is now in a format that is able to be worked on. Now creating specific variables that we want to look at
  ## Selecting first 8 columns
  schedule_new <- stats_filled_cleaner |> select(date, conference, X1, X2, X3, X4, X5, X6) |>
    ## Taking out first two rows (no data in them). Renaming columns to match what their variable is.
    slice(-1 , -2) |> rename(game_type = conference, away_team = X1, away_score = X2, location_marker = X3, home_team = X4, home_score = X5, overtime = X6) |> 
    ## Take out the day of the week in our date columns as we don't need to know if a game was played on a Monday per-se. 
    separate(col = date, into = c("weekday", "dm", "y"),
             sep = ", ") |> 
    unite("new_date", c(dm, y),
          sep = " ") |>
    select(-weekday) |>
    ##making date column into a <date> variable
    mutate(date = mdy(new_date)) |>
    ## Taking out the <chr> date variable
    select(-new_date) |> 
    select(date, everything()) |>
    ## Filtering out where there is no away team since that means no game was played
    filter(away_team != "") |>
    ## Filtering out exhibition games since we aren't looking at exhibition games
    filter(game_type != "Exhibition") |>
    ## Turning scores from <chr> to <dbl> variables
    mutate(away_score = as.double(away_score)) |>
    mutate(home_score = as.double(home_score)) |>
    ## creating a variable to indicate if a game was played at a neutral site
    mutate(neutral_site = case_when(location_marker == "vs." ~ 1,
                                    location_marker == "at" ~ 0)) |>
    ## Making the neutral_site variable as <lgl> 
    mutate(neutral_site = as.logical(neutral_site)) |>
    ## taking out location_marker
    select(-location_marker) |>
    ## Making a logical overtime variable. Note we are not differentiating between OT and 2OT
    mutate(overtime = case_when(overtime == "" ~ 0,
                                overtime == "ot" ~ 1,
                                overtime == "2ot" ~ 1)) |>
    mutate(overtime = as.logical(overtime)) |>
    ##Filtering out NA "overtime" values as this indicates no game played, since overtime will either be TRUE or FALSE
    filter(!is.na(overtime))|>
    ## Creating a score differential variable to indicate a win, loss, or tie for the home team. If we know the outcome for the home team, we know the outcome for the away team.
    mutate(score_diff = home_score - away_score) |>
    ## making an outcome variable for home team so ties get input as 0.5, wins get input as 1, and loss get input as 0.
    mutate(outcome = 
             case_when(score_diff == 0 ~ "0.5",
                       score_diff > 0 ~ "1",
                       score_diff < 0 ~ "0")) |>
    ## turning score_diff from <chr> to <dbl>
    mutate(outcome = as.double(outcome)) |>
    ## Filtering out games where D1 team played against D3 teams as these are exhibition as well
    filter(game_type != "Non-Conference v. D3")
  
  ## Tidy schedule is returned
  return(schedule_new)
}

##Load in Schedule
schedule <- scrape_men("20242025")

## Load in my arbitrary initial elo ranking
X22Rankings <- read_csv(here("datasets_dataframes/22Rankings.csv"))

schedule2324 = scrape_men("20232024")

#season = schedule
#game_date = "2024-10-04"
#ratings = X22Rankings
#k = 100

##Function to update rankings
##rating is the variable, ratings is the df.
update_rankings <- function(season, game_date, ratings, k = 20){
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
    mutate(exp_home = 1/(1 + 10^((away_elo - home_elo)/400))) |>
    mutate(exp_away = 1/(1 + 10^((home_elo - away_elo)/400))) |>
    ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
    mutate(elo_new_home = home_elo + k*(outcome - exp_home)) |>
    mutate(elo_new_away = away_elo + k*(outcome_away - exp_away)) |>
    rename(date_update_rankings = date)
  
  ## Find out which is the right date, select() and relocate(), **DO THIS FIRST**: try renaming date in one of the df to make less confusing (at start).
  ranked_home <- left_join(ratings, elo_ratings_update, by = join_by(Team == home_team)) |>
    relocate(elo_new_home) |>
    mutate(rating = if_else(!is.na(elo_new_home),
                            true = elo_new_home,
                            false = rating)) |>
    select(Team, rating, date_update_rankings)
  
  ratings_new <- left_join(ranked_home, elo_ratings_update, by = join_by(Team == away_team)) |>
    relocate(elo_new_away) |>
    mutate(rating = if_else(!is.na(elo_new_away),
                            true = elo_new_away,
                            false = rating)) |>
    select(Team, rating, date_update_rankings.x) |>
    mutate(date_update_rankings.x = game_date) |>
    rename(date = date_update_rankings.x)
    #fill(date_update_rankings.x, .direction = c("down")) |>
    #fill(date_update_rankings.x, .direction = c("up"))
  
  return(ratings_new)
}

#attempt = update_rankings(schedule, "2025-10-04", X22Rankings, 100)

##loop to get updated weekly ratings
## creating a vector for unique dates in a schedule dataframe
dates_vec <- unique(schedule$date)
## defining what new_rankings is going to be
new_rankings = rankings
## Creating a for loop with the update_rankings function to update rankings up to a specified date
for (i in dates_vec) {
  new_rankings <- update_rankings(season = schedule, game_date = i, ratings = new_rankings, k = 100)
  
}


## Iteration of update function, using a date filter. Old loop can't deal with NA Values

## initialize an empty list

#season = schedule
#end_date = "2025-02-25"
#ratings = X22Rankings
#k = 100

update_rankings_iter <- function(season, end_date, ratings, k){
  
  new_rankings <- list()
  
  season_cut <- season |> 
    ## Filter by a specified end date to deal with NA values (gmaes that have yet to be played)
    filter(date <= ymd(end_date))
  ## Creating a vector for unique dates in a season
  dates_vector <- unique(season_cut$date)
  ## Defining our rankings within the function
  new_rankings[[1]] <- ratings
  ## Creating a for loop for the function to generate new ratings with the updtae_rankings function
  for (i in 1:length(dates_vector)) {
    new_rankings[[i + 1]] <- update_rankings(season = season_cut, game_date = dates_vector[i], ratings = new_rankings[[i]], k = k)
  }
  return(new_rankings)
}
## go back to the update_rankings function and have it return the date as 
## a variable in the output

## read more about lists and the structure of them in r

## use bind_rows at try_rankings to bind all of the iterative updates together

## can then join with the schedule data set again by team-home_team and date and then again by team-away_team and date and keep only the necessary columns

#try_rankings = update_rankings(season = schedule, game_date = "2025-03-05", ratings = X22Rankings, k = 100)
#try_rankings24 = update_rankings_iter(schedule, "2025-03-05", X22Rankings, 100)

## Binding rows of rankings to make a master rankings file and lagging date to get correct date since the function does i + 1 do the date
#try_rankings24 = try_rankings24 |> bind_rows()

#lagged_df <- try_rankings24 |> group_by(date) |>
  #summarise(last_date = last(date)) |>
  #mutate(lag_date = lag(last_date)) |>
  #select(-last_date)

#rankings_lagged <- left_join(try_rankings24, lagged_df, join_by(date == lag_date)) |>
  #select(-date) |>
  #rename(date = date.y)

##Joining rankings into a master schedule

#schedule = schedule |>
  #mutate(home_elo = NA) |>
  #mutate(away_elo = NA)
  
#merged_sched_home = left_join(schedule, rankings_lagged, 
                        # by = join_by(date == date, home_team == Team)) |>
  #mutate(home_elo = rating) |>
  #select(-rating)

#merged_sched = left_join(merged_sched_home, rankings_lagged,
                         #by = join_by(date == date, away_team == Team)) |>
 # mutate(away_elo = rating) |>
  #select(-rating)

#schedule2425 = merged_sched |>
  #mutate(outcome_away = abs(outcome - 1)) |> 
  ## Calculating expected outcome variable for home and away team
  #mutate(exp_home = 1/(1 + 10^((away_elo - home_elo)/400))) |>
  #mutate(exp_away = 1/(1 + 10^((home_elo - away_elo)/400))) |>
  ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
 # mutate(elo_new_home = home_elo + 100 * (outcome - exp_home)) |>
 # mutate(elo_new_away = away_elo + 100 * (outcome_away - exp_away))
#print("BOOM TIME TO DO SOME ANALYSIS!!!")



## Making a function that can bind the outputs that we did manually.

## Function that takes into consideration goal differential, home_ice, k

##Function to update rankings
##rating is the variable, ratings is the df.
update_rankings_gd_ha <- function(season, game_date, ratings, k = 100, home_ice = 50, d = 0.5){
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
    mutate(exp_home = 1/(1 + 10^((away_elo - (home_elo + home_ice))/400))) |>
    mutate(exp_away = 1/(1 + 10^(((home_elo + home_ice) - away_elo)/400))) |>
    ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
    mutate(elo_new_home = home_elo + k*(outcome - exp_home) + d * score_diff) |>
    mutate(elo_new_away = away_elo + k*(outcome_away - exp_away) + d * -1 * (score_diff)) |>
    rename(date_update_rankings = date)
  
  ## Find out which is the right date, select() and relocate(), **DO THIS FIRST**: try renaming date in one of the df to make less confusing (at start).
  ranked_home_gd_ha <- left_join(ratings, elo_ratings_update, by = join_by(Team == home_team)) |>
    relocate(elo_new_home) |>
    mutate(rating = if_else(!is.na(elo_new_home),
                            true = elo_new_home,
                            false = rating)) |>
    select(Team, rating, date_update_rankings)
  
  ratings_new_gd_ha <- left_join(ranked_home_gd_ha, elo_ratings_update, by = join_by(Team == away_team)) |>
    relocate(elo_new_away) |>
    mutate(rating = if_else(!is.na(elo_new_away),
                            true = elo_new_away,
                            false = rating)) |>
    select(Team, rating, date_update_rankings.x) |>
    mutate(date_update_rankings.x = game_date) |>
    rename(date = date_update_rankings.x)
  #fill(date_update_rankings.x, .direction = c("down")) |>
  #fill(date_update_rankings.x, .direction = c("up"))
  
  return(ratings_new_gd_ha)
}


#try_rankings24 = update_rankings_iter(schedule, "2025-03-05", X22Rankings, 100)


##Final function Making starts here###

rankings = X22Rankings

##Function to update rankings
##rating is the variable, ratings is the df.
update_rankings_gd_ha <- function(season, game_date, ratings, k = 100, home_ice = 50, d = 0.5){
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
    mutate(exp_home = 1/(1 + 10^((away_elo - (home_elo + home_ice))/400))) |>
    mutate(exp_away = 1/(1 + 10^(((home_elo + home_ice) - away_elo)/400))) |>
    ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
    mutate(elo_new_home = home_elo + k*(outcome - exp_home) + d * score_diff) |>
    mutate(elo_new_home = if_else(elo_new_home < 100,
                                  true = 100,
                                  false = elo_new_home)) |>
    mutate(elo_new_away = away_elo + k*(outcome_away - exp_away) + d * -1 * (score_diff)) |>
    mutate(elo_new_away = if_else(elo_new_away < 100,
                                  true = 100,
                                  false = elo_new_away)) |>
    rename(date_update_rankings = date)
  
  ## Find out which is the right date, select() and relocate(), **DO THIS FIRST**: try renaming date in one of the df to make less confusing (at start).
  ranked_home_gd_ha <- left_join(ratings, elo_ratings_update, by = join_by(Team == home_team)) |>
    relocate(elo_new_home) |>
    mutate(rating = if_else(!is.na(elo_new_home),
                            true = elo_new_home,
                            false = rating)) |>
    select(Team, rating, date_update_rankings)
  
  ratings_new_gd_ha <- left_join(ranked_home_gd_ha, elo_ratings_update, by = join_by(Team == away_team)) |>
    relocate(elo_new_away) |>
    mutate(rating = if_else(!is.na(elo_new_away),
                            true = elo_new_away,
                            false = rating)) |>
    select(Team, rating, date_update_rankings.x) |>
    mutate(date_update_rankings.x = game_date) |>
    rename(date = date_update_rankings.x)
  #fill(date_update_rankings.x, .direction = c("down")) |>
  #fill(date_update_rankings.x, .direction = c("up"))
  
  return(ratings_new_gd_ha)
}

#attempt = update_rankings(schedule, "2025-10-04", X22Rankings, 100)

##loop to get updated weekly ratings
## creating a vector for unique dates in a schedule dataframe
dates_vec <- unique(schedule$date)
## defining what new_rankings is going to be
new_rankings = rankings
## Creating a for loop with the update_rankings function to update rankings up to a specified date
for (i in dates_vec) {
  new_rankings <- update_rankings_gd_ha(season = schedule, game_date = i, ratings = new_rankings, k = 100, home_ice = 50, d = 0.5)
  
}


## Iteration of update function, using a date filter. Old loop can't deal with NA Values

## initialize an empty list

#season = schedule
#end_date = "2025-02-25"
#ratings = X22Rankings
#k = 100

update_rankings_iter_gd_ha <- function(season, end_date, ratings, k, home_ice, d){
  
  new_rankings <- list()
  
  season_cut <- season |> 
    ## Filter by a specified end date to deal with NA values (gmaes that have yet to be played)
    filter(date <= ymd(end_date))
  ## Creating a vector for unique dates in a season
  dates_vector <- unique(season_cut$date)
  ## Defining our rankings within the function
  new_rankings[[1]] <- ratings
  ## Creating a for loop for the function to generate new ratings with the updtae_rankings function
  for (i in 1:length(dates_vector)) {
    new_rankings[[i + 1]] <- update_rankings_gd_ha(season = season_cut, game_date = dates_vector[i], ratings = new_rankings[[i]], k = k, home_ice = home_ice, d = d)
  }
  return(new_rankings)
}

update_rankings_residuals = function(season, end_date, ratings, k, home_ice, d){ 
  
  new_rankings = update_rankings_iter_gd_ha(season = season, end_date = end_date, ratings = ratings, k = k, home_ice = home_ice, d = d)
  
  full_rankings = new_rankings |> bind_rows()
  
  lagged_dates = full_rankings |> group_by(date) |>
    summarise(last_date = last(date)) |>
    mutate(lag_date = lag(last_date)) |>
    select(-last_date)
  
  lagged_rankings <- left_join(full_rankings, lagged_dates, join_by(date == lag_date)) |>
    select(-date) |>
    rename(date = date.y)
  
  season = season |>
    mutate(home_elo = NA) |>
    mutate(away_elo = NA)
  
  merged_season_home = left_join(season, lagged_rankings, 
                                 by = join_by(date == date, home_team == Team)) |>
    mutate(home_elo = rating) |>
    select(-rating)
  
  merged_season =  left_join(merged_season_home, lagged_rankings,
                             by = join_by(date == date, away_team == Team)) |>
    mutate(away_elo = rating) |>
    select(-rating)
  
  full_season = merged_season |>
    mutate(outcome_away = abs(outcome - 1)) |> 
    ## Calculating expected outcome variable for home and away team
    mutate(exp_home = 1/(1 + 10^((away_elo - (home_elo + home_ice))/400))) |>
    mutate(exp_away = 1/(1 + 10^(((home_elo + home_ice) - away_elo)/400))) |>
    ## Using expected outcome variable to generate new Elo ratings based on actual outcome and expected outcome
    mutate(elo_new_home = home_elo + k*(outcome - exp_home) + d * score_diff) |>
    mutate(elo_new_home = if_else(elo_new_home < 100,
                                  true = 100,
                                  false = elo_new_home)) |>
    mutate(elo_new_away = away_elo + k*(outcome_away - exp_away) + d * -1 * (score_diff)) |>
    mutate(elo_new_away = if_else(elo_new_away < 100,
                                  true = 100,
                                  false = elo_new_away)) |>
    mutate(residual = outcome - exp_home) |>
    mutate(abs_residual = abs(residual))
  
  mean_residual = full_season |> summarise(avg = mean(abs_residual, na.rm = TRUE))
  
  return(pull(mean_residual))
}


## Getting rankings from 2324:
## loading in the 2324 season
schedule2324 = scrape_men("20232024")

## running the 2023-24 season through the update_rankings_iter() function
rankings2324 = update_rankings_iter(schedule2324, "2024-04-13", X22Rankings, 100)[[105]]

## adjusting the rankings for preseason rankings using the same method as fivethirtyeight.com

rankings2324 = rankings2324 |>
  mutate(rating = rating * 0.7 + (0.3 * 1500)) |>
  select(-date)

## Optimizing with regular season gamees only:
schedule_reg = schedule |> filter(game_type != "Big Ten Tournament") |>
  filter(game_type != "ECAC Tournament") |>
  filter(game_type != "CCHA Tournament") |>
  filter(game_type != "CCHA Tournament") |>
  filter(date < "2025-03-08")


## MAPPING ##

## setting up grid of potential k, d, and home ice values
plan(multisession)
handlers("progress")
options(progressr.enable = TRUE)

#k = seq(40, 50, length.out = 10)
#home_ice = seq(60, 70, length.out = 10)
#d = seq(45, 55, length.out = 10)

grid = expand.grid(k = seq(35, 45, length.out = 10), home_ice = seq(50, 60, length.out = 10), d = seq(40, 50, length.out = 10))

mean_residuals = with_progress({future_pmap_dbl(grid, \ (k, home_ice, d) update_rankings_residuals(season = schedule_reg, end_date = "2025-03-25", ratings = rankings2324, k = k, home_ice = home_ice, d = d), .progress = TRUE)})

residual_df <- grid |> mutate(mean_residual = mean_residuals)
residual_df

optimal = residual_df |> filter(mean_residual == min(mean_residual))

apr_15_ranking = update_rankings_iter_gd_ha(schedule, "2025-04-15", rankings2324, 38.33333, 53.33333, 43.33333)
