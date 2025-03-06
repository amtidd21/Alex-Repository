## Task List

## For March 13

1. Construct initial ratings from 2023-2024 season. Shrink them according to what five thirty eight did.

2. Weight points with more games more in the model and also make the point size of these points larger.

3. Adjust endpoints of bins so that they are centered at the midpoint. And use fewer bins! (Try 15-20).

## For March 6

1. Work on function to fix the remaining bugs.

2. Continue to mess around with k, home ice advantage, and goal differential.

3. Start computing probability minus win/loss, and the plot of probability and proportion of times the home team won.

## For February 27

1. organize the ratings_update_function.R file so that upon restarting R, everything runs.

2. Continue to mess around with k, home ice advantage, and goal differential.

3. Start computing probability minus win/loss, and the plot of probability and proportion of times the home team won.


## For December 12

1. Comment current functions.

2. Put loop into a function with current schedule and end date as arguments.

## For November 21

1. Fix scraping function.

2. Pull data from new season.

3. For the new season, update the loop so that it only runs through up to a certain date.

4. Turn loop into a function: inputs are start date and end date as well as season and ratings and output should still be updating rating.


## For November 14

1. Write code to loop through the function for a given set of dates (`unique(elo_ratings_initial$date`).

## For November 7

1. Put code into a function with three inputs: schedule, set_of_ratings, date and one output: set_of_ratings. As you're writing, think about whether you need the initial data set, or, if you can sub in elo_main instead).

2. Write code to loop through the function for a given set of dates (`unique(elo_ratings_initial$date`).

3. rename `elo_ratings_initial` to `elo_schedule` and `elo_main` to `elo_ratings`.

## For October 10

1. Add to the working ELO code an additional update iteration (through using joining).

2. Short write-up on one ELO update.

## 03OCT2024 UPDATE:

Was able top get Elo ratings for all teams that played on October 7 2023.
Is it the best way to do it? No.
Does it work for this specific task at hand? I believe so!
Sadly, not all the teams played on that specific day, and some teams played twice.
Since we wanted to get the score for a specific date, not all the teams are there.
But can easily repeat the process for the next days and so forth!

## For October 3

1. Take the following

    * data frame with team names and starting ELO ratings
    * data frame with a schedule (home team, away team, whether or not the home team wins) and a result
    * k variable

and turn it into:

    * a data frame with the team names and their updated ELO ratings.

## For September 26

1. Get code working out of the loop.

2. Loop through the days (input is current elo ratings for everyone, output is updated elo ratings after that day).

    * write a function that takes a set of ELO ratings and a day. Output is a new set of ELO ratings.

##Sept 19 update:

looked through hope's thesis and her model to make her elo seemed pretty complicated. So the I went to the article the Dr. Lock sent me and it had a very user-friendly elo code that I could base mine off of! Wrote the code almost exactly as the lesson showed and almost got it to run. Also, tweaked my dataframe and function to make an "outcome" and "score differential" variable.

### For September 19

1. Filter out missing overtime scores.

2. Look at Hope's document for getting started calculating ELO for a single season. 

3. Start writing code to apply that to hockey (either with score differential or win/loss: whatever seems easier).


### For September 12

1. Make a list of teams and conferences from standings.

2. Scrape the league schedule and put data in a tidy format (e.g. get dates assigned, pull out teams not on the team list, have columns of "home_team", "away_team", "date", "score", "ot_or_not", etc.). See 234 notes for a review of `rvest`.

### For September 5

1. Look into possible data sources for D1 Hockey scores.

Here are some potential data bases:

1) https://www.collegehockeynews.com/stats/

2) https://www.eliteprospects.com/games/2023-2024/ncaa/all-teams?from=&to=

3) https://www.uscho.com/scoreboard/division-i-men/2023-2024/list-1/




## Update: 08SEP2024

Data set for teams + conference complete.

Having trouble scraping. Used the same exact setup from the DATA/STAT 234 course notes, error codes and R issues galore. I will push the document I'm working in to you, It describes the issue in more detail.

Second Update from 08SEP2024:

Resolved my scraping issue. Will start on tidying. Will need your help to start me in the right direction. The table is extremely "untidy"


## UPDATE 11SEP2024:

I HAVE MADE THE TIDY DF!!! I AM SO HAPPY RIGHT NOW: I'm also pushing the quarto doc with all the code in it. But essentially the rundown is that I created a date variable for game date, game_type character variable to indicate whether game was conference, non-conference ect, created home and away team variables that are categorical along with home and away team scores which are double, lastly created overtime and neutral site logical variable to indicate if game was played at a neutral site (eliminating major effects of being home/away), and an overtime logical variable to indicate whether the game went to over time or not.

There may still be slight effects for the listed home/away teams for neutral site as in some leagues the "listed" home team is able to make the final change before a faceoff.