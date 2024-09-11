## Task List

### For September 5

1. Look into possible data sources for D1 Hockey scores.

Here are some potential data bases:

1) https://www.collegehockeynews.com/stats/

2) https://www.eliteprospects.com/games/2023-2024/ncaa/all-teams?from=&to=

3) https://www.uscho.com/scoreboard/division-i-men/2023-2024/list-1/

### For September 12

1. Make a list of teams and conferences from standings.

2. Scrape the league schedule and put data in a tidy format (e.g. get dates assigned, pull out teams not on the team list, have columns of "home_team", "away_team", "date", "score", "ot_or_not", etc.). See 234 notes for a review of `rvest`.


## Update: 08SEP2024

Data set for teams + conference complete.

Having trouble scraping. Used the same exact setup from the DATA/STAT 234 course notes, error codes and R issues galore. I will push the document I'm working in to you, It describes the issue in more detail.

Second Update from 08SEP2024:

Resolved my scraping issue. Will start on tidying. Will need your help to start me in the right direction. The table is extremely "untidy"


## UPDATE 11SEP2024:

I HAVE MADE THE TIDY DF!!! I AM SO HAPPY RIGHT NOW: I'm also pushing the quarto doc with all the code in it. But essentially the rundown is that I created a date variable for game date, game_type character variable to indicate whether game was conference, non-conference ect, created home and away team variables that are categorical along with home and away team scores which are double, lastly created overtime and neutral site logical variable to indicate if game was played at a neutral site (eliminating major effects of being home/away), and an overtime logical variable to indicate whether the game went to over time or not.

There may still be slight effects for the listed home/away teams for neutral site as in some leagues the "listed" home team is able to make the final change before a faceoff.