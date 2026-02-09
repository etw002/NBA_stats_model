#load packages
library("hoopR")
library("tidyverse")

# 1. Get player data
boxscore_raw_data <- load_nba_player_box(seasons=2023)
frequency_data2023 <- select(boxscore_raw_data,athlete_id,athlete_display_name,team_name,minutes,starter)
allplayers_raw_data <- nba_commonallplayers(season = 2023)[[1]]
#sum games

# get years played
year_played_data <- select(allplayers_raw_data,PERSON_ID,DISPLAY_FIRST_LAST,FROM_YEAR,TO_YEAR)
year_played_data <- mutate(year_played_data,years_played=as.numeric(TO_YEAR) - as.numeric(FROM_YEAR))
year_played_data <- select(year_played_data,c(-TO_YEAR,-FROM_YEAR))

#injury history
dnp_why <-select(boxscore_raw_data,did_not_play,reason)
#total minutes

#starter/bench

#draft pick