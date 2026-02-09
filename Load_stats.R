#load packages
library("hoopR")
library("tidyverse")

SEASON = 2023

# 1. Get player data
boxscore_raw_data <- load_nba_player_box(seasons=SEASON)
allplayers_raw_data <- nba_commonallplayers(season = SEASON)[[1]]
#sum games

# get years played
year_played_data <- allplayers_raw_data %>%
  mutate(
    startyear=as.numeric(FROM_YEAR,na.rm=TRUE),
    endyear=pmin(as.numeric(TO_YEAR,na.rm=TRUE),SEASON),
    years_played=pmax(endyear-startyear,0)
    ) %>%
  select(PERSON_ID,DISPLAY_FIRST_LAST,years_played)

#injury history
dnp_why <-select(boxscore_raw_data,did_not_play,reason)


#total minutes, fouls, games started, games played
player_totals <- boxscore_raw_data %>%
  group_by(athlete_id, athlete_display_name) %>% 
  summarize(
    total_minutes = sum(minutes, na.rm = TRUE),
    total_fouls = sum(fouls,na.rm = TRUE),
    total_games_started = sum(starter,na.rm = TRUE),
    games_played  = n()
  ) %>%
  ungroup()