#load packages
library("hoopR")
library("tidyverse")
library("stringi")

SEASON = 2023

# 1. Get player data
boxscore_raw_data <- load_nba_player_box(seasons=SEASON) %>%
  filter(season_type==2)
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
dnp_why <-boxscore_raw_data %>%
  filter(did_not_play==TRUE, reason!="COACH'S DECISION") %>%
  #filter out suspension and logistical issues
  filter(reason!="SUSPENDED BY LEAGUE",
         reason!="NOT WITH TEAM",
         reason!="DID NOT DRESS",
         reason!="TRADE PENDING",
         reason!="SUSPENDED BY TEAM",
  ) %>% 
  select(athlete_id,athlete_display_name,did_not_play,reason) %>%
  group_by(athlete_id,athlete_display_name) %>%
  summarize(games_missed_injury=sum(did_not_play,na.rm=TRUE)) %>%
  ungroup()


#total minutes, fouls, games started, games played
player_totals <- boxscore_raw_data %>%
  group_by(athlete_id, athlete_display_name) %>% 
  summarize(
    total_minutes = sum(minutes, na.rm = TRUE),
    total_fouls = sum(fouls,na.rm = TRUE),
    total_games_started = sum(starter,na.rm = TRUE),
    games_played  = sum(did_not_play==FALSE,na.rm=TRUE)
  ) %>%
  ungroup()

#Clean tables
year_played_clean <- year_played_data %>%
  rename(athlete_display_name = DISPLAY_FIRST_LAST) %>%
  mutate(athlete_display_name = str_trim(athlete_display_name)) %>%
  mutate(
    athlete_display_name = stri_trans_general(athlete_display_name,"Latin-ASCII")
    )

player_totals_clean <- player_totals %>%
  mutate(
    athlete_display_name = stri_trans_general(athlete_display_name,
                                              "Latin-ASCII")
    ) %>%
  mutate(athlete_display_name = str_trim(athlete_display_name))

dnp_why_clean <- dnp_why %>%
  mutate(
    athlete_display_name = stri_trans_general(athlete_display_name,
                                              "Latin-ASCII")
    ) %>%
  mutate(athlete_display_name = str_trim(athlete_display_name))

#Join the tables
modeling_data <- player_totals_clean %>%
  left_join(year_played_clean, by = "athlete_display_name") %>%
  left_join(
    dnp_why_clean %>% select(athlete_id, games_missed_injury), 
    by = "athlete_id"
  ) %>%
  
  #Clean up
  mutate(
    games_missed_injury = replace_na(games_missed_injury, 0),
    years_played = replace_na(years_played, 0)
  ) %>%
  select(athlete_display_name,
         total_minutes,
         total_fouls,
         total_games_started,
         games_played,
         years_played,
         games_missed_injury
  )
