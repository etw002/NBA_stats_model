#load packages
library("hoopR")
library("tidyverse")
library("stringi")

SEASON = 2023

#Get player data
boxscore_raw_data <- load_nba_player_box(seasons=SEASON) %>%
  filter(season_type==2)
# calculate team stats for usage rate
team_game_stats <- boxscore_raw_data %>%
  filter(did_not_play == FALSE) %>%
  group_by(game_id, team_id, team_name) %>%
  summarize(
    team_FGA = sum(points, na.rm = TRUE),
    team_FTA = sum(rebounds, na.rm = TRUE),
    team_TO = sum(turnovers, na.rm = TRUE),
    team_minutes = sum(minutes,na.rm=TRUE)
  ) %>%
  ungroup()

#points, fg%, ft%, starter/bench, position
stats_ds <- boxscore_raw_data %>% 
  filter(minutes > 10 ) %>%
  mutate(fg = field_goals_made/field_goals_attempted,
         ft = free_throws_made/free_throws_attempted
  ) %>% 
  select(game_id,athlete_display_name,points,fg,ft,starter,athlete_position_abbreviation)

#usage rate
usage_data <- boxscore_raw_data %>%
  left_join(
    team_game_stats, 
    by = c("game_id", "team_id"),
    suffix = c("_player", "_team")
  ) %>% 
  filter(minutes > 10 ) %>%
  mutate(
    player_poss = field_goals_attempted + (0.44 * free_throws_attempted) + turnovers,
    team_poss = team_FGA + (0.44 * team_FTA) + team_TO,
    
    # Usage Rate Calculation
    # We multiply by (team_minutes / 5) because there are 5 players on the court
    usage_rate = 100 * (player_poss/team_poss) * (team_minutes/(minutes*5))
  ) %>%
  select(game_id,athlete_display_name,usage_rate,player_poss,team_poss,minutes,team_minutes)

ppg_modeling_data <- stats_ds %>%
  left_join(usage_data, by = c("game_id","athlete_display_name")) %>%
  select(game_id,athlete_display_name, points, usage_rate, fg, ft, starter, 
         athlete_position_abbreviation)
saveRDS(ppg_modeling_data, "Points_per_game_model_data.rds")