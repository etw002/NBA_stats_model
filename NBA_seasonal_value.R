#load packages
library("hoopR")
library("tidyverse")

# 1. Get player data for 2023
raw_data <- load_nba_player_box(seasons = 2023)

# 2. Create a simple 'Total Points' table
player_totals <- raw_data
  group_by(athlete_display_name)
  summarize(
    total_pts = sum(athlete_points, na.rm = TRUE),
    avg_min = mean(athlete_minutes, na.rm = TRUE),
    games_played = n()
  )
  filter(games_played > 10) # Actuarial 'credibility' threshold

head(player_totals)
