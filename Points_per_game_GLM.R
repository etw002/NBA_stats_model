#load packages
library("hoopR")
library("tidyverse")
library("stringi")

df <- readRDS("Stats_per_game_model_data.rds")


# replace no ft games with league average
df_glm <- df %>%
  mutate(ft = replace_na(ft, mean(ft, na.rm = TRUE)))

#Binomial GLM
availability_model <- glm(
  cbind(games_played, games_missed) ~ total_minutes + total_fouls + 
    total_games_started + years_played + games_missed_injury,
  family = binomial(link = "logit"),
  data = df_glm
)

summary(availability_model)