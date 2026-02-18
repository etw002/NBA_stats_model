#load packages
library("hoopR")
library("tidyverse")
library("stringi")

df <- readRDS("Frequency_ds.rds")


# Cap games at 82
df_glm <- df %>%
  mutate(
    games_played = pmin(games_played, 82),
    games_missed = 82 - games_played,
    games_missed = pmax(games_missed, 0)
  )

#Binomial GLM
availability_model <- glm(
  cbind(games_played, games_missed) ~ total_minutes + total_fouls + 
    total_games_started + years_played + games_missed_injury,
  family = binomial(link = "logit"),
  data = df_glm
)

summary(availability_model)