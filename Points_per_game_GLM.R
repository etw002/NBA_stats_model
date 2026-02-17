#load packages
library("hoopR")
library("tidyverse")
library("stringi")
library("MASS")


df <- readRDS("Points_per_game_model_data.rds")


# replace no ft games with league average
df_glm <- df %>%
  mutate(ft = replace_na(ft, mean(ft, na.rm = TRUE)))

points_model <- glm.nb(
  points ~ usage_rate + fg + ft + athlete_position_abbreviation
  + starter,
  data = df_glm
)

# 3. Check the results
summary(points_model)