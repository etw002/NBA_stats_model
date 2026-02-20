#load packages
library("hoopR")
library("tidyverse")
library("stringi")
library("MASS")

simulate_season <- function(modelfreq, modelsev, player_stats){
  
  prob <- predict(model_freq, newdata = player_data, type = "response")
  mu_val <- predict(model_sev, newdata = player_data, type = "response")
  theta_val <- model_sev$theta
  
  games_played <- rbinom(82, 1, prob)
  scores <- sapply(games_played, function(x) {
    if(x == 1) rnegbin(1, mu = mu_val, theta = theta_val) else 0
  })
  
  return(sum(scores))
}