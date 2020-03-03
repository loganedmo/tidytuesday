#####################################################################
# TIDY TUESDAY MARCH 3RD 2020: NHL GOALS
#####################################################################

library(dplyr)
library(ggplot2)
library(readr)

#############################################
# import data
#############################################

game_goals <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

#############################################
# best seasons for each top player
# highest point scoring season by player
# e.g. Wayne Gretzky will only have one record
# despite having 9 of the top 20 seasons overall
#
# top_* functions in dplyr guide:
# https://luisdva.github.io/rstats/Top-and-bottom-values-for-groups/
#
# TODO:
# break tie when player has multiple seasons
# with same number of points, goals, etc.
#############################################

player_best_season <- 
game_goals %>% 
  group_by(player, season) %>% 
  summarise(season_points  = sum(points)) %>% 
  top_n(1, season_points)

game_level_best_seasons <- 
  player_best_season %>% 
  left_join(game_goals, by = c("player", "season"))
