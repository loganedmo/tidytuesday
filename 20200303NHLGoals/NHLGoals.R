#####################################################################
# TIDY TUESDAY MARCH 3RD 2020: NHL GOALS
#
# NB: NHL season length has varied greatly across time
# this data includes seasons from 1980 on
# in that time frame (full) seasons of 80, 82, 84 games have occured
# along with lockout shortened seasons of 0 and 48 games
# this analysis does not take into account the duration of a
# given player's season
#
# Some players did not have data for full season's worth of data
# (are these due to games missed from injury?)
# 
# http://www.puckreport.com/2009/07/nhl-regular-season-length.html
#
# TODO:
# Birthday Shots! analysis?
#####################################################################

library(dplyr)
library(forcats)
library(ggplot2)
library(readr)

#############################################
# import data
#############################################

game_goals <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

#############################################
# best season for each top player
# highest point scoring season
# e.g. Wayne Gretzky will only have one record
# despite having 9 of the top 20 seasons overall
#
# top_* functions in dplyr guide:
# https://luisdva.github.io/rstats/Top-and-bottom-values-for-groups/
#############################################

# top_n() method does not break ties
# e.g. Dave Andreychuk has 2 top seasons 
# with exactly 99 points
player_best_season <- 
game_goals %>% 
  group_by(player, season) %>% 
  summarise(season_points  = sum(points)) %>% 
  # only take highest point season for each player
  top_n(1, season_points) %>% 
  # take first season (min) in case of tie
  summarise(season = min(season),
            season_points = min(season_points)) %>% 
  # ranking best seasons based on total points
  mutate(season_rank = row_number(desc(season_points)),
         top_5_season = if_else(season_rank <= 5, 1, 0),
         ovechkin_season = if_else(player == "Alex Ovechkin", 1, 0))

game_level_best_seasons <- 
  player_best_season %>% 
  filter(season_rank <= 20) %>% 
  left_join(game_goals, by = c("player", "season")) %>% 
  group_by(player) %>% 
  mutate(running_points = cumsum(points)) %>% 
  ungroup() %>% 
  mutate(player = fct_rev(fct_reorder(player, season_points)))

line_ends <- 
  game_level_best_seasons %>% 
    group_by(player) %>% 
    filter(game_num == max(game_num))

#############################################
# plot point progress throughout
# career best seasons
#
# line labels at end
# https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines
# see DatamineR's answer
#
# set line colors manually:
# https://community.rstudio.com/t/ggplot-set-colors-separately-for-geom-point-and-geom-line-manually/13901/2
#
# left align caption:
# https://stackoverflow.com/questions/41105759/aligning-title-subtitle-and-caption-for-horizontal-ggplot-barchart
#############################################

# NHL team hex code colors:
# https://encycolorpedia.com/teams/ice-hockey/nhl
player_colors <- c("#041e41", # Gretzky EDM
                   "#fcb514", # Lemieux PIT
                   "#ce1126", # Yzerman DET
                   "#fcb514", # Jagr PIT
                   "#041e41", # Kurri EDM
                   rep("grey70", 7), # others
                   "#cf0a2c", # Ovechkin WAS
                   rep("grey70", 7)) # others

ovechkin_plot <- 
game_level_best_seasons %>% 
  filter(season_rank <= 20) %>% 
  ggplot(aes(x = game_num, y = running_points, 
             group = player)) +
    geom_line(aes(color = player),
              size = 0.5) +
    geom_text(data = line_ends %>% 
                        filter(season_rank <= 5 |
                               player == "Alex Ovechkin"), 
              aes(x = game_num + 4.5, 
                  y = running_points, 
                  label = paste(player, team, season),
                  fontface = 2),
              #color = "white", 
              size = 2.5) +
    scale_color_manual(values = player_colors) +
    scale_x_continuous(breaks = seq(0, 80, 10), limits = c(0, 90)) +
    theme(panel.grid.major = element_line(size = 0.3, 
                                          colour = 'grey55'),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "grey50"),
          legend.position = "none",
          plot.title = element_text(size = 17),
          plot.subtitle = element_text(size = 17),
          plot.caption = element_text(hjust = 0, 
                                      size = 8,
                                      face = "italic"),
          axis.ticks = element_blank()) +
    labs(x = "\nGame\n",
         y = "\nTotal Season Points\n(Goals + Assists)\n",
         title = "\nAlex Ovechkin Recently Joined the Elite 700 Goal Club",
         subtitle = "But His Best Season Lags Behind Other Hall of Famers*\n",
         caption = "* Gretzky has 12 of the top 20 point seasons\n\nData |NHL Reference\nAuthor | Logan Edmonds\n@logedmonds")

ggsave(filename = '20200303NHLGoals/plots/ovechkin_plot.png', 
       dpi = 500,
       width = 10, 
       height = 5.8)
