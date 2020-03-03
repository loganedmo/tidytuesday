#####################################################################
# TIDY TUESDAY Feb 4th 2020: NFL Stadium Attendence
#
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-04/readme.md
#####################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)

#############################################
# read data
#############################################

attendance <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

standings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

#############################################
# join tables
#############################################
wkly_attendance <- 
attendance %>% 
  filter(!is.na(weekly_attendance)) %>% 
  mutate(full_name = paste(team, team_name)) %>% 
  select(full_name, year, week, weekly_attendance)

sb_winners <- 
standings %>% 
  mutate(full_name = paste(team, team_name)) %>% 
  select(full_name, year, sb_winner)

home_games <-   
games %>% 
  filter(str_detect(week, "[0-9]") == TRUE) %>% # remove playoff games
  mutate(full_name = home_team,
         week = as.integer(week)) %>%
  select(full_name, year, week) 

home_attendance <- 
wkly_attendance %>% 
  inner_join(sb_winners, by = c("year", "full_name")) %>% 
  inner_join(home_games, by = c("year", "full_name", "week")) %>% 
  mutate(sb_winner = as.factor(sb_winner)) %>% 
  group_by(full_name, year) %>% 
  mutate(home_game = row_number()) %>% 
  ungroup()

sb_team_attendance <-
  home_attendance %>% 
  filter(sb_winner == "Won Superbowl")
  

#############################################
# plot sb winner home attendances
#
# Highlight SB Winner Lines:
# https://www.littlemissdata.com/blog/highlight
#############################################

# team colors: https://encycolorpedia.com/teams/american-football/nfl
sb_colors <- tibble(
  full_name = c("Baltimore Ravens", "Denver Broncos", 
                "Green Bay Packers", "Indianapolis Colts",
                "Kansas City Chiefs", "New England Patriots",
                "New Orleans Saints", "New York Giants",
                "Philadelphia Eagles", "Pittsburgh Steelers",
                "Seattle Seahawks", "Tampa Bay Buccaneers"),
  team_color = c("#241773", "#fb4f14", "#203731",
                 "#002c5f", "#e31837", "#002244",
                 "#d3bc8d", "#0b2265", "#004c54",
                 "#ffb612", "#002244", "#d50a0a")
)

# team logos
img_ravens <- png::readPNG("20200204NFLStadiumAttendance/img/baltimore-ravens.png")
ravens <- grid::rasterGrob(img_ravens, interpolate = T)

# faceted line
ggplot() +
  geom_line(data = home_attendance,
            aes(x = home_game, y = weekly_attendance, 
                group = full_name), 
                color = alpha("#d9d9d9", 0.9)) +
  geom_line(data = sb_team_attendance,
            aes(x = home_game, y = weekly_attendance, 
                group = full_name, color = full_name)) + 
  scale_y_continuous(limits = c(0, 100000), 
                     breaks = seq(0, 100000, 25000)) +
  annotation_custom(ravens, xmin = 1, 
                    xmax = 5, ymin = 30000, ymax = 100000) +
  facet_wrap(~year, nrow = 5, ncol = 4) +
  scale_color_manual(values = sb_colors$team_color) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "transparent"), 
        panel.grid.minor = element_line(colour = "transparent"),
        #plot.background = element_rect(fill = "#737373"),
        #panel.background = element_rect(fill = "#737373"),
        legend.position = "none")
