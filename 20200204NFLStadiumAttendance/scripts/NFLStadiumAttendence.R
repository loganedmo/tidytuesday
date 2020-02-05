#####################################################################
# TIDY TUESDAY Feb 4th 2020: NFL Stadium Attendence
#
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-04/readme.md
#####################################################################

library(readr)

#############################################
# read data
#############################################

attendance <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

standings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')
