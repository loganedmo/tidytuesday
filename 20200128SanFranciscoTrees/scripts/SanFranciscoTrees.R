#####################################################################
# TIDY TUESDAY JAN. 28TH 2020: SAN FRANCISCO TREES
#
#
#
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-28/readme.md
#####################################################################

#############################################
# load packages
#############################################

library(dplyr)
library(ggmap)
library(ggplot2)
library(lubridate)
library(readr)

#############################################
# import data
#############################################

sf_trees <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

#############################################
# plantings by year
# NA dates are pre 1955 trees
# consider removing pre-1972: sparse data
#############################################

sf_trees %>% 
  mutate(date = if_else(is.na(date), 
                as_date('1955-01-01'), date))

#############################################
# static mapping of SF
# use Google's Static Map API service
#############################################

ggmap::register_google(key = "AIzaSyA3dDU9SYrydFwnDA8wKiLwSqP0AnivDG0")

ggmap(get_googlemap

