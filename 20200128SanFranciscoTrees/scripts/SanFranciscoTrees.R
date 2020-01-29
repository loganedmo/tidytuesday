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
# ~ 2,000 rows with missing lat/lon
#############################################

sf_trees <- 
sf_trees %>% 
  filter(date >= '1972-01-01' & # sparse data prior to 1972
         !is.na(latitude) & !is.na(longitude)) %>% 
  mutate(date = if_else(is.na(date), 
                as_date('1955-01-01'), date))

#############################################
# static mapping of SF
# use Google's Static Map API service
# SF lat/lon - 37.7749° N, 122.4194° W
#############################################

ggmap::register_google(key = "AIzaSyA3dDU9SYrydFwnDA8wKiLwSqP0AnivDG0")

# raster object
sf_map <- 
get_googlemap(center = c(lon = -122.4194, 37.7749),
              zoom = 12, scale = 2, maptype = "terrain")

ggmap(sf_map) +
  geom_point(data = sf_trees, 
             aes(x = longitude, y = latitude, color = date),
             size = 0.1, alpha = 0.5) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

