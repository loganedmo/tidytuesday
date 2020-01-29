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
library(RColorBrewer)

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

# lat/lon boundary for tree locations
sf_tree_bb <- c(left = -122.53,
                right = -122.35,
                bottom = 37.70,
                top = 37.82)

# Stamen Map raster object
sf_map_stamen <- 
  get_stamenmap(bbox = sf_tree_bb,
                zoom = 13,
                maptype = "terrain",
                color = "bw",
                force = TRUE)

ggmap(sf_map_stamen, extent = "device") +
  stat_density_2d(data = sf_trees, 
                  aes(x = longitude, y = latitude,
                      fill = ..level..),
                  geom = "polygon",
                  alpha = 0.1) +
  scale_fill_gradientn(colors = brewer.pal(7, "BuGn")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", 
                                       color = NA),
        panel.background = element_rect(fill = "#f5f5f2",                                             color = NA), 
        legend.background = element_rect(fill = "#f5f5f2",                                             color = NA),
        panel.border = element_blank())

ggmap(sf_map_stamen) +
  stat_summary2d()

#############################################
# deprecated code
# Google Maps API offers less flexibility
# than Stamen Maps
#############################################

# Google Maps raster object
sf_map_google <- 
  get_googlemap(center = c(lon = -122.434, lat = 37.761),
                zoom = 12, scale = 2, 
                maptype = "terrain")
