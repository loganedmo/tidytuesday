#####################################################################
# TIDY TUESDAY JAN. 28TH 2020: SAN FRANCISCO TREES
#
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-28/readme.md
#
# TODO:
# tree age map - change binwidth to SF city block (~475 x 325 ft)
# need to convert lat/lon degrees to feet
#
# create map theme
#
# better legend breaks
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
library(viridis)

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

sf_trees_all <- 
sf_trees %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>% 
  mutate(date = if_else(is.na(date), 
                as_date('1955-01-01'), date),
         age = (today() - date)/365.25) # years with decimal

sf_trees_new <- 
sf_trees_all %>% 
  filter(date >= '1972-01-01') # sparse data prior to 1972

#############################################
# static mapping of SF
# SF lat/lon - 37.7749° N, 122.4194° W
#############################################

# lat/lon boundary for tree locations
sf_tree_bb <- c(left = -122.53,
                right = -122.35,
                bottom = 37.70,
                top = 37.82)

# Stamen Map raster object
sf_map_stamen <- 
  get_stamenmap(bbox = sf_tree_bb,
                zoom = 13,
                maptype = "toner-lite",
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
        plot.background = element_rect(fill = "#D9D9D9", 
                                       color = NA),
        panel.background = element_rect(fill = "#D9D9D9",                                             color = NA), 
        legend.background = element_rect(fill = "#D9D9D9",                                             color = NA),
        panel.border = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Density of Tree Plantings in San Francisco",
       caption = "#tidytuesday | Logan Edmonds @logedmonds")

#############################################
# median age of trees by geographic bins
# 
# binning 2d area based on third variable (i.e. age):
# https://stackoverflow.com/questions/18285415/density2d-plot-using-another-variable-for-the-fill-similar-to-geom-tile/
#
# continuous scale tips and legend placement:
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
#
# legend positioning:
# https://stackoverflow.com/questions/28816467/ggplot-position-legend-in-top-left/28818021
#
# caption positioning:
# https://ggplot2.tidyverse.org/articles/releases/ggplot2-2.2.0.html
#############################################

legend_breaks <- c(5, 10, 20, 30, 40, 50, 60)

legend_labels <- c("<5", "10", "20", "30", "40", "50", "60+")

age_plot_all <- 
ggmap(sf_map_stamen, extent = "device") +
  stat_summary_2d(data = sf_trees_all, 
                 aes(x = longitude, y = latitude, z = age),
                 fun = median,
                 binwidth = c(0.0025, 0.0025),
                 #color = "white",
                 alpha = 0.2) +
  #scale_fill_manual(values = viridis(8),
  #                  breaks = legend_breaks,
  #                  labels = legend_labels,
  #                  name = "Median Tree Age",
  #                  guide = guide_colorbar(
  #                    direction = "horizontal",
  #                    title.position = "top",
  #                    barheight = unit(2, units = "mm"),
  #                    barwidth = unit(50, units = "mm"))) +
  
  #horizontal legend courtesy of Timo Grossenbacher
  scale_fill_viridis(option = "viridis",
                     name = "Median Tree Age",
                     guide = guide_colorbar(
                     direction = "horizontal",
                     title.position = "top",
                     barheight = unit(2, units = "mm"),
                     barwidth = unit(50, units = "mm")
                       )) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "#D9D9D9", 
                                       color = NA),
        panel.background = element_rect(fill = "#D9D9D9",                                             color = NA), 
        legend.background = element_rect(fill = "#D9D9D9",                                             color = NA),
        panel.border = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0.0),
        legend.title = element_text(size = 8)) +
  labs(title = "\nWhere are new growth planted trees in San Francisco?",
       subtitle = "Primarily on the fringes of the city\n",
       caption = "Trees planted prior to 1955 have undocumented age\n#tidytuesday | @logedmonds")
  
#############################################
# deprecated code
#
# Google's Static Map API service
# offers less flexibility than Stamen Maps
#############################################

# ggmap::register_google(key = "")
# 
# # Google Maps raster object
# sf_map_google <- 
#   get_googlemap(center = c(lon = -122.434, lat = 37.761),
#                 zoom = 12, scale = 2, 
#                 maptype = "terrain")
