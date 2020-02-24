#####################################################################
# TIDY TUESDAY
# FEB. 23RD 2020: FOOD CONSUMPTION + CO2 EMISSIONS
#
# data cleaning blog:
# https://r-tastic.co.uk/post/from-messy-to-tidy/
#
#####################################################################

library(countrycode) # match country -- continent
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)

#############################################
# read data
# NB: data is already clean
#############################################

food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

food_consumption %>% 
  mutate(continent = countrycode(country, "country.name", "continent"),
         region = countrycode(country, "country.name", "region"),
         country = as_factor(country),
         food_category = as_factor(food_category),
         continent = as_factor(continent),
         region = as_factor(region))

