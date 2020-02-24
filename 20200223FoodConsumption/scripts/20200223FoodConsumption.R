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
library(waffle)

#############################################
# read data
# NB: data is already clean
#############################################

food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

food_consumption <- 
food_consumption %>% 
  mutate(continent = countrycode(country, "country.name", "continent"),
         region = countrycode(country, "country.name", "region")) %>% 
  mutate_if(is.character, as_factor)

#############################################
# waffle chart
#
# example blog:
# https://www.nsgrantham.com/food-carbon-footprint
#############################################

# example waffle plot code
ggplot() +
  geom_waffle(data = carbon_footprints, 
              aes(values = co2 / 10, fill = food),
              color = "#F8F8FF", size = .25, n_rows = 10, flip = TRUE)

meat_sources <- c("Pork", "Poultry", "Beef", "Lamb & Goat", "Fish")

meat_consumption <- 
food_consumption %>% 
  filter(food_category %in% meat_sources) %>% 
  group_by(continent, food_category) %>% 
  summarise(consumption = sum(consumption)) %>% 
  mutate(consumption_perc = consumption/sum(consumption))
