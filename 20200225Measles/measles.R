#####################################################################
# TIDY TUESDAY FEB 25TH 2020: MEASLES OUTBREAK
#
#
#####################################################################

library(dplyr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(readr)
library(sf)
library(stringr)
library(tmap)

# convert first character to upper case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#############################################
# import data
#############################################

ok_measles <- 
  read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/state-overviews.csv") %>% 
  filter(state == "Oklahoma") %>% 
  mutate(overall = as.numeric(overall)) %>% 
  rename(county = `county/district`)

#############################################
# shapefile data for OK counties
#############################################

# sf package OK shapefile
ok_counties <- 
  st_as_sf(map("county", 
               plot = FALSE, 
               fill = TRUE)) %>% 
  filter(str_detect(ID, "oklahoma")) %>% 
  mutate(state = firstup(str_extract(ID, "oklahoma")),
         county = firstup(str_extract(ID, "[^,]*$")))

ok_all <- 
  ok_counties %>% 
    left_join(ok_measles, by = "county")

tm_shape(ok_all) +
  tm_polygons("overall",
              style = "quantile") +
  tm_text("county", 
          size = 0.5,
          col = "white")
