#####################################################################
# TIDY TUESDAY FEB 25TH 2020: MEASLES OUTBREAK
#
#
#####################################################################

library(dplyr)
library(forcats)
library(ggplot2)
library(maps)
library(readr)
library(sf)
library(stringr)
library(tmap)
library(viridis)

# convert first character to upper case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#############################################
# import data
#############################################

# "Le Flore" county is misspelled
ok_measles <- 
  read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/state-overviews.csv") %>% 
  filter(state == "Oklahoma") %>% 
  rename(county  = `county/district`) %>% 
  mutate(overall = as.numeric(overall),
         county  = recode(county, "Le" = "Le Flore"))

#############################################
# shapefile data for OK counties
#############################################

# sf package OK shapefile
ok_counties <- 
  st_as_sf(map("county", 
               plot = FALSE, 
               fill = TRUE)) %>% 
  filter(str_detect(ID, "oklahoma")) %>% 
  # convert to uppercase for join
  mutate(state = firstup(str_extract(ID, "oklahoma")),
         county = firstup(str_extract(ID, "[^,]*$")))

# firstup() function did not correctly parse
# counites with multiple uppercase chars (e.g. "McClain")
# or counties with spaces ("Le Flore")
correct_names <- 
  c("Le Flore", "McClain", "McCurtain",
    "McIntosh", "Roger Mills")

# find mismatch indices
county_miscodes <- 
  which(!ok_counties$county %in% ok_measles$county)

# replace mismatches with correct names
ok_counties$county[county_miscodes] <- correct_names

#############################################
# join measles county level data with county sf
#############################################

ok_all <- 
  ok_counties %>% 
    left_join(ok_measles, by = "county")

#############################################
# mapping counties
# Zev Ross tmap guide:
# http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/#part-2-creating-beautiful-maps-with-tmap
#
# adding viridis fill"
# https://johnmackintosh.com/2017-09-01-easy-maps-with-tmap/
#############################################
viridis_cols <- inferno(7)

cuts <- c(50, 85, 88, 91, 94, 97, 99.9, 100)
cut_labels <- 
  c("<85%", "85 - 88%", "88 - 91%", "91 - 94%", 
    "94 - 97%", "97 - 99.9%", "100%")

ok_measles_map <- 
tm_shape(ok_all) +
  tm_polygons("overall", 
              breaks = cuts,
              palette = viridis_cols,
              border.alpha = 0.5,
              border.col = "white", 
              legend.show = FALSE) +
  tm_text("county", 
          size = 0.5,
          col = "white") +
  tm_layout(title = "\nOklahoma Measles, Mumps, Rubella (MMR) School Vaccinations", 
            asp = 0, 
            main.title.fontface = "bold",
            title.size = 1.3,
            title.position = c(0.034, 0.93),
            title.color = "white",
            legend.position = c(0.1, 0.1), 
            inner.margins = c(0.0, 0.03, 0.0, 0.03), 
            outer.margins = c(0.1, 0, 0.1, 0),
            bg.color = "black") +
  tm_legend(legend.title.color = "white",
            legend.text.color = "white",
            legend.position = c(0.2, 0.42)) +
  tm_add_legend(type = "fill", 
                border.col = "white", 
                border.alpha = 0.1, 
                labels = cut_labels,
                title = "2017 Rates",
                col = viridis_cols) +
  tm_credits("Data | Wall Street Journal\nAuthor | Logan Edmonds\n@logedmonds", 
             col = "white", 
             position = c(0.035, 0.04))

tmap_save(ok_measles_map,
          '20200225Measles/plots/ok_measles.png', 
          width = 20, height = 18, units = "cm")

