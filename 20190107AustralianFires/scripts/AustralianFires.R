
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(viridis)

# read data in 
rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')

temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# find missing temperatures
temperature %>% 
  mutate(year = year(date),
         missing_temperature = if_else(is.na(temperature), 1, 0)) %>%    group_by(year) %>% 
  summarise(missing_temperatures = sum(missing_temperature)) %>% 
  arrange(missing_temperatures) %>% 
  View()


# plotting

test <- 
temperature %>% 
  filter(city_name == 'PERTH') %>% 
  mutate(previous_temp = lag(temperature),
         value_change_temp = temperature - previous_temp,
         percent_change_temp = (value_change_temp/previous_temp) * 100,
         temp_type = factor(temp_type))

test %>% 
  filter(date >= '2019-05-01') %>% 
  ggplot(aes(x = date, y = percent_change_temp, 
             fill = percent_change_temp)) + 
    geom_bar(stat = "identity", color = "black") +
    scale_fill_viridis(option = "inferno") +
    facet_grid(temp_type ~ .) +
    #scale_x_date(limits = c(date('1910-01-01'), date('1910-04-10'))) +
    coord_flip()
