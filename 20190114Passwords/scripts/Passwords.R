###############################################################################
# TIDY TUESDAY JANUARY 14, 2020: PASSWORDS
#
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-14/readme.md
# 
# original viz:
# https://informationisbeautiful.net/visualizations/top-500-passwords-visualized/?utm_content=buffer994fa&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
###############################################################################

library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)

# time constants
secs_in_hour <- 60 * 60
secs_in_day <- 24 * secs_in_hour
secs_in_week <- 7 * secs_in_day
secs_in_month <- (365.25/12) * secs_in_day
secs_in_year <- 365.25 * secs_in_day

passwords <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# data cleaning
passwords <- 
passwords %>% 
  select(-rank_alt) %>% 
  filter(complete.cases(.)) %>% 
  mutate(online_crack_sec = 
         case_when(time_unit == "years" ~ value * secs_in_year,
                   time_unit == "months" ~ value * secs_in_month,
                   time_unit == "weeks" ~ value * secs_in_week,
                   time_unit == "days" ~ value * secs_in_day,
                   time_unit == "hours" ~ value * secs_in_hour,
                   time_unit == "minutes" ~ value * 60,
                   TRUE ~ value),
         online_crack_group =
         case_when(online_crack_sec <= 60 ~ "< minute",
                   online_crack_sec > 60 & 
                   online_crack_sec < secs_in_hour~ "< hour",                          online_crack_sec > secs_in_hour &
                   online_crack_sec <= secs_in_day ~ "< day",
                   online_crack_sec > secs_in_day &
                   online_crack_sec <= secs_in_week ~ "< week",
                   online_crack_sec > secs_in_week &
                   online_crack_sec <= secs_in_year ~ "< year",
                   online_crack_sec > secs_in_year ~ "year+"),
         online_crack_group = fct_reorder(online_crack_group, online_crack_sec),
         category =  fct_reorder(category, online_crack_sec))

crack_groups <- 
passwords %>% 
  group_by(category, online_crack_group) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

# time to crack by category
crack_groups %>% 
  ggplot(aes(x = online_crack_group, y = category)) +
  geom_tile(aes(fill = percent), color = "black") +
  theme_minimal() +
  theme()
            

category_counts <- 
passwords %>% 
  group_by(category) %>% 
  summarise(count = n(),
            avg_strength = mean(strength),
            avg_crack_time = mean(online_crack_sec),
            median_crack_time = median(online_crack_sec))

# counts by category
category_counts %>% 
  ggplot(aes(x = category, y = count)) +
    geom_bar(stat = "identity", color = "black") +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank())

# time to crack distributions
# category types
passwords %>% 
  ggplot(aes(x = online_crack_sec)) +
    geom_histogram() +
    scale_x_log10() +
    facet_grid(category ~ .)
    
# time to crack by stregnth scale
passwords %>% 
  filter(strength <= 10) %>% 
  ggplot(aes(x = strength, y = offline_crack_sec)) +
  geom_point() +
  scale_y_log10()

passwords %>% 
  ggplot(aes(x = str_length(password), y = online_crack_sec,
             color = category)) +
    geom_jitter() + 
    scale_y_log10()
