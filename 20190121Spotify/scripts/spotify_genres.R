##########################################################################
# TIDY TUESDAY JAN 21ST 2020: SPOTIFY GENRES
#
# Original Article:
# https://www.kaylinpavlik.com/classifying-songs-genres/
#
##########################################################################

library(dplyr)
library(forcats)
library(fmsb) # radar charts
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

# read data
spotify_songs <- 
read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs <- 
spotify_songs %>% 
  mutate(track_album_release_date = as_date(track_album_release_date),
         playlist_genre = fct_infreq(playlist_genre))

# when were albums releaased?
spotify_songs %>% 
  ggplot(aes(x = track_album_release_date)) +
    geom_histogram(color = "black", binwidth = 365.25) +
    scale_x_date(date_breaks = "5 years",
                 date_minor_breaks = "5 years",
                 date_labels = "%Y",
                 limits = c(as_date("1959-01-01"), as_date("2021-01-01"))) +
    facet_grid(playlist_genre ~ .)

# histograms for all audio features
spotify_songs %>% 
  select(danceability:duration_ms) %>% 
  gather(key = audio_feature) %>% 
  ggplot(aes(x = value)) +
    geom_histogram(color = "black") +
    facet_wrap(~ audio_feature, scales = "free") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()) +
    labs(x = "Audio Feature Value")

# radar plot data prep
spotify_songs %>% 
  select(playlist_genre, playlist_subgenre,
         speechiness, tempo, danceability,
         energy, duration_ms, valence) %>% 
  mutate_if(is.numeric, scale)
  group_by(playlist_genre) %>% 
  summarise_if(is.numeric, median)



