# Cleaning script

# Load libraries
library(tidyverse)
library(here)
library(lubridate)

# Read in data

flights <- read_csv(here("raw_data/flights.csv"))
airports <- read_csv(here("raw_data/airports.csv"))
airlines <- read_csv(here("raw_data/airlines.csv"))
weather <- read_csv(here("raw_data/weather.csv"))
planes <- read_csv(here("raw_data/planes.csv"))

# 1. Flight data
# flights without departure time, and arrival time - can't conclude anything from these, remove them to separate file first

double_nas <- flights %>% 
  filter(is.na(dep_time),
         is.na(arr_time))

flights_no_time_nas <- flights %>% 
  drop_na(dep_time) %>% 
  drop_na(arr_time)

flights_no_time_nas %>% 
  summary

# make a date column by combining year, month and day, same for time

flight_date <- flights_no_time_nas %>% 
  mutate(date = make_datetime(year, month, day), 
         time = make_datetime(year, month, day, hour, minute), .after = year) %>% 
  select(., -c(year, month, day, time_hour, hour, minute))

flight_time <- flight_date %>% 
  mutate(time = str_remove(time, "^2017-[0-9]{2}-[0-9]{2} "))

# create new column for dep delay labels

flight_time <- flight_time %>% 
  mutate(delay_dep = case_when(dep_delay < 0 ~ "early",
                               dep_delay == 0 ~ "on_time",
                               dep_delay < 10 ~ "<10 minutes delay",
                               dep_delay < 30 ~ "10-30 minutes delay",
                               dep_delay < 60 ~ "30-60 minutes delay",
                               dep_delay <120 ~ "1-2 hour delay",
                               dep_delay > 120 ~ "more than 2 hour delay",
                               TRUE ~ "NA"), .after = dep_delay)

## create new column for arrival delay labels

flight_time <- flight_time %>% 
  mutate(delay_arr = case_when(arr_delay < 0 ~ "early",
                               arr_delay == 0 ~ "on_time",
                               arr_delay < 10 ~ "<10 minutes delay",
                               arr_delay < 30 ~ "10-30 minutes delay",
                               arr_delay < 60 ~ "30-60 minutes delay",
                               arr_delay <120 ~ "1-2 hour delay",
                               arr_delay > 120 ~ "more than 2 hour delay",
                               TRUE ~ "NA"), .after = arr_delay)

# Create a delay difference column, to calculate difference in time between delayed departure and arrival

flight_clean <- flight_time %>% 
  mutate(delay_diff = dep_delay - arr_delay, .after = delay_arr)

# write clean flight data to folder
write_csv(flight_clean, "clean_data/flight_clean.csv", append = FALSE)






