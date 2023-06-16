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
         time = make_datetime(year, month, day, hour, minute), .after = year) 

flight_time <- flight_date %>% 
  mutate(time = str_remove(time, "^2017-[0-9]{2}-[0-9]{2} "))

# create new column for dep delay labels

flight_time <- flight_time %>% 
  mutate(delay_dep = case_when(dep_delay <= 0 ~ "Early or on-time",
                               dep_delay <= 15 ~ "< 15 minutes delay",
                               dep_delay <= 60 ~ "< 1hr delay",
                               dep_delay > 60 ~ " > 1hr delay",
                               TRUE ~ "NA"), .after = dep_delay)

## create new column for arrival delay labels

flight_time <- flight_time %>% 
  mutate(delay_arr = case_when(arr_delay <= 0 ~ "Early or on-time",
                                arr_delay <= 15 ~ "< 15 minutes delay",
                                arr_delay <= 60 ~ "< 1hr delay",
                                arr_delay > 60 ~ "> 1hr delay",
                                TRUE ~ "NA"), .after = arr_delay)


# Create a delay difference column, to calculate difference in time between delayed departure and arrival

flight_clean <- flight_time %>% 
  mutate(delay_diff = dep_delay - arr_delay, .after = delay_arr)

flight_clean <- flight_clean %>% 
  mutate(delayed = if_else(dep_delay > 0, "TRUE", "FALSE"),
         delayed_1hr_more = if_else(dep_delay > 60, "TRUE", "FALSE"), .after = dep_delay)

# write clean flight data to folder
write_csv(flight_clean, "clean_data/flight_clean.csv", append = FALSE)

# filter out just the Newark airport flights
newark_flights <- flight_clean %>% 
  filter(origin == "EWR")


# weather data
summary(weather)

# clean up where there are NAs in weather dataset for wind_speed, as this is most likely the variable to impact flights the most

weather_clean <- weather %>% 
  filter(!is.na(wind_speed))

# remove unneccesary columns
weather_clean <- weather_clean %>% 
  select(., -c(year, month, day, hour))

# filter out just the Newark weather
newark_weather <- weather_clean %>% 
  filter(origin == "EWR")

summary(newark_weather)


# merge newark flights and newark weather by the time_hour
newark_flights_weather <- newark_flights %>% 
  left_join(newark_weather, by = c("time_hour", "origin"))

# write data to clean data folder
write_csv(newark_flights_weather, "clean_data/newark_flights_weather.csv", append = FALSE)

# merge  flights and  weather by the time_hour
flights_weather <- flight_clean %>% 
  left_join(weather_clean, by = c("time_hour", "origin"))

flights_weather <- flights_weather %>% 
  mutate(wind_class = case_when(wind_speed <= 24 ~"Breeze",
                                wind_speed <= 31 ~"Strong breeze",
                                wind_speed <= 38 ~ "Near gale",
                                wind_speed > 38 ~ "Gale"),
         .after = wind_speed)

# write data to clean data folder
write_csv(flights_weather, "clean_data/flights_weather.csv", append = FALSE)

# airplanes data
summary(planes)

#join plane data to the flights weather data
flights_weather_planes <- flights_weather %>% 
  left_join(planes, by = "tailnum")

# write data to clean data folder
write_csv(flights_weather_planes, "clean_data/flights_weather_planes.csv", append = FALSE)

flights_weather_planes %>% 
  summary


# airlines data
flights_weather_planes_airlines <- flights_weather_planes %>% 
  left_join(airlines, by = "carrier")

# write data to clean folder
write_csv(flights_weather_planes_airlines, "clean_data/flights_weather_planes_airlines.csv", append = FALSE)

# airports data
# join airpots data to flights_weather_planes_airlines by dest == faa

# several airports listed as NA because they are internation
airport %>% 
  mutate()

all_joined <- flights_weather_planes_airlines %>% 
  left_join(airports, by = c("dest" = "faa"))

# several airports listed as NA because they are internation
all_joined <- all_joined %>% 
  mutate(name.y = case_when(dest == "SJU" ~ "Luis Manoz Marin International Airport, Puerto Rico",
                            dest == "BQN" ~ "Rafael Hernández International Airport, Puerto Rico",
                            dest == "PSE" ~ "Mercedita International Airport, Puerto Rico",
                            dest == "STT" ~ "Cyril E King Airport, Virgin Islands",
                            TRUE ~ name.y))

write.csv(all_joined, "clean_data/all_joined.csv", append = FALSE)
