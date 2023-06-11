# halloween candy project

library(tidyverse)
library(janitor)
library(here)
library(stringr)
library(readxl)
library(tidyr)

candy_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")

# First join candy_2015 to candy_2016
## step 1 - clean up the column headings
names(candy_2015)
candy_2015_clean <- clean_names(candy_2015)
candy_2016_clean <- clean_names(candy_2016)

dim(candy_2015_clean)
dim(candy_2016_clean)

## step 2 - rename some of the column headings to be the same, to avoid extra columns
candy_2016_clean <- candy_2016_clean %>% 
  rename(licorice = licorice_yes_black,
         bonkers = bonkers_the_candy,
         box_o_raisins = boxo_raisins)

## rename some of the column headings to be the same, to avoid extra columns
candy_2016_clean <- candy_2016_clean %>% 
  rename(dark_chocolate_hershey = hersheys_dark_chocolate)


## 1st Join - add rows from 2016 to the data from 2015
candy_2015_2016 <- bind_rows(candy_2015_clean, candy_2016_clean)
dim(candy_2015_2016)

# Second join between candy_2015_2016 and candy_2017 
## Step 1 - clean column headings for candy 2017 dataset
candy_2017_clean <- clean_names(candy_2017)


## Step 2 - candy 2017 column headings all contain "Qx_" prefix, which needs to be removed before binding to the other dataset. 
names(candy_2017_clean) [2:120] <- substring(names(candy_2017_clean)[2:120], 4)


## Step 3 - rename some of the column headings in candy_2015_2016 to be the same as candy_2017, to avoid extra columns
candy_2015_2016 <- candy_2015_2016 %>% 
  rename(age = how_old_are_you,
         country = which_country_do_you_live_in,
         state_province_county = which_state_province_county_do_you_live_in,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         boxo_raisins = box_o_raisins)

## get dimensions of candy_2017_clean before the join
dim(candy_2017_clean)

## step 4 - rename a few more column headings in candy_2017 to be identical to other datasets, to avoid extra columns
candy_2017_clean <- candy_2017_clean %>% 
  rename(going_trick_or_treating = going_out,
         x100_grand_bar = "100_grand_bar",
         anonymous_brown_globs_that_come_in_black_and_orange_wrappers = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
         bonkers = bonkers_the_candy,
         licorice = licorice_yes_black)


## step5 - join two datasets add rows from candy_2017_clean to the data from 2015+2016
candy_all_years <- bind_rows(candy_2015_2016, candy_2017_clean)
dim(candy_all_years)

### check  headings from all to check for duplication
names(candy_all_years)

# Clean up country column
candy_all_years %>% 
  distinct(country)

## clean up casing in country column
candy_all_years <- candy_all_years %>% 
  mutate(country = str_to_lower(country))

## check 
candy_all_years %>% 
  distinct(country)
  
## extract this pattern
pattern <- "united states of america"
candy_all_years %>% 
  str_detect(pattern)

## replace all paterns with "usa"
candy_all_years <- candy_all_years %>% 
  mutate(country = str_replace(country, "united states of america", "usa"))

## check the pattern isn't still there. 
candy_all_years %>% 
  str_detect(pattern)

## check 
candy_all_years %>% 
  distinct(country)

## extract this pattern
pattern2 <- "united states"
candy_all_years %>% 
  str_detect(pattern)

## replace all patterns with "usa"
candy_all_years <- candy_all_years %>% 
  mutate(country = str_replace(country, "united states", "usa"))

## check the pattern isn't still there. 
candy_all_years %>% 
  str_detect("united states")

## replace all patterns with "usa"
candy_all_years <- candy_all_years %>% 
  mutate(country = str_replace(country, "us", "usa"))

## replace all patterns with "usa"
candy_all_years <- candy_all_years %>% 
  mutate(country = str_replace(country, "u.s.a", "usa"))

## check 
candy_all_years %>% 
  distinct(country)

## replace all patterns with "usa"
candy_all_years <- candy_all_years %>% 
  mutate(country = str_replace(country, "usaa", "usa"),
         country = str_replace(country, "usasa", "usa"),
         country = str_replace(country, "usa.", "usa"))

### check 
candy_all_years %>% 
  distinct(country)

## count different countries
candy_all_years %>% 
  select(country) %>% 
  count(country) %>% 
  arrange(desc(n))

## clean up a few more US variations in country column 
candy_all_years <- candy_all_years %>% 
  mutate(country = str_replace(country, "u.s.", "usa"),
         country = str_replace(country, "america", "usa"))

# try to combine internal_id and timestamp
candy_all_years <- candy_all_years %>% 
  unite(id, c("timestamp", "internal_id"), sep= "")

### return the column headings so we can select just columns with candy
candy_all_years %>% 
  names()

# select just the candy columns to remove all the comments
candy_only <- candy_all_years %>% 
  select(id: going_trick_or_treating, 
         your_gender: state_province_county,
         butterfinger: york_peppermint_patties, 
         sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year, 
         necco_wafers, 
         bonkers_the_board_game:peeps,
         reeses_pieces:whatchamacallit_bars)

dim(candy_only)

# turn candy wide format into long format
candy_long <- candy_only %>% 
  pivot_longer(cols = c("butterfinger": "whatchamacallit_bars"),
               names_to = "candy_type",
               values_to = "rating")

# write the candy_long dataset to a csv file and save in the clean_data folder
write_csv(candy_long, "clean_data/candy_long.csv", append = FALSE)
write_csv(candy_all_years, "clean_data/candy_all_years.csv", append = FALSE)
