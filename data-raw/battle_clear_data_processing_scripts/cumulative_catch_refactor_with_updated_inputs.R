# This code will calculate daily catch, cumulative catch, and percent cumulative catch
# for brood years at the lower Clear Creek rotary screw trap. The data comes from 4 queries
# Report Data CHN, Report Data WCS, Report Data LFFCS, and Report Data RBT

# This script is a refactor of code by Mike Schraml, USFWS

if (!require("tidyverse")) {            # data management
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("padr")) {         # for for inserting missing dates
  install.packages("padr")
  library(padr)
}

# prep data ---------------------------------------------------------------

catch <- read.csv("data/catch.csv") |> 
  select(date = sample_date, brood_year, fws_run, r_catch, 
         common_name, station_code, interp) |> 
  glimpse()

# cumulative catch --------------------------------------------------------

# brood_year_filter <- 2021 # use this if you want to filter
sample_data <- catch |> 
  filter(interp == FALSE,
         common_name %in% c("Chinook Salmon", "Rainbow Trout")) |> 
  #filter(brood_year %in% brood_year_filter) |> 
  glimpse()

min_date <- as.Date(min(sample_data$date, na.rm = T))
max_date <- as.Date(max(sample_data$date, na.rm = T))

# insert missing dates and set new dates Rcatch to 0
sample_data_full <- sample_data |> 
  mutate(date = as.Date(date)) |> 
  pad(interval = "day",
      start_val = min_date,
      end_val = max_date) |>
  fill_by_value(r_catch, value = 0) |> 
  glimpse()

daily_catch <- sample_data_full |>
  group_by(date, brood_year, fws_run, common_name, station_code) |> 
  summarise(catch = sum(r_catch, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(brood_year, fws_run, common_name, station_code) |> 
  mutate(cumulative_catch = cumsum(catch),
         total_catch = sum(catch, na.rm = TRUE),
         cumulative_percent_catch = round((cumulative_catch / total_catch * 100), 0),
         cumulative_percent_catch = ifelse(cumulative_percent_catch == "NaN", NA, cumulative_percent_catch)) |>
  select(-catch) |> 
  arrange(common_name, fws_run, brood_year, date) |> 
  ungroup() |> 
  glimpse()

daily_catch_wide <- daily_catch |> 
  filter(!is.na(brood_year)) |> 
  select(-c(cumulative_catch, total_catch)) |> 
  group_by(brood_year, station_code, fws_run, common_name) |> 
  pivot_wider(id_cols = c(brood_year, date),
              names_from = c(station_code, fws_run, common_name),
              values_from = cumulative_percent_catch) |> 
  glimpse()


# write file --------------------------------------------------------------
# as appropriate

