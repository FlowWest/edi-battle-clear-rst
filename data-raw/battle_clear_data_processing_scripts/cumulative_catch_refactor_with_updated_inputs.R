# This code will calculate daily catch, cumulative catch, and percent cumulative catch
# for brood years at the lower Clear Creek rotary screw trap. The data comes from 4 queries
# Report Data CHN, Report Data WCS, Report Data LFFCS, and Report Data RBT
# Mike Schraml 02/02/2022


# This script is a refactor of Cumulative catch LCC.R
# Mike Schraml, USFWS
# 05-23-2023

# Load needed Packages
if (!require("readxl")) {            # for importing data sets
  install.packages("readxl")
  library(readxl)
}

if (!require("tidyverse")) {            # data management
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("openxlsx")) {         # for writing xlsx files to the desk top
  install.packages("openxlsx")
  library(openxlsx)
}

if (!require("padr")) {         # for for inserting missing dates
  install.packages("padr")
  library(padr)
}

if (!require("lubridate")) {         # for for dealing with dates
  install.packages("lubridate")
  library(lubridate)
}


# prep data ---------------------------------------------------------------

catch <- read.csv(here::here("data", "catch.csv")) |> 
  select(date = sample_date, brood_year, fws_run, r_catch, 
         common_name, station_code, interp) |> 
  glimpse()


# cumulative catch --------------------------------------------------------
# by <- 2019 
by <- 2021 # 2019 is original value; too restrictive for checking code

sample_data <- catch |> # TODO how to get station code?
  filter(brood_year == by, 
         interp == FALSE,
         common_name %in% c("Chinook Salmon", "Rainbow Trout")) |> 
  glimpse()

min_date <- as.Date(min(sample_data$date, na.rm = T))
max_date <- as.Date(max(sample_data$date, na.rm = T))

# insert missing dates and set new dates Rcatch to 0
sample_data_full <- sample_data |> 
  mutate(date = as.Date(date)) |> 
  pad(interval = "day",
      start_val = min_date,
      end_val = max_date) |>
  fill_by_value(r_catch, value = 0) |> glimpse()

daily_catch <- sample_data_full |>
  group_by(date, fws_run, common_name, station_code) |> 
  summarise(catch = sum(r_catch, na.rm = TRUE),
            cumulative_catch = cumsum(catch)) |> # TODO move total_catch into summarise?
  mutate(total_catch = sum(catch, na.rm = TRUE), 
         cumulative_percent_catch = round((cumulative_catch / total_catch * 100), 0),
         cumulative_percent_catch = ifelse(cumulative_percent_catch == "NaN", NA, cumulative_percent_catch)) |> 
  glimpse()

daily_catch_wide <- daily_catch |> 
  select(-c(cumulative_catch, total_catch)) |> 
  group_by(station_code, fws_run, common_name) |> 
  pivot_wider(id_cols = date,
              names_from = c(station_code, fws_run, common_name),
              values_from = cumulative_percent_catch) |> 
  glimpse()
  

# write file
write.csv(daily_catch_wide, here::here("data-raw", "cumulative_catch.csv"),
          row.names = FALSE)

