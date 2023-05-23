# This code will calculate daily catch, cumulative catch, and percent cumulative catch
# for brood years at the lower Clear Creek rotary screw trap. The data comes from 4 queries
# Report Data CHN, Report Data WCS, Report Data LFFCS, and Report Data RBT
# Mike Schraml 02/02/2022


# This script is a refactor of Cumulative catch LCC.R
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

sample <- read_csv(here::here("data", "trap.csv")) |> 
  select(date = sample_date, station_code) |> 
  distinct_all() |> glimpse()

catch <- read.csv("data/catch.csv") |> 
  select(date, brood_year, fws_run, r_catch, common_name, interp) |> 
  mutate(date = as.Date(date)) |> 
  glimpse()



# cumulative catch --------------------------------------------------------
by <- 2019

sample_data <- catch |> # TODO how to get station code?
  filter(#brood_year == by, 
         interp == FALSE,
         common_name %in% c("Chinook Salmon", "Rainbow Trout")) |> 
  glimpse()

min_date <- min(sample_data$date)
max_date <- max(sample_data$date)

# insert missing dates and set new dates Rcatch to 0
sample_data_full <- sample_data |> 
  pad(interval = "day",
      start_val = min_date,
      end_val = max_date) |> 
      # start_val = lubridate::ymd("2019-01-01"),
      # end_val = lubridate::ymd("2020-06-30")) |> 
  fill_by_value(r_catch, value = 0) |> glimpse()

daily_catch <- sample_data_full |>
  select(-c(brood_year, interp)) |> 
  group_by(date, fws_run, common_name) |> 
  summarise(catch = sum(r_catch, na.rm = TRUE),
            cumulative_catch = cumsum(catch)) |>
  ungroup() |> 
  group_by(date, fws_run, common_name) |> 
  summarise(total_catch = sum(catch, na.rm = TRUE),
            cumulative_percent_catch = round((cumulative_catch / total_catch * 100), 0)) |> 
  glimpse()

daily_catch_wide <- daily_catch |> 
  pivot_wider(names_from = c(fws_run, common_name),
              values_from = cumulative_percent_catch) |> 
  glimpse()
  

# Run this Code to output the Excel file
write.xlsx(cum.catch, "C:/Users/mschraml/Desktop/ROutput/LCC Cumulative Catch.xlsx")

