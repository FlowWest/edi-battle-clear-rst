# This code will calculate the daily catch, passage, fork length range,
# weekly and brood year upper and lower 90% confidence limits 
# for the lower Clear Creek rotary screw trap site 
# Use the BOR Passage query to obtain the needed catch data set.
# Use the BOR SID query to obtain the turbidity, and mark-recapture data set
# The data created by this code will be used for the BOR biweekly reports
# The flow and temperature data will be scraped from CDEC

# This script is a refactor of code by Mike Schraml, USFWS

if (!require("tidyverse")) {            # Data management
  install.packages("tidyverse")
  library(tidyverse)
}

# Set brood years for filtering if desired
# you will have to uncomment filtering in the script
# brood_years <- c(2021, 2022)

# data prep --------------------------------------------------------------

# Load needed data sets, rename columns, separate SampleID into julian date
# and year. Format columns and subset data to two week period.

# create new tables based on edi tables and old tables  -------------------------
catch <- read.csv("data/catch.csv") |> 
  mutate(year = year(sample_date),
         week = week(sample_date),
         jdate = yday(sample_date)) |> 
  glimpse()

release <- read.csv("data/release.csv") |> 
  glimpse()

recapture <- read.csv("data/recapture.csv") |> 
  group_by(site, release_site, release_id) |> 
  summarise(number_recaptured = sum(number_recaptured, na.rm = T),
            median_fork_length_recaptured = median(median_fork_length_recaptured, na.rm = T)) |> 
  glimpse()

weekly_mark_recap <- left_join(release, recapture, by = c("release_id", "site", "release_site")) |> 
  mutate(date_released = as.Date(date_released),
         year = year(date_released), 
         week = week(date_released)) |> 
  group_by(site, release_site, release_id, year, week) |> 
  summarize(number_recaptured = ifelse(is.na(number_recaptured), 0, number_recaptured),
            efficiency = (number_recaptured + 1)/(number_released + 1)) |> 
  ungroup() |> 
  glimpse()

# join catch to mark recaps 
catch_data <- left_join(catch, weekly_mark_recap, 
                        by = c("week", "year")) |> 
  filter(!is.na(brood_year),
         !station_code %in% c("", NA)) |> 
  mutate(fws_run = ifelse(fws_run == "not provided", NA, fws_run)) |> 
  glimpse()


sample_data <- read_csv("data/trap.csv") |> 
  filter(!is.na(station_code)) |> 
  select(station_code, sample_id, sample_date, turbidity) |> 
  mutate(year = year(sample_date), 
         week = week(sample_date)) |> 
  glimpse()

# daily passage ---------------------------------------------------------------

global_efficiency <- catch_data |> 
  group_by(station_code, year) |> 
  summarise(global_efficiency = mean(efficiency, na.rm = T)) |> 
  fill(global_efficiency) |> 
  glimpse()

# filter to Chinook and Rainbow trout and group by race and brood year
# group by trap, race, date, station, and sum catch data. Join with
# sample_data to get dates. replace NAs in catch with 0
daily_catch_summary <- catch_data |> 
  mutate(date = as.Date(sample_date)) |> 
  filter(common_name %in% c("Rainbow Trout", "Chinook Salmon")) |> 
  group_by(brood_year, common_name, fws_run, date, station_code) |> 
  left_join(global_efficiency, by = c("station_code", "year")) |> 
  summarise(catch = sum(r_catch, na.rm = TRUE),
            efficiency = mean(efficiency, na.rm = TRUE),
            efficiency = ifelse(efficiency %in% c("NaN", NA), global_efficiency, efficiency)) |>  # uses global efficiency in place of NA efficiency 
  ungroup() |> 
  mutate(catch = if_else(is.na(catch), 0, catch, efficiency)) |> 
  select(date, catch, efficiency, common_name, fws_run, station_code, brood_year) |> 
  glimpse()

# calculate daily passage
daily_passage <- daily_catch_summary |> 
  #mutate(passage = ifelse(is.na(efficiency), catch, round((catch / efficiency), 0))) |> # this is helpful if you have NA efficiency
  mutate(passage = round((catch / efficiency), 0)) |> 
  select(date, passage, common_name, fws_run, station_code, brood_year) |> 
  glimpse()

# wide format to match original code
daily_passage_wide <- daily_passage |> 
  filter(!is.na(passage)) |> 
  group_by(common_name, fws_run, station_code, brood_year) |> 
  pivot_wider(names_from = c(common_name, fws_run, station_code, brood_year), 
              values_from = passage) |>
  glimpse()


# fork lengths ------------------------------------------------------------

# select data and filter out fl == 0 and to brood year == 2021 and 2022
# join with sample_data to get dates
# filter out any NA Dates
# summarise by minimum and maximum fork lengths
fork_length_summary <- catch_data |> 
  mutate(date = as.Date(sample_date)) |> 
  select(date, brood_year, year, fws_run, fork_length, common_name, station_code) |> 
  filter(common_name %in% c("Rainbow Trout", "Chinook Salmon"),
         fork_length != 0,
         !is.na(date)) |> 
  # filter(between(brood_year, brood_years[1], brood_years[2])) |> # uncomment this if you want to filter to brood years
  group_by(brood_year, common_name, fws_run, station_code, date) |>
  summarise(minimum = min(fork_length), maximum = max(fork_length)) |> 
  ungroup() |> 
  glimpse()

fork_length_summary_wide <- fork_length_summary |> 
  pivot_wider(names_from = c(common_name, fws_run, station_code, brood_year), 
              values_from = c(minimum, maximum)) |> 
  glimpse()

# join fork length to passage ---------------------------------------------

# reformat date
daily_summary_full <- daily_passage |> 
  left_join(fork_length_summary, by = c("date", "common_name", "fws_run", "station_code", 
                                        "brood_year")) |> 
  glimpse()

# pivot wide to match original code
daily_summary_full_wide <- daily_summary_full |> 
  pivot_wider(id_cols = date,
              names_from = c(common_name, fws_run, station_code, brood_year), 
              values_from = passage) |> 
  glimpse()

# Igo hourly water temperatures -------------------------------------------

# load data and rename columns. remove NAs, reformat Date
# filepaths will need to be updated to reflect your repository structure
temp <- read.csv("data-raw/scripts_and_data_from_natasha/CLEAR CREEK NEAR IGO (temp).csv") |> 
  rename(Temperature = TEMP.W.DEG.F) |> 
  select(-DATE...TIME..PST.) |> 
  filter(!is.na(Temperature)) |> 
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> 
  glimpse()

flow <- read.csv("data-raw/scripts_and_data_from_natasha/CLEAR CREEK NEAR IGO (flow).csv") |> 
  rename(cfs = FLOW.CFS) |> 
  select(-DATE...TIME..PST.) |> 
  filter(!is.na(cfs)) |> 
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> 
  glimpse()

# calculate peak daily flow (discharge) at IGO
peak_flow_daily <- flow |> 
  group_by(Date) |> 
  summarise(maximum_cfs = max(cfs))

# calculate mean daily water temperature at Igo
mean_temp_daily <- temp |> 
  group_by(Date) |> 
  summarise(Temperature = round(mean(Temperature), 1)) # renaming Temperature according to old code

# turbidity ---------------------------------------------------------------
turbidity <- sample_data |> 
  select(Date = sample_date, turbidity) |> 
  glimpse()

# join data frames --------------------------------------------------------

# join turbidity, fork length, temperature, and flow
# rename columns so they match with main stem
environmental_data_forklength <- peak_flow_daily |> 
  full_join(mean_temp_daily, by = "Date") |> 
  full_join(turbidity, by = "Date") |> 
  full_join(fork_length_summary |> 
              rename(Date = date), by = "Date") |> 
  rename("Discharge volume (cfs)" = maximum_cfs,
         "Water temperature (C)" = Temperature,
         "Water turbidity (NTU)" = turbidity,
         maximum_fl = maximum,
         minimum_fl = minimum) |> 
  glimpse()

# table with passage to reflect SacPAS format:
# https://www.cbr.washington.edu/sacramento/data/query_redbluff_daily.html
environmental_data_forklength_passage <- environmental_data_forklength |> 
  full_join(daily_summary_full |> 
              select(-c(minimum, maximum)), 
            by = c("Date" = "date", "brood_year",
                   "common_name", "fws_run", "station_code")) |> glimpse()


# write data --------------------------------------------------------------
# as appropriate
