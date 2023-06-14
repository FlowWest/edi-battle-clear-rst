
# This code will calculate the daily catch, passage, fork length range,
# weekly and brood year upper and lower 90% confidence limits 
# for the lower Clear Creek rotary screw trap site 
# Use the BOR Passage query to obtain the needed catch data set.
# Use the BOR SID query to obtain the turbidity, and mark-recapture data set
# The data created by this code will be used for the BOR biweekly reports
# The flow and temperature data will be scraped from CDEC
# Create the SampleID.xls spreadsheet

# This script is a refactor of Daily Passage and FL LCC.R
# 12-22-2022

# Load needed Packages
if (!require("readxl")) {            # for importing data sets
  install.packages("readxl")
  library(readxl)
}

if (!require("tidyverse")) {            # Data management
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("lubridate")) {          # for handling dates
  install.packages("lubridate")
  library(lubridate)
}


# Set brood years, calendar year, Julian dates and strata
brood_years <- c(2021, 2022)
weeks <- c(13, 14)
julian_dates <- c(85, 98) 

# TODO figure out if we need julian date
# data prep --------------------------------------------------------------

# Load needed data sets, rename columns, separate SampleID into julian date
# and year. Format columns and subset data to two week period.
# 
# ORIGINAL TABLES -------------------------------------------------------------
catch_data <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha", "BOR Data.csv")) |> 
  rename(Date = SampleDate) |> 
  separate(SampleID, c("jdate", "year"), "_") |>
  mutate(RCatch = as.double(RCatch),
         jdate = as.numeric(jdate),
         year = as.numeric(year),
         Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> 
  filter(between(jdate, julian_dates[1], julian_dates[2])) |> glimpse()

sample_data <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha", "LCC SampleID.csv")) |> 
  rename(Date = SampleDate) |> 
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> glimpse()


# CREATE NEW TABLES BASED ON EDI TABLES AND OLD TABLES -------------------------
catch <- read.csv("data/catch.csv") |> 
  mutate(year = year(sample_date),
         week = week(sample_date),
         jdate = yday(sample_date)) |> glimpse()

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
                        by = c("week", "year")) |> #TODO see if we can map release sites to traps 
  glimpse()


sample_data <- read_csv("data/trap.csv") |> 
  select(station_code, sample_id, sample_date, turbidity) |> 
  mutate(year = year(sample_date), 
         week = week(sample_date)) |> glimpse()

# daily passage ---------------------------------------------------------------

# filter to Chinook and Rainbow trout and group by race and brood year
# group by trap, race, date, station, and sum catch data. Join with
# sample_data to get dates. replace NAs in catch with 0
daily_catch_summary <- catch_data |> 
  mutate(date = as.Date(sample_date)) |> 
  filter(common_name %in% c("Rainbow Trout", "Chinook Salmon")) |> 
  group_by(brood_year, common_name, fws_run, date, station_code) |> 
  summarise(catch = sum(r_catch, na.rm = TRUE),
            efficiency = mean(efficiency, na.rm = TRUE),
            efficiency = ifelse(efficiency == "NaN", NA, efficiency)) |>  # TODO how to keep efficiency? 
  ungroup() |> 
  mutate(catch = if_else(is.na(catch), 0, catch, efficiency)) |> 
  select(date, catch, efficiency, common_name, fws_run, station_code, brood_year) |> 
  glimpse()

# calculate daily passage
daily_passage <- daily_catch_summary |> 
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


# rename headers
names(rbt.pa) <- r.names.a
names(rbt.pb) <- r.names.b
names(lfcs.pa) <- l.names.a
names(lfcs.pb) <- l.names.b
names(wcs.pa) <- w.names.a
names(wcs.pb) <- w.names.b
names(scs.p) <- s.names
names(fcs.p) <- f.names


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
         !is.na(date),
         between(brood_year, brood_years[1], brood_years[2])) |> 
  group_by(brood_year, common_name, fws_run, station_code, date) |>
  summarise(minimum = min(fork_length), maximum = max(fork_length)) |> 
  ungroup() |> 
  glimpse()

fork_length_summary_wide <- fork_length_summary |> 
  pivot_wider(names_from = c(common_name, fws_run, station_code, brood_year), 
              values_from = c(minimum, maximum)) |> 
  glimpse()

# TODO: some columns are all NAs - where does this happen?
# TODO: rename columns? Might be iffy for automation


# Rename headers
names(fl.wa) <- w.names.fl.a
names(fl.wb) <- w.names.fl.b
names(fl.s) <- s.names.fl
names(fl.f) <- f.names.fl
names(fl.lfa) <- l.names.fl.a
names(fl.lfb) <- l.names.fl.b
names(rbt.fla) <- r.names.fla
names(rbt.flb) <- r.names.flb


# join fork length to passage ---------------------------------------------

# reformat date
daily_summary_full <- daily_passage |> 
  left_join(fork_length_summary, by = c("date", "common_name", "fws_run", "station_code", 
                                        "brood_year")) |> 
  glimpse()

# pivot wide to match original code
daily_summary_full_wide <- daily_summary_full |> 
  pivot_wider(names_from = c(common_name, fws_run, station_code, brood_year), 
              values_from = c(minimum, maximum, passage)) |> 
  glimpse()


# Igo hourly water temperatures -------------------------------------------

# load data and rename columns. remove NAs, reformat Date
temp <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha", "CLEAR CREEK NEAR IGO (temp).csv")) |> 
  rename(Temperature = TEMP.W.DEG.F) |> 
  select(-DATE...TIME..PST.) |> 
  filter(!is.na(Temperature)) |> 
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> 
  glimpse()

flow <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha", "CLEAR CREEK NEAR IGO (flow).csv")) |> 
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


# write data --------------------------------------------------------------

write.csv(environmental_data_forklength, here::here("data-raw", "BOR Daily Passage and FL.csv", row.names = FALSE))







