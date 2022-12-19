
# This code will calculate the daily catch, passage, fork length range,
# weekly and brood year upper and lower 90% confidence limits 
# for the lower Clear Creek rotary screw trap site 
# Use the BOR Passage query to obtain the needed catch data set.
# Use the BOR SID query to obtain the turbidity, and mark-recapture data set
# The data created by this code will be used for the BOR biweekly reports
# The flow and temperature data will be scraped from CDEC
# Create the SampleID.xls spreadsheet
# Created: Mike Schraml 02/08/2022


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


# Set brood year, calendar year, Julian dates and strata
brood_years <- c(2021, 2022)
weeks <- c(13, 14)
julian_dates <- c(85, 98) 

# Change brood year when needed
r.names.a <- c("Date", "BY21 RBT")
r.names.b <- c("Date", "BY22 RBT")
l.names.a <- c("Date", "BY21 Late-fall")
l.names.b <- c("Date", "BY22 Late-fall")
w.names.a <- c("Date", "BY21 Winter")
w.names.b <- c("Date", "BY22 Winter")
s.names <- c("Date", "BY21 Spring")
f.names <- c("Date", "BY21 Fall")

# Change brood year when needed
l.names.fl.a <- c("Date", "BY21 Late-Fall minimum FL", "BY21 Late-fall maximum FL")
l.names.fl.b <- c("Date", "BY22 Late-Fall minimum FL", "BY22 Late-fall maximum FL")
w.names.fl.a <- c("Date", "BY21 Winter minimum FL", "BY21 Winter maximum FL")
w.names.fl.b <- c("Date", "BY22 Winter minimum FL", "BY22 Winter maximum FL")
f.names.fl <- c("Date", "BY21 Fall minimum FL", "BY21 Fall maximum FL")
s.names.fl <- c("Date", "BY21 Spring minimum FL", "BY21 Spring maximum FL")
r.names.fla <- c("Date", "BY21 RBT FL", "BY21 RBT maximum FL")
r.names.flb <- c("Date", "BY22 RBT minimum FL", "BY22 RBT maximum FL")


# data prep --------------------------------------------------------------

# Load needed data sets, rename columns, separate SampleID into julian date
# and year. Format columns and subset data to two week period.
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


# daily passage ---------------------------------------------------------------

# filter to Chinook and Rainbow trout and group by race and brood year
# group by trap, race, date, station, and sum catch data. Join with
# sample_data to get dates. replace NAs in catch with 0
daily_catch_summary <- catch_data |> 
  filter(OrganismCode %in% c("RBT", "CHN")) |> 
  group_by(BroodYear, OrganismCode, FWSRace, jdate, StationCode, Date) |>
  summarise(catch = sum(RCatch, na.rm = TRUE)) |> 
  full_join(sample_data, by = c("jdate", "StationCode", "Date")) |> 
  mutate(catch = if_else(is.na(catch), 0, catch, BaileysEff)) |> 
  select(SampleID, Date, catch, BaileysEff, OrganismCode, FWSRace, StationCode) |> 
  glimpse()

# calculate daily passage
daily_passage <- daily_catch_summary |> 
  mutate(passage = round(catch / BaileysEff), 0) |> 
  select(Date, passage, OrganismCode, FWSRace, StationCode) |> 
  glimpse()

# wide format to match original code
daily_passage_wide <- daily_passage |> 
  pivot_wider(names_from = c(OrganismCode, FWSRace, StationCode, BroodYear), 
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
  select(Date, BroodYear, IDYear, FWSRace, ForkLength, OrganismCode, StationCode, jdate) |> 
  filter(OrganismCode %in% c("RBT", "CHN"),
         ForkLength != 0,
         !is.na(Date),
         between(BroodYear, brood_years[1], brood_years[2])) |> 
  group_by(BroodYear, OrganismCode, FWSRace, StationCode, Date, jdate) |>
  full_join(sample_data, by = c("StationCode", "Date", "jdate")) |> 
  summarise(minimum = min(ForkLength), maximum = max(ForkLength)) |> 
  glimpse()

fork_length_summary_wide <- fork_length_summary |> 
  pivot_wider(names_from = c(OrganismCode, FWSRace, StationCode, BroodYear), 
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
  full_join(fork_length_summary, by = c("Date", "OrganismCode", "FWSRace", "StationCode", 
                                        "jdate", "BroodYear")) |>
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> 
  glimpse()

# pivot wide to match original code
daily_summary_full_wide <- daily_summary_full |> 
  pivot_wider(names_from = c(OrganismCode, FWSRace, StationCode, BroodYear), 
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
  select(Date, Turbidity) |> 
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y"))) |> 
  glimpse()


# join data frames --------------------------------------------------------

# join turbidity, fork length, temperature, and flow
# rename columns so they match with main stem
environmental_data_forklength <- peak_flow_daily |> 
  full_join(mean_temp_daily, by = "Date") |> 
  full_join(turbidity, by = "Date") |> 
  full_join(fork_length_summary, by = "Date") |> 
  rename("Discharge volume (cfs)" = maximum_cfs,
         "Water temperature (C)" = Temperature,
         "Water turbidity (NTU)" = Turbidity,
         maximum_fl = maximum,
         minimum_fl = minimum) |> 
  glimpse()


# write data --------------------------------------------------------------

write.csv(environmental_data_forklength, here::here("data-raw", "BOR Daily Passage and FL.csv", row.names = FALSE))







