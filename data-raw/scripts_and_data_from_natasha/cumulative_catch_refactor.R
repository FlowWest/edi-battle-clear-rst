# This code will calculate daily catch, cumulative catch, and percent cumulative catch
# for brood years at the lower Clear Creek rotary screw trap. The data comes from 4 queries
# Report Data CHN, Report Data WCS, Report Data LFFCS, and Report Data RBT
# Mike Schraml 02/02/2022


# This script is a refactor of Cumulative catch LCC.R
# 12-27-2022

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

sample_data <- read_csv(here::here("data-raw", "scripts_and_data_from_natasha", 
                               "BOR Data.csv")) |> 
  mutate(Date = as.Date(SampleDate, format = "%m/%d/%Y")) |> 
  select(Date, BroodYear, FWSRace, RCatch, OrganismCode, 
         StationCode) |> 
  glimpse()


# cumulative catch --------------------------------------------------------

# original code filters to brood year 2019 and excludes interpolated values.
# I can't find the datasets used in the original code. The BOR Data.csv has
# similar columns but does not have an interpolation column or broodyear == 2019.

brood_year <- 2019

# insert missing dates and set new dates Rcatch to 0
sample_data |> 
  pad(interval = "day",
      start_val = lubridate::ymd("2019-01-01"),
      end_val = lubridate::ymd("2020-06-30")) |> 
  fill_by_value(Rcatch, value = 0) |> glimpse()


# Insert missing dates and set new dates RCatch to zero
rbt <- pad(rbt, interval = "day",
           start_val = lubridate::ymd("2019-01-01"),
           end_val = lubridate::ymd("2020-06-30")) %>%
  fill_by_value(RCatch, value = 0)

lfc <- pad(lfc, interval = "day",
           start_val = lubridate::ymd("2019-04-01"),
           end_val = lubridate::ymd("2020-03-31")) %>%
  fill_by_value(RCatch, value = 0)

wcs <- pad(wcs, interval = "day",
           start_val = lubridate::ymd("2019-07-01"),
           end_val = lubridate::ymd("2020-06-30")) %>%
  fill_by_value(RCatch, value = 0)

scs <- pad(scs, interval = "day",
           start_val = lubridate::ymd("2019-10-01"),
           end_val = lubridate::ymd("2020-09-30")) %>%
  fill_by_value(RCatch, value = 0)

fcs <- pad(fcs, interval = "day",
           start_val = lubridate::ymd("2019-10-01"),
           end_val = lubridate::ymd("2020-09-30")) %>%
  fill_by_value(RCatch, value = 0)

# Change FWSRace NAs to S, W or L for days no fish were captured, so that the dates are not lost when filtering
wcs$FWSRace[wcs$RCatch >-1] <- "W"
scs$FWSRace[scs$RCatch >-1] <- "S"
fcs$FWSRace[fcs$RCatch >-1] <- "F"
lfc$FWSRace[lfc$RCatch >-1] <- "L"

## Calculate Daily Catch, this will be used for the cumulative and % cumulative calculations.
# Select needed data, order by SampleID, group by SampleID, sum daily catches
wcs <- wcs %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

scs <- scs %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

fcs <- fcs %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

lfc <- lfc %>% arrange(SampleDate) %>%
  filter(FWSRace == "L") %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

rbt <- rbt %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

##################### Calculate cumulative catches #####################


# calculate the cumulative daily catch
scs <- mutate(scs, cum.scs = cumsum(catch))
wcs <- mutate(wcs, cum.wcs = cumsum(catch))
fcs <- mutate(fcs, cum.fcs = cumsum(catch))
lfc <- mutate(lfc, cum.lfc = cumsum(catch))
rbt <- mutate(rbt, cum.rbt = cumsum(catch))

# Find total catch
t.catch.s <- sum(scs$catch)
t.catch.w <- sum(wcs$catch)
t.catch.f <- sum(fcs$catch)
t.catch.l <- sum(lfc$catch)
t.catch.rbt <- sum(rbt$catch)

# calculate the cumulative percent of daily catch and round to 0.1
scs <- mutate(scs, scs.pdc = round(cum.scs/t.catch.s * 100, 1))
wcs <- mutate(wcs, wcs.pdc = round(cum.wcs/t.catch.w * 100, 1))
fcs <- mutate(fcs, fcs.pdc = round(cum.fcs/t.catch.f * 100, 1))
lfc <- mutate(lfc, lfc.pdc = round(cum.lfc/t.catch.l * 100, 1))
rbt <- mutate(rbt, rbt.pdc = round(cum.rbt/t.catch.rbt * 100, 1))

# Combine data sets
chn.cum.p <-cbind.data.frame(scs$SampleDate, scs$catch, scs$cum.scs, scs$scs.pdc,
                             fcs$catch, fcs$cum.fcs, fcs$fcs.pdc)


# Rename columns
colnames(chn.cum.p) <- c("Date", "SCS catch", "SCS Cumulative", "SCS % cumulative", "FCS catch", "FCS Cumulative", "FCS % cumulative")
colnames(wcs) <- c("Date", "WCS catch", "WCS Cumulative", "WCS % cumulative")
colnames(lfc) <- c("Date", "LFCS catch", "LFCS Cumulative", "LFCS % cumulative")
colnames(rbt) <- c("Date", "RBT catch", "RBT Cumulative", "RBT % cumulative")

# First create list of the data frames and the worksheet names to be written to desk top
cum.catch <- list("RBT Cumulative" = rbt, "lfcS Cumulative" = lfc, "WCS Cumulative" = wcs, "CHN Cumulative" = chn.cum.p)

# Run this Code to output the Excel file
write.xlsx(cum.catch, "C:/Users/mschraml/Desktop/ROutput/LCC Cumulative Catch.xlsx")













