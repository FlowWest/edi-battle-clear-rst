# This code will calculate the biweekly and brood year  passage indices (to date), 95% confidence limits and standard error.
# at the lower Clear Creek RST site.
# Notes:  For this to run correctly the IDWeek must be a character and be from 01 to 52.
# Data will come from the current year's database
# Use the following query BOR Passage
# Mike Schraml, 05/04/2022

# This script is a refactor of BOR Bootstrap LCC.R
# 1-23-2022

# Load needed Packages
if (!require("readxl")) {            # for importing data sets
  install.packages("readxl")
  library(readxl)
}

if (!require("tidyverse")) {            # for data manipulations and summaries
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("openxlsx")) {         # for writing xlsx files to the desk top
  install.packages("openxlsx")
  library(openxlsx)
}


# bootstrap across weeks 14 and 15 ----------------------------------------

# Load data sets
broodyears <- c(2021, 2022)
IDweeks <- c(14, 15)

catch <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha", "BOR Data.csv")) |> 
  filter(OrganismCode %in% c("CHN", "RBT"),
         BroodYear %in% broodyears,
         IDWeek %in% IDweeks) |> 
  mutate(strata.week = paste0(IDWeek, SubWeek)) |> 
  glimpse()

# create a data frame with only data for efficiency
efficiency_summary <- catch |> 
  distinct(strata.week, BaileysEff, NumReleased, OrganismCode, 
           StationCode, FWSRace, BroodYear) |> 
  glimpse()

# summarize catch by strata week and join with efficiency values
strata_catch_summary <- catch |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear, strata.week) |> 
  summarise(w.catch = sum(RCatch, na.rm = T)) |> 
  left_join(efficiency_summary, by = c("OrganismCode", "StationCode", "FWSRace", 
                                       "BroodYear", "strata.week")) |> 
  arrange(strata.week) |> 
  glimpse()

# calculate weekly passage indices
weekly_passage_indices <- strata_catch_summary |> 
  mutate(passage = round(w.catch/BaileysEff)) |> glimpse()

create_names <- weekly_passage_indices |> 
  mutate(FWSRace = ifelse(FWSRace == "", "UK", FWSRace),
         codes = paste0(OrganismCode, "_", StationCode, "_", FWSRace, "_", BroodYear, "_", strata.week),
         codes_biweekly = paste0(OrganismCode, "_", StationCode, "_", FWSRace, "_", BroodYear))

name_codes <- unique(create_names$codes)
biweekly_name_codes <- unique(create_names$codes_biweekly) # without strata.week

# bootstrap process
bootstrap_summary <- tibble(lower_CI = rep(0, nrow(weekly_passage_indices)),
                            upper_CI = rep(0, nrow(weekly_passage_indices)),
                            passage = rep(0, nrow(weekly_passage_indices)),
                            SE = rep(0, nrow(weekly_passage_indices)))

bootstrap_results <- data.frame(matrix(0, nrow = 1000, ncol = nrow(weekly_passage_indices)))
names(bootstrap_results) <- name_codes

set.seed(2323)
for(i in 1:nrow(weekly_passage_indices)){
  
  weekly_catch <- weekly_passage_indices$w.catch[i]
  efficiency_value <- as.numeric(signif(rep(weekly_passage_indices$BaileysEff[i]), 4))
  num_released_value <- as.numeric(weekly_passage_indices$NumReleased[i])
  
  catch_vector <- rep(weekly_catch, 1000)
  efficiency_vector <- rep(efficiency_value, 1000)
  recapture_vector <- round(rbinom(1000, num_released_value, efficiency_value), 0)
  
  weekly_unordered_bootstrap <- round(weekly_catch * (num_released_value + 1) / (recapture_vector + 1), 0)
  
  # get confidence intervals
  bootstrap_summary$lower_CI[i] <- quantile(weekly_unordered_bootstrap, 0.050)
  bootstrap_summary$upper_CI[i] <- quantile(weekly_unordered_bootstrap, 0.950)
  bootstrap_summary$SE[i] <- sd(weekly_unordered_bootstrap) / sqrt(1000)
  bootstrap_summary$passage[i] <- weekly_passage_indices$passage[i]
  
  # store vector
  bootstrap_results[,i] <- weekly_unordered_bootstrap
}

# insert bootstrap results into biweekly table ----------------------------
# pivot longer so you can group by strata.week and summarize
bootstrap_results_long <- bootstrap_results |> 
  pivot_longer(cols = everything(), names_to = "code") |> 
  separate(code, into = c("OrganismCode", "StationCode", "FWSRace", "BroodYear", "strata.week"),
           sep = "_") |> 
  rename(bootstrap_results = value) |> 
  glimpse()

# summarize weekly passage indices across strata.weeks (for biweekly passage indices)
biweekly_passage_indices <- weekly_passage_indices |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |> # don't group by strata.week here to sum across both weeks within groups
  summarise(biweekly_passage_estimate = sum(passage, na.rm = T))

# summarize bootstrap results across strata.weeks and take the 5th and 95th percentiles
biweekly_bootstrap_CIs <- bootstrap_results_long |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |>  
  summarise(lower_CI = quantile(bootstrap_results, 0.05),
            upper_CI = quantile(bootstrap_results, 0.95)) |> 
  mutate(BroodYear = as.numeric(BroodYear),
         FWSRace = if_else(FWSRace == "UK", "", FWSRace)) |> 
  glimpse()

# insert into biweekly table (corrolary of "biweekly" in original bootstrap.R)
biweekly_passage_index <- tibble(code = biweekly_name_codes) |> 
  separate(code, into = c("OrganismCode", "StationCode", "FWSRace", "BroodYear"),
           sep = "_") |>
  mutate(BroodYear = as.numeric(BroodYear),
         FWSRace = if_else(FWSRace == "UK", "", FWSRace)) |> 
  left_join(biweekly_passage_indices, by = c("OrganismCode", "FWSRace", "StationCode", "BroodYear")) |> 
  left_join(biweekly_bootstrap_CIs, by = c("OrganismCode", "FWSRace", "StationCode", "BroodYear")) |> 
  glimpse()


# bootstrap across brood years 21 and 22 (all weeks) -------------------------------------------------

catch_brood_year <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha", "BOR Data.csv")) |> 
  filter(OrganismCode %in% c("CHN", "RBT"),
         BroodYear %in% broodyears) |>  
  mutate(FWSRace = if_else(FWSRace == "N/P", "", FWSRace)) |> 
  glimpse()

# create a data frame with only data for efficiency
# take mean efficiency values across OrganismCode, StationCode, FWSRace, and BroodYear
# TODO run this by erin - it creates similar values but still is slightly off. 
# if you don't take the mean eff and numreleased value across years, you end up with lots of
# "distinct" values which means that later when you sum across years, you get really high numbers
efficiency_summary_brood_year <- catch_brood_year |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |> 
  mutate(mean_eff = mean(BaileysEff, na.rm = T),
         mean_num_released = mean(NumReleased, na.rm = T)) |> 
  distinct(OrganismCode, StationCode, FWSRace, BroodYear,
         mean_eff, mean_num_released) |> 
  glimpse()

# summarize catch by brood year and join with efficiency values
catch_summary_brood_year <- catch_brood_year |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |> 
  summarise(brood_year_catch = sum(RCatch, na.rm = T)) |> 
  left_join(efficiency_summary_brood_year, by = c("OrganismCode", "StationCode", "FWSRace", 
                                       "BroodYear")) |> 
  glimpse()

# calculate passage indices
brood_year_passage_indices <- catch_summary_brood_year |> 
  mutate(passage = round(brood_year_catch/mean_eff)) |> glimpse()

by_names <- brood_year_passage_indices |> 
  mutate(FWSRace = ifelse(FWSRace == "", "UK", FWSRace),
         codes = paste0(OrganismCode, "_", StationCode, "_", FWSRace, "_", BroodYear))
by_names <- unique(by_names$codes)

# bootstrap process
by_bootstrap_summary <- tibble(lower_CI = rep(0, nrow(brood_year_passage_indices)),
                               upper_CI = rep(0, nrow(brood_year_passage_indices)),
                               passage = rep(0, nrow(brood_year_passage_indices)),
                               SE = rep(0, nrow(brood_year_passage_indices)))

by_bootstrap_results <- data.frame(matrix(0, nrow = 1000, ncol = nrow(brood_year_passage_indices)))
names(by_bootstrap_results) <- by_names

set.seed(2323)
for(i in 1:nrow(brood_year_passage_indices)){
  
  by_catch <- brood_year_passage_indices$brood_year_catch[i]
  efficiency_value <- as.numeric(signif(rep(brood_year_passage_indices$mean_eff[i]), 4))
  num_released_value <- as.numeric(round(brood_year_passage_indices$mean_num_released[i]), 0)
  
  catch_vector <- rep(by_catch, 1000)
  efficiency_vector <- rep(efficiency_value, 1000)
  recapture_vector <- round(rbinom(1000, num_released_value, efficiency_value), 0)
  
  by_unordered_bootstrap <- round(by_catch * (num_released_value + 1) / (recapture_vector + 1), 0)
  
  # get confidence intervals
  by_bootstrap_summary$lower_CI[i] <- quantile(by_unordered_bootstrap, 0.050)
  by_bootstrap_summary$upper_CI[i] <- quantile(by_unordered_bootstrap, 0.950)
  by_bootstrap_summary$SE[i] <- sd(by_unordered_bootstrap) / sqrt(1000)
  by_bootstrap_summary$passage[i] <- brood_year_passage_indices$passage[i]
  
  # store vector
  by_bootstrap_results[,i] <- by_unordered_bootstrap
}

# pivot longer so you can group by strata.week and summarize
by_bootstrap_results_long <- by_bootstrap_results |> 
  pivot_longer(cols = everything(), names_to = "code") |> 
  separate(code, into = c("OrganismCode", "StationCode", "FWSRace", "BroodYear"),
           sep = "_") |> 
  rename(by_bootstrap_results = value) |> 
  glimpse()

# summarize brood year passage indices across all weeks
brood_year_passage_indices <- brood_year_passage_indices |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |> 
  summarise(brood_year_passage_estimate = sum(passage, na.rm = T)) |> 
  mutate(BroodYear = as.numeric(BroodYear)) |> glimpse()

# summarize bootstrap results across strata.weeks and take the 5th and 95th percentiles
broodyear_bootstrap_CIs <- by_bootstrap_results_long |> 
  ungroup() |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |>  
  summarise(lower_CI = quantile(by_bootstrap_results, 0.05),
            upper_CI = quantile(by_bootstrap_results, 0.95)) |> 
  mutate(FWSRace = if_else(FWSRace == "UK", "", FWSRace),
         BroodYear = as.numeric(BroodYear)) |> 
  glimpse()

# insert into broodyear table (corrolary of "brood.year" in original bootstrap.R)
brood_year_passage_index <- tibble(code = by_names) |> 
  separate(code, into = c("OrganismCode", "StationCode", "FWSRace", "BroodYear"),
           sep = "_") |>
  mutate(BroodYear = as.numeric(BroodYear),
         FWSRace = if_else(FWSRace == "UK", "", FWSRace)) |> 
  left_join(brood_year_passage_indices, by = c("OrganismCode", "FWSRace", "StationCode", "BroodYear")) |> 
  left_join(broodyear_bootstrap_CIs, by = c("OrganismCode", "FWSRace", "StationCode", "BroodYear")) |> 
  glimpse()







