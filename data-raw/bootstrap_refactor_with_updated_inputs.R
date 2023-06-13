# This code will calculate the biweekly and brood year
# passage indices (to date), 95% confidence limits and standard error.

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


# load in data ------------------------------------------------------------
release <- read.csv("data/release.csv") |> 
  glimpse()

recapture <- read.csv("data/recapture.csv") |> 
  group_by(site, release_site, release_id) |> 
  summarise(number_recaptured = sum(number_recaptured, na.rm = T),
            median_fork_length_recaptured = median(median_fork_length_recaptured, na.rm = T)) |> 
  glimpse()

# calculate efficiency
weekly_mark_recap <- left_join(release, recapture, by = c("release_id", "site", "release_site")) |> 
  mutate(date_released = as.Date(date_released),
         year = year(date_released), 
         week = week(date_released)) |> 
  group_by(site, release_site, release_id, year, week) |> 
  summarize(number_recaptured = ifelse(is.na(number_recaptured), 0, number_recaptured),
            efficiency = (number_recaptured + 1)/(number_released + 1),
            number_released = ifelse(is.na(number_released), 0, number_released)) |> 
  ungroup() |> 
  select(year, week, number_released, efficiency) |> 
  glimpse()

trap <- read.csv(here::here("data", "trap.csv")) |> 
  select(station_code, sample_date, sub_week) |> 
  distinct_all() |> 
  glimpse()


# pull in sub weeks from trap table and efficiency/num_released from weekly_mark_recap table
# recreates the BOR Data.csv table used in original scripts
catch <- read.csv(here::here("data", "catch.csv")) |> 
  mutate(date = as.Date(sample_date),
         week = week(date),
         year = year(date)) |> 
  left_join(weekly_mark_recap, by = c("year", "week")) |> 
  left_join(trap, by = c("station_code", "sample_date")) |> 
  mutate(id_week = ifelse(week < 10, paste0("0", week), as.character(week))) |> 
  glimpse()

catch_data <- catch |> 
  filter(common_name %in% c("Rainbow Trout", "Chinook Salmon")) |> 
  unite(strata, c(id_week, sub_week), sep = "") |> 
  glimpse()

# create a data frame with only efficiency data
efficiency_summary <- catch_data |> 
  select(station_code, fws_run, strata, efficiency, number_released,
         common_name, brood_year) |> 
  distinct_all() |> 
  glimpse()

# summarize catch by strata week and join with efficiency values
strata_catch_summary <- catch_data |> 
  group_by(common_name, station_code, fws_run, brood_year, strata) |> 
  summarise(weekly_catch = sum(r_catch, na.rm = T)) |> 
  left_join(efficiency_summary, by = c("common_name", "station_code", "fws_run", 
                                       "brood_year", "strata")) |> 
  arrange(strata) |>
  mutate(weekly_passage = round(weekly_catch / efficiency),
         id_week = substr(strata, 1, 2)) |> 
  glimpse()


# bootstrap function ---------------------------------------------------------------
bootstrap_function <- function(x, replicates) {
  n <- nrow(x)
  
  bootstraps <- matrix(NA, nrow = replicates, ncol = n) |> 
    data.frame()
  names(bootstraps) <- unique(x$strata)
  
  for(i in 1:n) {
    strata_name <- x$strata[i]
    weekly_catch <- x$weekly_catch[i]
    weekly_passage <- x$weekly_passage[i]
    efficiency <- x$efficiency[i]
    number_released <- x$number_released[i]
    
    if(is.na(efficiency)) {
      bootstraps[, i] <- rep(NA, replicates)
    } else {
      catch_sample <- rep(weekly_catch, replicates)
      efficiency_sample <- rep(efficiency, replicates)
      
      random_recaptures <- round(rbinom(replicates, number_released, efficiency), 0)
      random_weekly_passage <- round(weekly_catch * (number_released + 1) / (random_recaptures + 1), 0)
      
      lcl <- quantile(random_weekly_passage, 0.05)
      ucl <- quantile(random_weekly_passage, 0.95)
      se <- sd(random_weekly_passage) / sqrt(replicates)
      bootstraps[, i] <- random_weekly_passage
    }
    
  }
  # data frame for CIs and SE results
  bootstraps_totals <- rowSums(bootstraps, na.rm = TRUE)
  results_df <- tibble("LCL" = quantile(bootstraps_totals, 0.05, na.rm = TRUE),
                       "passage" = sum(x$weekly_passage, na.rm = TRUE),
                       "UCL" = quantile(bootstraps_totals, 0.95, na.rm = TRUE),
                       "se" = sd(bootstraps_totals) / sqrt(replicates))
  
  return(results_df)
}


# run bootstraps for biweekly ---------------------------------------------

# filters for brood years and weeks
brood_years <- c(2021, 2022)
subset_weeks <- c("01", "02")


set.seed(2323)

biweekly_bootstraps <- strata_catch_summary |> 
  filter(#id_week %in% subset_weeks,
         brood_year %in% brood_years) |> # for biweekly bootstrapping, filter to selected weeks
  group_by(common_name, fws_run, brood_year, station_code) |> 
  group_split() |> 
  purrr::map(function(x) {
    if(length(x$common_name)<= 1) {
      return(tibble("LCL" = NA,
                    "passage" = sum(x$weekly_passage, na.rm = TRUE),
                    "UCL" = NA,
                    "se" = NA,
                    "common_name" = unique(x$common_name),
                    "fws_run" = unique(x$fws_run),
                    "brood_year" = unique(x$brood_year),
                    "station_code" = unique(x$station_code)))
    } else {
      bootstrap_function(x, replicates = 1000) |> 
        mutate("common_name" = unique(x$common_name),
               "fws_run" = unique(x$fws_run),
               "brood_year" = unique(x$brood_year),
               "station_code" = unique(x$station_code))
    }
    
  }) |> 
  list_rbind() |> 
  mutate(selected_strata = paste0(subset_weeks[1], "_", subset_weeks[2])) |> 
  select(station_code, selected_strata, brood_year, common_name, fws_run, 
         LCL, passage, UCL, se) |> 
  glimpse()


# run bootstraps for brood year -------------------------------------------
brood_year_bootstraps <- strata_catch_summary |> 
  group_by(common_name, fws_run, brood_year, station_code) |> 
  group_split() |> 
  purrr::map(function(x) {
    if(length(x$common_name)<= 1) {
      return(tibble("LCL" = NA,
                    "passage" = sum(x$weekly_passage, na.rm = TRUE),
                    "UCL" = NA,
                    "se" = NA,
                    "common_name" = unique(x$common_name),
                    "fws_run" = unique(x$fws_run),
                    "brood_year" = unique(x$brood_year),
                    "station_code" = unique(x$station_code)))
    } else {
      bootstrap_function(x, replicates = 1000) |> 
        mutate("common_name" = unique(x$common_name),
               "fws_run" = unique(x$fws_run),
               "brood_year" = unique(x$brood_year),
               "station_code" = unique(x$station_code))
    }
    
  }) |> 
  list_rbind() |> 
  select(station_code, brood_year, common_name, fws_run, 
         LCL, passage, UCL, se) |> 
  glimpse()


# write table -------------------------------------------------------------

# test with original data -------------------------------------------------

# original data to test function
BOR_data <- read.csv(here::here("data-raw", "scripts_and_data_from_natasha",
                                "BOR Data.csv")) |> 
  janitor::clean_names() |> 
  rename(common_name = organism_code,
         fws_run = fws_race,
         efficiency = baileys_eff,
         number_released = num_released) |> 
  mutate(sample_date = mdy(sample_date),
         id_week = ifelse(id_week < 10, paste0("0", id_week), as.character(id_week))) |> 
  unite(strata, c(id_week, sub_week), sep = "") |> 
  glimpse()

BOR_efficiency <- BOR_data |> 
  select(station_code, fws_run, strata, efficiency, number_released,
         common_name, brood_year) |> 
  distinct_all() |> 
  glimpse()

BOR_strata_summary <- BOR_data |>
  group_by(common_name, station_code, fws_run, brood_year, strata) |> 
  summarise(weekly_catch = sum(r_catch, na.rm = T)) |> 
  left_join(BOR_efficiency, by = c("common_name", "station_code", "fws_run", 
                                   "brood_year", "strata")) |> 
  arrange(strata) |>
  mutate(weekly_passage = round(weekly_catch / efficiency),
         fws_run = ifelse(fws_run == "", NA, fws_run),
         id_week = substr(strata, 1, 2)) |> 
  glimpse()

# biweekly
biweekly_bootstraps <- BOR_strata_summary |> 
  filter(id_week %in% subset_weeks) |> 
  group_by(common_name, fws_run, brood_year, station_code) |> 
  group_split() |> 
  purrr::map(function(x) {
    if(length(x$common_name)<= 1) {
      return(tibble("LCL" = NA,
                    "passage" = sum(x$weekly_passage, na.rm = TRUE),
                    "UCL" = NA,
                    "se" = NA,
                    "common_name" = unique(x$common_name),
                    "fws_run" = unique(x$fws_run),
                    "brood_year" = unique(x$brood_year),
                    "station_code" = unique(x$station_code)))
    } else {
      bootstrap_function(x, replicates = 1000) |> 
        mutate("common_name" = unique(x$common_name),
               "fws_run" = unique(x$fws_run),
               "brood_year" = unique(x$brood_year),
               "station_code" = unique(x$station_code))
    }
    
  }) |> 
  list_rbind()

# brood year
brood_year_bootstraps <- BOR_strata_summary |> 
  group_by(common_name, fws_run, brood_year, station_code) |> 
  group_split() |> 
  purrr::map(function(x) {
    if(length(x$common_name)<= 1) {
      return(tibble("LCL" = NA,
                    "passage" = sum(x$weekly_passage, na.rm = TRUE),
                    "UCL" = NA,
                    "se" = NA,
                    "common_name" = unique(x$common_name),
                    "fws_run" = unique(x$fws_run),
                    "brood_year" = unique(x$brood_year),
                    "station_code" = unique(x$station_code)))
    } else {
      bootstrap_function(x, replicates = 1000) |> 
        mutate("common_name" = unique(x$common_name),
               "fws_run" = unique(x$fws_run),
               "brood_year" = unique(x$brood_year),
               "station_code" = unique(x$station_code))
    }
    
  }) |> 
  list_rbind()

