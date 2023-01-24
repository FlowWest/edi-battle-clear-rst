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
biweekly_name_codes <- unique(create_names$codes_biweekly)

# bootstrap process
bootstrap_summary <- tibble(lower_CI = rep(NA, nrow(weekly_passage_indices)),
                            upper_CI = rep(NA, nrow(weekly_passage_indices)),
                            passage = rep(NA, nrow(weekly_passage_indices)),
                            SE = rep(NA, nrow(weekly_passage_indices)))

bootstrap_results <- data.frame(matrix(NA, nrow = 1000, ncol = nrow(weekly_passage_indices)))
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


biweekly_passage_index <- data.frame(matrix(NA, nrow = 3, ncol = length(biweekly_name_codes)))
names(biweekly_passage_index) <- biweekly_name_codes

biweekly_passage_estimates <- weekly_passage_indices |> 
  group_by(OrganismCode, StationCode, FWSRace, BroodYear) |> # don't group by strata.week here to sum across both weeks within groups
  summarise(biweekly_passage_estimate = sum(passage, na.rm = T))

# fill in biweekly passage estimate table
# TODO stopped here 1-23-2022
bootstrap_results |> 
  group_by(OrganismCode, StationCode, FWSRace, )
rowwise() |> 
  mutate(rowsums_bootstrap = sum(c_across(cols = everything())),
         rowsums_lower_CI = quantile(c_across(cols = everything()), 0.050),
         rowsums_upper_CI = quantile(c_across(cols = everything()), 0.950)) |> 
  select(rowsums_bootstrap, rowsums_lower_CI, rowsums_upper_CI) |> 
  glimpse()


########################################### End  Rainbow Trout/steelhead first brood year Bootstrap Loop ##################################################

# Sum across rows of the Boots data
rbt.sum.boots.1 <- rowSums(rbt.unordered.boots.1, na.rm = TRUE)
rbt.sum.boots.1

rbt.Alcl.90.1 <- quantile(rbt.sum.boots.1, 0.050)
rbt.Aucl.90.1 <- quantile(rbt.sum.boots.1, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
rbt.Ordered.Boots.1 <- sort(rbt.sum.boots.1)
rbt.Alcl.90a.1 <- rbt.Ordered.Boots.1[50]
rbt.Aucl.90a.1 <- rbt.Ordered.Boots.1[950]

rbt.Alcl.90.1
rbt.Alcl.90a.1
rbt.Aucl.90.1
rbt.Aucl.90a.1

# Calculate biweekly passage Index
rbt.biweekly.passage.1 <- colSums(rbt.p1["rbt.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
rbt.se1 <- sd(rbt.sum.boots.1)/sqrt(1000)

############## Add data to biweekly Table ##############R
biweekly[1, 8] <- rbt.biweekly.passage.1          # biweekly passage
biweekly[2, 8] <- round(rbt.Alcl.90a.1, 0)        # biweekly Lower 90% confidence limit
biweekly[3, 8] <- round(rbt.Aucl.90a.1, 0)        # biweekly Upper 90% confidence limit


# First create list of the data frames and the worksheet names to be written to desk top
l <- list("Biweekly" = biweekly, "Brood Year" = brood.year)

# second run this Code
write.csv(l, "C:/Users/nwingerter/OneDrive - DOI/Desktop/BOR Biweekly Reports/2022 Weeks 13-14/LCC/Output/BOR LCC Bootstraps Weeks 13-14.csv", row.names = FALSE)









