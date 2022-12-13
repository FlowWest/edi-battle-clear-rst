# install.packages("RODBC")
library(RODBC)
library(tidyverse)
library(lubridate)

# Set up connection with CAMP access database 
access_database <- odbcConnectAccess2007(here::here("data-raw", "scripts_and_data_from_natasha",
                                                 "2022-2023_RST Database unproofed 11-22-22.accdb"))

# main tables 
catch_raw <- sqlFetch(access_database, "Catch") 
trap_sample <- sqlFetch(access_database, "Sample") 

# lookups 
run_lu <- sqlFetch(access_database, "RaceList") 
lifestage_lu <- sqlFetch(access_database, "StagesLookUp") 
stations_lu <- sqlFetch(access_database, "StationsLookUp") |> glimpse()
organisim_lu <- sqlFetch(access_database, "OrganismsLookUp") 


View(catch_raw)
View(trap_sample)

cleaned_catch <- catch_raw |> 
  full_join(trap_sample, by = c("SampleRowID" = "SampleRowID")) |> 
  left_join(run_lu, by = c("Race" = "RaceCode")) |> 
  left_join(lifestage_lu, by = c("LifeStage" = "LifeStage")) |> 
  left_join(stations_lu, by = c("StationCode" = "StationCode")) |> 
  left_join(organisim_lu, by = c("OrganismCode" = "OrganismCode")) |> 
  glimpse()
