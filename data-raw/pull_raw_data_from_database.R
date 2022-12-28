# install.packages("RODBC")
library(RODBC)
library(Hmisc)
library(tidyverse)
library(lubridate)

# Set up connection with CAMP access database (PC)
# access_database <- odbcConnectAccess2007(here::here("data-raw", "scripts_and_data_from_natasha", "2022-2023_RST Database unproofed 11-22-22.accdb"))

# main tables (PC)
# catch_raw <- sqlFetch(access_database, "Catch") 
# trap_sample <- sqlFetch(access_database, "Sample") 

# lookups (PC)
# run_lu <- sqlFetch(access_database, "RaceList") 
# lifestage_lu <- sqlFetch(access_database, "StagesLookUp") 
# stations_lu <- sqlFetch(access_database, "StationsLookUp") |> glimpse()
# organisim_lu <- sqlFetch(access_database, "OrganismsLookUp") 

# main tables (mac)
filepath <- "data-raw/scripts_and_data_from_natasha/2022-2023_RST_Database_unproofed_11-22-22.accdb"
mdb.get(file = filepath, tables = TRUE)
catch_raw <- mdb.get(filepath, "Catch") |> glimpse()
trap_sample <- mdb.get(filepath, "Sample") |> glimpse()

# lookups (mac)
run_lu <- mdb.get(filepath, "RaceList") |> glimpse()
lifestage_lu <- mdb.get(filepath, "StagesLookUp") |> glimpse()
stations_lu <- mdb.get(filepath, "StationsLookUp")  |> glimpse()
organism_lu <- mdb.get(filepath, "OrganismsLookUp") |> glimpse()


View(catch_raw)
View(trap_sample)

# cleaned_catch

cleaned_catch <- catch_raw |> 
  full_join(trap_sample, by = c("SampleRowID" = "SampleRowID")) |> 
  left_join(run_lu, by = c("Race" = "RaceCode")) |> 
  left_join(lifestage_lu, by = c("LifeStage" = "LifeStage")) |> 
  left_join(stations_lu, by = c("StationCode" = "StationCode")) |> 
  left_join(organism_lu, by = c("OrganismCode" = "OrganismCode")) |> 
  select(SampleID, SampleDate, SampleTime, Location, StationCode, 
         CommonName, Count, ForkLength, Weight, Race, LifeStage = StageName, 
         RCatch, Interp, BroodYear) |> 
  mutate(Run = case_when(Race == "N/P" ~ "not recorded", 
                         Race == "W" ~ "winter", 
                         Race == "S" ~ "spring", 
                         Race == "F" ~ "fall", 
                         Race == "L" ~ "late fall"), 
         LifeStage = case_when(LifeStage == "not provided" ~ "not recorded",
                               LifeStage %in% c("CHN - obvious fry", "RBT - fry") ~ "fry", 
                               LifeStage %in% c("CHN - silvery parr", "RBT - silvery parr") ~ "silvery parr",
                               LifeStage == "CHN - smolt" ~ "smolt",
                               LifeStage == "CHN - yolk sac fry" ~ "yolk sac fry",
                               LifeStage == "RBT - parr" ~ "parr",
                               T ~ LifeStage),
         Interp = tolower(Interp)) |> 
  select(-Race) |> 
  glimpse()

# TODO think about NA vs not recorded 
unique(cleaned_catch$Run) # why no fall run 
unique(cleaned_catch$Location)
unique(cleaned_catch$LifeStage)

# cleaned_trap
cleaned_trap <- trap_sample |> 
  left_join(stations_lu, by = c("StationCode" = "StationCode")) |> 
  select(SampleID, SampleDate, SampleTime, StationCode, Location, 
         TrapFishing, Counter, FlowStartMeter, FlowEndMeter, 
         StartCounter, Velocity, Turbidity) |>  
  glimpse()
# TODO temp and discharge?
# TODO visit type, trapfunctioning, include catch - necessary?


# TODO release table?
