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
         Interp = tolower(Interp),
         SampleTime = as.character(chron::times(strftime(SampleTime, "%H:%M:%S"))),
         SampleDate = as.character(SampleDate)) |> 
  select(-Race, -Location) |> 
  janitor::clean_names() |> 
  glimpse()

write_csv(cleaned_catch, here::here("data", "catch.csv"))

unique(cleaned_catch$Run) # time frame means that we don't have any fall run here
unique(cleaned_catch$Location)
unique(cleaned_catch$LifeStage)

# cleaned_trap
cleaned_trap <- trap_sample |> 
  left_join(stations_lu, by = c("StationCode" = "StationCode")) |> 
  select(SampleID, SampleDate, SampleTime, StationCode, Location, 
         TrapFishing, Counter, FlowStartMeter, FlowEndMeter, 
         StartCounter, Velocity, Turbidity) |> 
  mutate(SampleTime = format(as.POSIXct(gsub("[()]", "", SampleTime), format = "%m/%d/%y %H:%M:%S"), "%H:%M:%S")) |> 
  janitor::clean_names() |> 
  glimpse()

write_csv(cleaned_trap, here::here("data", "trap.csv"))

# 2022-2023 RST database --------------------------------------------------
# read in mdb
library(Hmisc)
filepath <- here::here("data-raw", "2022-2023_RST_Database_Proofed_01-02-2023_data.accdb")
tables <- mdb.get(filepath, tables = TRUE) |> glimpse()
# tables are: BioSample, Run, Sample, Catch, DataDscriptions, HistoricalSampleID
# data
catch_raw <- mdb.get(filepath, "Catch") |> glimpse()
trap_raw <- mdb.get(filepath, "Sample") |> glimpse()

# store all LU tables in a named list
LU_table_names <- tables[! tables %in% c("Catch", "Sample")]
LU_tables <- sapply(LU_table_names, function(x){
  return(mdb.get(filepath, paste0(x)))
}, USE.NAMES = T)

detach(package:Hmisc)

# catch
# TODO missing a date variable
catch <- catch_raw |> 
  left_join(LU_tables$OrganismsLookUp |> 
              select(OrganismCode, CommonName),
            by = "OrganismCode") |> 
  left_join(LU_tables$RaceList |> 
              select(RaceCode, Race_Description = Description),
            by = c("Race" = "RaceCode")) |> 
  left_join(LU_tables$StagesLookUp |> 
              select(LifeStage, StageName),
            by = "LifeStage") |> 
  select(-c(OrganismCode, Race, LifeStage)) |> 
  rename(LifeStage = StageName) |> 
  mutate(LifeStage = str_remove_all(LifeStage, "RBT - "),
         LifeStage = str_remove_all(LifeStage, "CHN - "),
         Race = if_else(Race_Description == "N/P", "not provided", Race_Description),
         Subsample = if_else(Subsample == "N/P", "not provided", Subsample),
         FishFLTS = gsub("[()]", "", FishFLTS),
         Date = mdy(str_replace_all(str_sub(FishFLTS, 1, 8), "/", "-"))) |>
  select(-Race_Description) |> 
  clean_names() |> 
  glimpse()

write_csv(catch, here::here("data", "catch_2022.csv"))

# trap
trap <- trap_raw |> 
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "WeatherList") |> 
              select(ValueCode, Weather = CodeDescription),
            by = c("WeatherCode" = "ValueCode")) |> 
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "HabitatList") |> 
              select(ValueCode, Habitat_Description = CodeDescription),
            by = c("Habitat" = "ValueCode")) |> 
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "TrapSampleTypeList") |> 
              select(ValueCode, TrapSampleType_Description = CodeDescription),
            by = c("TrapSampleType" = "ValueCode")) |> 
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "DielList") |> 
              select(ValueCode, Diel_Description = CodeDescription),
            by = c("Diel" = "ValueCode")) |> 
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "DebrisTypeList") |> 
              select(ValueCode, DebrisType_Description = FormDisplay),
            by = c("DebrisType" = "ValueCode")) |>   
  mutate(SampleTime = str_sub(as.character(SampleTime), 11, 18),
         SampleTime = if_else(SampleTime == "", NA_character_, SampleTime),
         TrapStartTime = str_sub(as.character(TrapStartTime), 11, 18),
         TrapStartTime = if_else(TrapStartTime == "", NA_character_, TrapStartTime)) |> 
  select(-c(WeatherCode, Habitat, TrapSampleType, Diel, DebrisType)) |> 
  rename(Habitat = Habitat_Description, 
         TrapSampleType = TrapSampleType_Description,
         Diel = Diel_Description,
         DebrisType = DebrisType_Description) |> 
  clean_names() |> 
  glimpse()

write_csv(trap, here::here("data", "trap_2022.csv"))



