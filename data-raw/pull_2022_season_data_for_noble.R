# install.packages("RODBC")
library(RODBC)
library(Hmisc)
library(tidyverse)
library(lubridate)

# main tables (mac)
filepath <- "data-raw/RSTDatabase_2020_07_06_to_2022_09_30.accdb"
mdb.get(file = filepath, tables = TRUE)
catch_raw <- mdb.get(filepath, "Catch", mdbexportArgs = '') |> glimpse()
trap_sample <- mdb.get(filepath, "Sample", mdbexportArgs = '') |> glimpse()

View(catch_raw)
View(trap_sample)
# lookups (mac)
run_lu <- mdb.get(filepath, "RaceList") |> glimpse()
lifestage_lu <- mdb.get(filepath, "StagesLookUp") |> glimpse()
stations_lu <- mdb.get(filepath, "StationsLookUp")  |> glimpse()
organism_lu <- mdb.get(filepath, "OrganismsLookUp") |> glimpse()

# cleaned_catch
cleaned_catch <- catch_raw |> 
  left_join(trap_sample, by = c("SampleRowID" = "SampleRowID")) |> 
  left_join(run_lu, by = c("Race" = "RaceCode")) |> 
  left_join(lifestage_lu, by = c("LifeStage" = "LifeStage")) |> 
  left_join(stations_lu, by = c("StationCode" = "StationCode")) |> 
  left_join(organism_lu, by = c("OrganismCode" = "OrganismCode")) |> 
  select(CatchRowID, SampleID, SampleDate, SampleTime, Location, StationCode, 
         CommonName, Count, ForkLength, Weight, Race, LifeStage = StageName, 
         RCatch, Interp, BroodYear, FishFLTS) |> 
  filter(as.Date(SampleDate) > as.Date("2021-10-01"), 
         as.Date(SampleDate) < as.Date("2022-07-01")) |> 
  mutate(Run = case_when(Race == "N/P" ~ "not recorded", 
                         Race == "W" ~ "winter", 
                         Race == "S" ~ "spring", 
                         Race == "F" ~ "fall", 
                         Race == "L" ~ "late fall"), 
         LifeStage = case_when(LifeStage == "not provided" ~ "not recorded",
                               LifeStage %in% c("CHN - obvious fry", "RBT - fry") ~ "fry", 
                               LifeStage %in% c("CHN - silvery parr", "RBT - silvery parr") ~ "silvery parr",
                               LifeStage %in% c("CHN - smolt", "RBT - smolt") ~ "smolt",
                               LifeStage %in% c("CHN - yolk sac fry", "RBT - yolk sac fry") ~ "yolk sac fry",
                               LifeStage %in% c("CHN - parr", "RBT - parr") ~ "parr",
                               T ~ LifeStage),
         Interp = tolower(Interp),
         SampleTime = as.character(chron::times(strftime(SampleTime, "%H:%M:%S"))),
         SampleDate = as.character(SampleDate),
         CommonName = tolower(CommonName),
         StationCode = tolower(StationCode),
         river = ifelse(StationCode == "ubc", "battle creek", "clear creek"),
         Weight = ifelse(Weight == 0, NA, Weight),
         ForkLength = ifelse(ForkLength == 0, NA, ForkLength)) |> 
  distinct() |> 
  janitor::clean_names() |> 
  select(river, station_code, sample_date, sample_time, common_name, run, life_stage, count, fork_length) |> 
  filter(common_name == "chinook salmon", count != 0) |> glimpse()
View(cleaned_catch)
summary(cleaned_catch)
write_csv(cleaned_catch, here::here("data-raw", "battle_clear_catch_2021_2022.csv"))

unique(cleaned_catch$run) # time frame means that we don't have any fall run here
unique(cleaned_catch$station_code)
unique(cleaned_catch$life_stage)
unique(cleaned_catch$common_name)
summary(cleaned_catch)

cleaned_catch |> 
  ggplot(aes(x = as_date(sample_date), y = count)) + geom_col()

cleaned_catch |> ggplot(aes(x = fork_length)) + 
  geom_histogram()

