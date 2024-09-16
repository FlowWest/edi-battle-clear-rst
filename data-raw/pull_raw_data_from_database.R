# install.packages("RODBC")
library(RODBC)
library(Hmisc)
library(tidyverse)
library(lubridate)
library(janitor)

# Mike Schaml provided updated databased files June 2024: 2022-2023 database and 2023-2024 database
# The 2022-2023 database originally only had data through January 2023 (we only included through Sept 2022 in EDI package)
# TODO
# Items to check with Mike and Kaitin
# filter(catch, is.na(sample_date)) 
# trap |> 
#   group_by(sample_id, station_code) |> 
#   tally() |> 
#   filter(n > 1)


# 2023-2024 RST database --------------------------------------------------
# read in mdb
library(Hmisc)
filepath <- here::here("data-raw", "scripts_and_data_from_natasha", 
                       "2023-2024_RST_Database_20240606.accdb")
tables <- mdb.get(filepath, tables = TRUE) 
tables

# tables are: BioSample, Run, Sample, Catch, DataDscriptions, HistoricalSampleID
# data
catch_raw <- mdb.get(filepath, "Catch", mdbexportArgs = '') |> glimpse()
trap_raw <- mdb.get(filepath, "Sample", mdbexportArgs = '') |> glimpse()

# store all LU tables in a named list
LU_table_names <- tables[! tables %in% c("Catch", "Sample")]
LU_tables <- sapply(LU_table_names, function(x){
  return(mdb.get(filepath, paste0(x)))
}, USE.NAMES = T)

detach(package:Hmisc)

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
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "ConditionList") |> 
              select(ValueCode, Gear_Description = CodeDescription),
            by = c("GearConditionCode" = "ValueCode")) |> 
  mutate(SampleTime = str_sub(as.character(SampleTime), 11, 18),
         SampleTime = if_else(SampleTime == "", NA_character_, SampleTime),
         TrapStartTime = str_sub(as.character(TrapStartTime), 11, 18),
         TrapStartTime = if_else(TrapStartTime == "", NA_character_, TrapStartTime)) |> 
  select(-c(WeatherCode, Habitat, TrapSampleType, Diel, DebrisType,
            BaileysEff, ReportBaileysEff, NumReleased, UserName, UserName2,
            TrapComments, GearConditionCode, SampleRowID)) |> 
  rename(Habitat = Habitat_Description, 
         TrapSampleType = TrapSampleType_Description,
         Diel = Diel_Description,
         DebrisType = DebrisType_Description,
         Gear_Condition = Gear_Description) |> 
  clean_names() |> 
  glimpse()

# catch
catch <- catch_raw |> 
  left_join(LU_tables$OrganismsLookUp |> 
              select(OrganismCode, CommonName) |> 
              labelled::remove_var_label(),
            by = "OrganismCode") |> 
  left_join(LU_tables$RaceList |> 
              select(RaceCode, Race_Description = Description),
            by = c("Race" = "RaceCode")) |> 
  left_join(LU_tables$StagesLookUp |> 
              select(LifeStage, StageName),
            by = "LifeStage") |> 
  left_join(trap_raw |> 
              select(SampleRowID, StationCode, SampleDate, SampleID),
            by = "SampleRowID") |> 
  select(-c(OrganismCode, Race, LifeStage, "SampleRowID")) |> 
  rename(LifeStage = StageName) |> 
  mutate(LifeStage = str_remove_all(LifeStage, "RBT - "),
         LifeStage = str_remove_all(LifeStage, "CHN - "),
         Race = if_else(Race_Description == "N/P", "not provided", Race_Description),
         Subsample = if_else(Subsample == "N/P", "not provided", Subsample),
         Dead = ifelse(Dead == "Y", TRUE, FALSE),
         Interp = ifelse(Interp == "NO", FALSE, TRUE)) |> 
  select(-c(Race_Description, FishFLTS, KFactor, ReportCatch, ReportAgeClass, ReportRace,
            RCatchCom)) |> 
  rename(Run = Race, FWSRun = FWSRace) |> 
  clean_names() |> 
  glimpse()

# This database is "2023-10-02" to "2024-06-06"
min(catch$sample_date, na.rm = T)
ck <- filter(catch, is.na(sample_date)) # 8 that have no date. should we remove?
ck <- filter(trap, is.na(sample_date))
max(catch$sample_date, na.rm = T)

write_csv(catch, here::here("data", "catch_current.csv"))
write_csv(trap, here::here("data", "trap_current.csv"))

# 2022-2023 RST database (mike sent new one in June 2024) ----------------------
# read in mdb
library(Hmisc)
filepath <- here::here("data-raw", "scripts_and_data_from_natasha", 
                       "2022-2023_RST_Database_202406.accdb")
tables <- mdb.get(filepath, tables = TRUE) 
tables

# tables are: BioSample, Run, Sample, Catch, DataDscriptions, HistoricalSampleID
# data
catch_raw <- mdb.get(filepath, "Catch", mdbexportArgs = '') |> glimpse()
trap_raw <- mdb.get(filepath, "Sample", mdbexportArgs = '') |> glimpse()

# store all LU tables in a named list
LU_table_names <- tables[! tables %in% c("Catch", "Sample")]
LU_tables <- sapply(LU_table_names, function(x){
  return(mdb.get(filepath, paste0(x)))
}, USE.NAMES = T)

detach(package:Hmisc)

# trap
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
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "ConditionList") |> 
              select(ValueCode, Gear_Description = CodeDescription),
            by = c("GearConditionCode" = "ValueCode")) |> 
  mutate(SampleTime = str_sub(as.character(SampleTime), 11, 18),
         SampleTime = if_else(SampleTime == "", NA_character_, SampleTime),
         TrapStartTime = str_sub(as.character(TrapStartTime), 11, 18),
         TrapStartTime = if_else(TrapStartTime == "", NA_character_, TrapStartTime)) |> 
  select(-c(WeatherCode, Habitat, TrapSampleType, Diel, DebrisType,
            BaileysEff, ReportBaileysEff, NumReleased, UserName, UserName2,
            TrapComments, GearConditionCode, SampleRowID)) |> 
  rename(Habitat = Habitat_Description, 
         TrapSampleType = TrapSampleType_Description,
         Diel = Diel_Description,
         DebrisType = DebrisType_Description,
         Gear_Condition = Gear_Description) |> 
  clean_names() |> 
  glimpse()

trap |> 
  group_by(sample_id, station_code) |> 
  tally() |> 
  filter(n > 1)

trap |> 
  filter(sample_id == "031_24" & station_code == "LCC")
# catch
catch <- catch_raw |> 
  left_join(LU_tables$OrganismsLookUp |> 
              select(OrganismCode, CommonName) |> 
              labelled::remove_var_label(),
            by = "OrganismCode") |> 
  left_join(LU_tables$RaceList |> 
              select(RaceCode, Race_Description = Description),
            by = c("Race" = "RaceCode")) |> 
  left_join(LU_tables$StagesLookUp |> 
              select(LifeStage, StageName),
            by = "LifeStage") |> 
  left_join(trap_raw |> 
              select(SampleRowID, StationCode, SampleDate, SampleID),
            by = "SampleRowID") |> 
  select(-c(OrganismCode, Race, LifeStage, "SampleRowID")) |> 
  rename(LifeStage = StageName) |> 
  mutate(LifeStage = str_remove_all(LifeStage, "RBT - "),
         LifeStage = str_remove_all(LifeStage, "CHN - "),
         Race = if_else(Race_Description == "N/P", "not provided", Race_Description),
         Subsample = if_else(Subsample == "N/P", "not provided", Subsample),
         Dead = ifelse(Dead == "Y", TRUE, FALSE),
         Interp = ifelse(Interp == "NO", FALSE, TRUE)) |> 
  select(-c(Race_Description, FishFLTS, KFactor, ReportCatch, ReportAgeClass, ReportRace,
            RCatchCom)) |> 
  rename(Run = Race, FWSRun = FWSRace) |> 
  clean_names() |> 
  glimpse()

catch_old_version_2022 <- read_csv(here::here("data", "catch_late.csv"))
trap_old_version_2022 <- read_csv(here::here("data", "trap_late.csv"))
# "2022-10-01" to "2023-09-30", 6359 obs, 817 obs for trap
min(catch_old_version_2022$sample_date, na.rm = T)
ck <- filter(catch_old_version_2022, is.na(sample_date)) #5 that have no date. should we remove?
max(catch_old_version_2022$sample_date, na.rm = T)

min(catch$sample_date, na.rm = T)
ck <- filter(catch, is.na(sample_date)) #5 that have no date. should we remove?
max(catch$sample_date, na.rm = T)
catch |> 
  group_by(station_code, catch_row_id) |> 
  tally() |> 
  filter(n > 1)

write_csv(catch, here::here("data", "catch_late.csv"))
write_csv(trap, here::here("data", "trap_late.csv"))



# The files below are historical and should not be rerun

# 2020-2022 database ------------------------------------------------------
# rm(list=ls())
# # read in mdb
# library(Hmisc)
# filepath <- here::here("data-raw", "scripts_and_data_from_natasha",
#                        "RST_Database_2020-07-06_to_2022-09-30.accdb")
# tables <- mdb.get(filepath, tables = TRUE)
# tables
# # data tables are: BioSample, Catch, Sample, HistoricalSampleID
# # data
# catch_raw <- mdb.get(filepath, "Catch", mdbexportArgs = '') |> glimpse()
# trap_raw <- mdb.get(filepath, "Sample", mdbexportArgs = '') |> glimpse()
# 
# # store all LU tables in a named list
# LU_table_names <- tables[! tables %in% c("Catch", "Sample")]
# LU_tables <- sapply(LU_table_names, function(x){
#   return(mdb.get(filepath, paste0(x)))
# }, USE.NAMES = T)
# detach(package:Hmisc)
# 
# 
# # trap --------------------------------------------------------------------
# 
# trap <- trap_raw |> 
#   left_join(LU_tables$VariableCodesLookUp |> 
#               filter(CodeListName == "WeatherList") |> 
#               select(ValueCode, Weather = CodeDescription),
#             by = c("WeatherCode" = "ValueCode")) |> 
#   left_join(LU_tables$VariableCodesLookUp |> 
#               filter(CodeListName == "HabitatList") |> 
#               select(ValueCode, Habitat_Description = CodeDescription),
#             by = c("Habitat" = "ValueCode")) |> 
#   left_join(LU_tables$VariableCodesLookUp |> 
#               filter(CodeListName == "TrapSampleTypeList") |> 
#               select(ValueCode, TrapSampleType_Description = CodeDescription),
#             by = c("TrapSampleType" = "ValueCode")) |> 
#   left_join(LU_tables$VariableCodesLookUp |> 
#               filter(CodeListName == "DielList") |> 
#               select(ValueCode, Diel_Description = CodeDescription),
#             by = c("Diel" = "ValueCode")) |> 
#   left_join(LU_tables$VariableCodesLookUp |> 
#               filter(CodeListName == "DebrisTypeList") |> 
#               select(ValueCode, DebrisType_Description = FormDisplay),
#             by = c("DebrisType" = "ValueCode")) |> 
#   mutate(LunarPhase = ifelse(LunarPhase == "", NA_character_, LunarPhase)) |> 
#   left_join(LU_tables$LunarPhaseLookUp |>
#               select(LunarPhase = strPhase,
#                      lunar_phase_description = formDisplay) |> 
#               distinct(LunarPhase, lunar_phase_description) |> 
#               filter(lunar_phase_description != ""),
#             by = c("LunarPhase")) |>
#   left_join(LU_tables$VariableCodesLookUp |> 
#               filter(CodeListName == "ConditionList") |> 
#               select(ValueCode, Gear_Description = CodeDescription),
#             by = c("GearConditionCode" = "ValueCode")) |>
#   mutate(SampleTime = str_sub(as.character(SampleTime), 11, 18),
#          SampleTime = if_else(SampleTime == "", NA_character_, SampleTime),
#          TrapStartTime = str_sub(as.character(TrapStartTime), 11, 18),
#          TrapStartTime = if_else(TrapStartTime == "", NA_character_, TrapStartTime),
#          Thalweg = ifelse(Thalweg == "Y", "Yes", "No")) |> 
#   select(-c(WeatherCode, Habitat, TrapSampleType, Diel, DebrisType,
#             UserName, UserName2, LunarPhase, GearConditionCode,
#             UserName, UserName2, TrapComments, LunarPhase, TrapSampleType, 
#             BaileysEff, ReportBaileysEff, NumReleased, SampleRowID)) |> 
#   rename(Habitat = Habitat_Description, 
#          TrapSampleType = TrapSampleType_Description,
#          Diel = Diel_Description,
#          DebrisType = DebrisType_Description,
#          LunarPhase = lunar_phase_description,
#          GearCondition = Gear_Description) |> 
#   clean_names() |> 
#   glimpse()
# 
# catch -------------------------------------------------------------------

# TODO what are ReportCatch, ReportAgeClass, and ReportRace?
catch <- catch_raw |>
  left_join(LU_tables$OrganismsLookUp |>
              select(OrganismCode, CommonName),
            by = "OrganismCode") |>
  left_join(LU_tables$RaceList |>
              select(RaceCode, Race_Description = Description),
            by = c("Race" = "RaceCode")) |>
  left_join(LU_tables$RaceList |>
              select(RaceCode, FWS_Race_Description = Description),
            by = c("FWSRace" = "RaceCode")) |>
  left_join(LU_tables$RaceList |>
              select(RaceCode, Report_Race_Description = Description),
            by = c("ReportRace" = "RaceCode")) |>
  left_join(LU_tables$StagesLookUp |>
              select(LifeStage, StageName),
            by = "LifeStage") |>
  left_join(trap_raw |>
              select(SampleRowID, StationCode, SampleDate, SampleID),
            by = "SampleRowID") |>
  select(-c(OrganismCode, Race, LifeStage, FWSRace, ReportRace, SampleRowID)) |>
  rename(LifeStage = StageName) |>
  mutate(LifeStage = str_remove_all(LifeStage, "RBT - "),
         LifeStage = str_remove_all(LifeStage, "CHN - "),
         Race = if_else(Race_Description == "N/P", "not provided", Race_Description),
         FWS_Race = if_else(FWS_Race_Description == "N/P", "not provided", FWS_Race_Description),
         Report_Race = if_else(Report_Race_Description == "N/P", "not provided", Report_Race_Description),
         Subsample = if_else(Subsample == "N/P", "not provided", Subsample),
         Dead = ifelse(Dead %in% c("Yes", "Y"), TRUE, FALSE),
         Interp = ifelse(Interp == "NO", FALSE, TRUE),
         AgeClass = ifelse(AgeClass == "", NA, AgeClass)) |>
  select(-c(FWS_Race_Description, Race_Description, Report_Race_Description,
            CatchRowID, FishFLTS, KFactor, RCatchCom,
            ReportCatch, ReportAgeClass, Report_Race, FishFLTS)) |>
  rename(FWSRun = FWS_Race, Run = Race) |>
  clean_names() |>
  glimpse()


# write clean files
write_csv(catch, here::here("data", "catch_early.csv"))
write_csv(trap, here::here("data", "trap_early.csv"))


# 1998-2022 database ------------------------------------------------------

# rm(list=ls())
# # read in mdb
# library(Hmisc)
# filepath <- here::here("data-raw", "scripts_and_data_from_natasha",
#                        "RST_Database_1998-2022.accdb")
# tables <- mdb.get(filepath, tables = TRUE)
# tables
# # data tables are: BioSample, Catch, Sample, HistoricalSampleID
# # data
# catch_raw <- mdb.get(filepath, "Catch", mdbexportArgs = '') |> glimpse()
# trap_raw <- mdb.get(filepath, "Sample", mdbexportArgs = '') |> glimpse()
# 
# # store all LU tables in a named list
# LU_table_names <- tables[! tables %in% c("Catch", "Sample")]
# LU_tables <- sapply(LU_table_names, function(x){
#   return(mdb.get(filepath, paste0(x)))
# }, USE.NAMES = T)
# detach(package:Hmisc)
# 
# # trap
# trap_historical <- trap_raw |>
#   left_join(LU_tables$VariableCodesLookUp |>
#               filter(CodeListName == "WeatherList") |>
#               select(ValueCode, Weather = CodeDescription),
#             by = c("WeatherCode" = "ValueCode")) |>
#   left_join(LU_tables$VariableCodesLookUp |>
#               filter(CodeListName == "HabitatList") |>
#               select(ValueCode, Habitat_Description = CodeDescription),
#             by = c("Habitat" = "ValueCode")) |>
#   left_join(LU_tables$VariableCodesLookUp |>
#               filter(CodeListName == "TrapSampleTypeList") |>
#               select(ValueCode, TrapSampleType_Description = CodeDescription),
#             by = c("TrapSampleType" = "ValueCode")) |>
#   left_join(LU_tables$VariableCodesLookUp |>
#               filter(CodeListName == "DielList") |>
#               select(ValueCode, Diel_Description = CodeDescription),
#             by = c("Diel" = "ValueCode")) |>
#   left_join(LU_tables$VariableCodesLookUp |>
#               filter(CodeListName == "DebrisTypeList") |>
#               select(ValueCode, DebrisType_Description = FormDisplay),
#             by = c("DebrisType" = "ValueCode")) |>
#   left_join(LU_tables$VariableCodesLookUp |>
#               filter(CodeListName == "ConditionList") |>
#               select(ValueCode, Gear_Description = CodeDescription),
#             by = c("GearConditionCode" = "ValueCode")) |>
#   mutate(SampleTime = str_sub(as.character(SampleTime), 11, 18),
#          SampleTime = ifelse(SampleTime == "", NA_character_, SampleTime),
#          TrapStartTime = str_sub(as.character(TrapStartTime), 11, 18),
#          TrapStartTime = ifelse(TrapStartTime == "", NA_character_, TrapStartTime),
#          TrapStartDate = str_sub(as.character(TrapStartDate), 2, 9),
#          TrapStartDate = as.Date(TrapStartDate, format = "%m/%d/%y"),
#          UBCSite = ifelse(UBCSite == "", NA, UBCSite)) |>
#   select(-c(WeatherCode, Habitat, TrapSampleType, Diel, DebrisType,
#             BaileysEff, ReportBaileysEff, NumReleased, UserName, UserName2,
#             TrapComments, GearConditionCode, SampleRowID)) |>
#   rename(Habitat = Habitat_Description,
#          TrapSampleType = TrapSampleType_Description,
#          Diel = Diel_Description,
#          DebrisType = DebrisType_Description,
#          Gear_Condition = Gear_Description) |>
#   janitor::clean_names() |>
#   glimpse()
# 
# # catch
# catch_historical <- catch_raw |>
#   left_join(LU_tables$OrganismsLookUp |>
#               mutate(OrganismCode = as.character(OrganismCode),
#                      CommonName = as.character(CommonName)) |>
#               select(OrganismCode, CommonName),
#             by = "OrganismCode") |>
#   left_join(LU_tables$RaceList |>
#               select(RaceCode, Race_Description = Description),
#             by = c("Race" = "RaceCode")) |>
#   left_join(LU_tables$StagesLookUp |>
#               select(LifeStage, StageName),
#             by = "LifeStage") |>
#   left_join(trap_raw |>
#               select(SampleRowID, StationCode, SampleDate, SampleID),
#             by = "SampleRowID") |>
#   select(-c(OrganismCode, Race, LifeStage, "SampleRowID")) |>
#   rename(LifeStage = StageName) |>
#   mutate(LifeStage = str_remove_all(LifeStage, "RBT - "),
#          LifeStage = str_remove_all(LifeStage, "CHN - "),
#          Race = ifelse(Race_Description == "N/P", "not provided", Race_Description),
#          Subsample = case_when(Subsample == "N/P" ~ "not provided",
#                                Subsample == "" ~ NA,
#                                TRUE ~ Subsample),
#          Dead = case_when(Dead %in% c("Yes", "Yes") ~ TRUE,
#                           Dead %in% c("NO", "N0", "No", "no") ~ FALSE,
#                           Dead %in% c("", "N/P") ~ NA),
#          Interp = ifelse(Interp %in% c("YES", "YSE"), TRUE, FALSE),
#          AgeClass = ifelse(AgeClass == "", NA, AgeClass),
#          FWSRace = case_when(FWSRace == "N/P" ~ "not provided",
#                              FWSRace == "" ~ NA,
#                              TRUE ~ FWSRace)) |>
#   select(-c(Race_Description, FishFLTS, KFactor, ReportCatch, ReportAgeClass, ReportRace,
#             RCatchCom)) |>
#   rename(Run = Race, FWSRun = FWSRace) |>
#   janitor::clean_names() |>
#   glimpse()
# 
# write_csv(catch_historical, here::here("data", "catch_historical.csv"))
# write_csv(trap_historical, here::here("data", "trap_historical.csv"))
