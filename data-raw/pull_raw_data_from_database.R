# install.packages("RODBC")
library(RODBC)
library(Hmisc)
library(tidyverse)
library(lubridate)


# 2022-2023 RST database --------------------------------------------------
# read in mdb
library(Hmisc)
filepath <- here::here("data-raw", "scripts_and_data_from_natasha", 
                       "2022-2023_RST_Database_Proofed_01-02-2023_data.accdb")
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

# catch
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
         RCatchCom, CatchRowID)) |> 
  rename(Run = Race, FWSRun = FWSRace) |> 
  clean_names() |> 
  glimpse()

write_csv(catch, here::here("data", "catch_late.csv"))
write_csv(trap, here::here("data", "trap_late.csv"))



# 2020-2022 database ------------------------------------------------------
rm(list=ls())
# read in mdb
library(Hmisc)
filepath <- here::here("data-raw", "scripts_and_data_from_natasha",
                       "RST_Database_2020-07-06_to_2022-09-30.accdb")
tables <- mdb.get(filepath, tables = TRUE)
tables
# data tables are: BioSample, Catch, Sample, HistoricalSampleID
# data
catch_raw <- mdb.get(filepath, "Catch", mdbexportArgs = '') |> glimpse()
trap_raw <- mdb.get(filepath, "Sample", mdbexportArgs = '') |> glimpse()

# store all LU tables in a named list
LU_table_names <- tables[! tables %in% c("Catch", "Sample")]
LU_tables <- sapply(LU_table_names, function(x){
  return(mdb.get(filepath, paste0(x)))
}, USE.NAMES = T)
detach(package:Hmisc)


# trap --------------------------------------------------------------------

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
  mutate(LunarPhase = ifelse(LunarPhase == "", NA_character_, LunarPhase)) |> 
  left_join(LU_tables$LunarPhaseLookUp |>
              select(LunarPhase = strPhase,
                     lunar_phase_description = formDisplay) |> 
              distinct(LunarPhase, lunar_phase_description) |> 
              filter(lunar_phase_description != ""),
            by = c("LunarPhase")) |>
  left_join(LU_tables$VariableCodesLookUp |> 
              filter(CodeListName == "ConditionList") |> 
              select(ValueCode, Gear_Description = CodeDescription),
            by = c("GearConditionCode" = "ValueCode")) |>
  mutate(SampleTime = str_sub(as.character(SampleTime), 11, 18),
         SampleTime = if_else(SampleTime == "", NA_character_, SampleTime),
         TrapStartTime = str_sub(as.character(TrapStartTime), 11, 18),
         TrapStartTime = if_else(TrapStartTime == "", NA_character_, TrapStartTime),
         Thalweg = ifelse(Thalweg == "Y", "Yes", "No")) |> 
  select(-c(WeatherCode, Habitat, TrapSampleType, Diel, DebrisType,
            UserName, UserName2, LunarPhase, GearConditionCode,
            UserName, UserName2, TrapComments, LunarPhase, TrapSampleType, 
            BaileysEff, ReportBaileysEff, NumReleased, SampleRowID)) |> 
  rename(Habitat = Habitat_Description, 
         TrapSampleType = TrapSampleType_Description,
         Diel = Diel_Description,
         DebrisType = DebrisType_Description,
         LunarPhase = lunar_phase_description,
         GearCondition = Gear_Description) |> 
  clean_names() |> 
  glimpse()

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
