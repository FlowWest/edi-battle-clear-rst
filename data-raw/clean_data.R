# clean_data.R
library(tidyverse)
library(janitor)
library(lubridate)

# catch
# catch_early <- read.csv(here::here("data", "catch_early.csv")) |> glimpse() # no longer needed thanks to historical db
catch_current <- read.csv(here::here("data", "catch_current.csv")) |> glimpse()
catch_late <- read.csv(here::here("data", "catch_late.csv")) |> glimpse()
catch_historical <- read.csv(here::here("data", "catch_historical.csv")) |> glimpse()

# to convert subsample column
# frac_to_decimal <- function(x) {
#   new_vals <- sapply(x, function(x) eval(parse(text = x)))
#   return(new_vals)
# }

catch <- bind_rows(catch_historical |> 
                     filter(sample_date < min(catch_late$sample_date, na.rm = T)),
                   catch_late |> 
                     filter(sample_date < min(catch_current$sample_date, na.rm = T)), 
                     catch_current) |> 
  relocate(c(sample_id, sample_date, station_code, count, r_catch), .before = fork_length) |> 
  filter(!is.na(sample_date)) |> 
  mutate(fws_run = case_when(fws_run == "F" ~ "fall",
                             fws_run == "W" ~ "winter",
                             fws_run == "S" ~ "spring",
                             fws_run == "L" ~ "late-fall"),
         subsample = case_when(subsample %in% c("2004", "2005", "2007", "2008", "2009", "2010", "2011", 
                                                "2015", "2016", "2017", "2018", "F", "S") ~ NA_character_,
                               subsample == "2-Jan" ~ "1/2",
                               subsample == "4-Jan" ~ "1/4",
                               subsample == "8-Jan" ~ "1/8",
                               subsample == "16-Jan" ~ "1/16",
                               subsample == "Jan-32" ~ "1/32",
                               subsample == "Jan-64" ~ "1/64",
                               TRUE ~ subsample),
         station_code = case_when(station_code == "LCC" ~ "lower clear creek",
                                  station_code == "UCC" ~ "upper clear creek",
                                  station_code == "UBC" ~ "upper battle creek",
                                  station_code == "LBC" ~ "lower battle creek",
                                  station_code == "PHB" ~ "power house battle creek",
                                  station_code == "" ~ NA,
                                  TRUE ~ station_code)) |> 
  select(-c(dead, age_class, run)) |> 
  glimpse()

min(catch$sample_date)
max(catch$sample_date)
# trap
# trap_early <- read.csv(here::here("data", "trap_early.csv")) |> glimpse()
trap_current <- read.csv(here::here("data", "trap_current.csv")) |> glimpse()
trap_late <- read.csv(here::here("data", "trap_late.csv")) |> glimpse()
trap_historical <- read.csv(here::here("data", "trap_historical.csv")) |> glimpse()

trap <- bind_rows(trap_historical |> 
                    filter(sample_date < min(trap_late$sample_date, na.rm = T)),
                  trap_late |> 
                    filter(sample_date < min(trap_current$sample_date, na.rm = T)), 
                  trap_current) |> 
  #filter(as.Date(sample_date) <= as.Date("2022-09-30")) |> # until newer data is QC'd - check with Mike
  select(-c(lunar_phase, start_counter)) |> 
  rename(end_counter = counter) |> 
  mutate(thalweg = case_when(thalweg == "Y" ~ TRUE, 
                             thalweg == "N" ~ FALSE,
                             thalweg %in% c("", "R") ~ NA),
         trap_fishing = ifelse(trap_fishing == 1, TRUE, FALSE),
         partial_sample = ifelse(partial_sample == 1, TRUE, FALSE),
         station_code = case_when(station_code == "LCC" ~ "lower clear creek",
                                  station_code == "UCC" ~ "upper clear creek",
                                  station_code == "UBC" ~ "upper battle creek",
                                  station_code == "LBC" ~ "lower battle creek",
                                  station_code == "PHB" ~ "power house battle creek",
                                  station_code == "" ~ NA,
                                  TRUE ~ station_code)) |> 
  relocate(c(sample_id, sample_date, sample_time, trap_start_date, trap_start_time, station_code),
           .before = depth_adjust) |> 
  glimpse()


ck <- trap |> 
  group_by(sample_id, station_code, year(sample_date)) |> 
  tally() |> 
  filter(n > 1)

trap |> filter(sample_id == "016_05")
# recapture and release ---------------------------------------------------

# recapture - clear
recapture_clear <- read_csv(here::here("data", "clear_recapture.csv")) |> 
  # filter(year(date_recaptured) >= "2020") |> 
  glimpse()

# recapture - battle
recapture_battle <- read_csv(here::here("data", "battle_recapture.csv")) |> 
  # filter(year(date_recaptured) >= "2020") |> 
  glimpse()

recapture <- bind_rows(recapture_clear, recapture_battle) |> 
  select(-site) |> # TODO remove this in the other script
  mutate(site = case_when(release_site %in% c("VB", "P4") ~ "lower clear creek",
                          release_site == "CCRB" ~ "upper clear creek",
                          is.na(release_site) ~ "upper battle creek",
                          TRUE ~ release_site),
         release_site = case_when(release_site == "VB" ~ "vulture bar",
                                  release_site == "P4" ~ "grand matthews permanent turbidity monitoring site",
                                  release_site == "CCRB" ~ "clear creek road bridge",
                                  TRUE ~ release_site),
         fws_run = NA_character_,
         hatchery_origin = NA_character_) |> 
  relocate(c(release_id, date_recaptured, release_site, site), 
           .before = number_recaptured) |> 
  glimpse()

# to distinguish between LCC/UCC for Clear Creek and RM 8.3/8.4 for UBC

# release - clear
release_clear <- read_csv(here::here("data", "clear_release.csv")) |> 
  # filter(year(date_released) >= "2020") |> 
  mutate(time_released = as.character(time_released)) |> 
  glimpse()

# release - battle
release_battle <- read_csv(here::here("data", "battle_release.csv")) |> 
  # filter(year(date_released) >= "2020") |> 
  mutate(time_released = as.character(time_released)) |> 
  glimpse()

release <- bind_rows(release_clear, release_battle) |> 
  select(-site) |> # TODO fix this in other script
  mutate(site = case_when(release_site %in% c("VB", "P4") ~ "lower clear creek",
                          release_site == "CCRB" ~ "upper clear creek",
                          is.na(release_site) ~ "upper battle creek",
                          TRUE ~ release_site),
         release_site = case_when(release_site == "VB" ~ "vulture bar",
                                  release_site == "P4" ~ "grand matthews permanent turbidity monitoring site",
                                  release_site == "CCRB" ~ "clear creek road bridge",
                                  TRUE ~ release_site),
         fws_run = NA_character_,
         hatchery_origin = NA_character_) |> 
  relocate(c(release_id, date_released, time_released, release_site, site), 
           .before = number_released) |> # reorder columns
  glimpse()

# SacPAS daily passage summary
# passage_summary <- read.csv(here::here("data-raw", "BOR Daily Passage and FL.csv")) |> 
#   janitor::clean_names() |> 
#   select(date, station_code, common_name, fws_run, brood_year, passage,
#          minimum_fl, maximum_fl, discharge_volume_cfs, water_temperature_c, 
#          water_turbidity_ntu) |> 
#   glimpse()


# write full datasets -----------------------------------------------------

write_csv(catch, here::here("data", "catch.csv"))
write_csv(trap, here::here("data", "trap.csv"))
write_csv(recapture, here::here("data", "recapture.csv"))
write_csv(release, here::here("data", "release.csv"))
# write.csv(passage_summary, here::here("data", "passage_summary.csv"), row.names = FALSE)


# read and glimpse --------------------------------------------------------

catch <- read_csv(here::here("data", "catch.csv")) |> glimpse()
trap <- read_csv(here::here("data", "trap.csv")) |> glimpse()
recapture <- read_csv(here::here("data", "recapture.csv")) |> glimpse()
release <- read_csv(here::here("data", "release.csv")) |> glimpse()
passage_summary <- read_csv(here::here("data", "passage_summary.csv")) |> glimpse()
