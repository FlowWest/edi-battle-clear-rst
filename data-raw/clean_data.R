# clean_data.R
library(tidyverse)
library(janitor)
library(lubridate)
library(googledrive)

# catch
# catch_early <- read.csv(here::here("data", "catch_early.csv")) |> glimpse() # no longer needed thanks to historical db
catch_current2025 <- read.csv(here::here("data", "catch_current2025.csv")) |> glimpse()
catch_current <- read.csv(here::here("data", "catch_current.csv")) |> glimpse()
catch_late <- read.csv(here::here("data", "catch_late.csv")) |> glimpse()
catch_historical <- read.csv(here::here("data", "catch_historical.csv")) |> glimpse()

# to convert subsample column
# frac_to_decimal <- function(x) {
#   new_vals <- sapply(x, function(x) eval(parse(text = x)))
#   return(new_vals)
# }

# For this update we have new data in 2 databases. We have some from 2024 and some in 2025
catch_current <- bind_rows(catch_current,
                           catch_current2025 |> 
                             filter(sample_date >= "2024-10-01") |> 
                             select(-field1))
catch <- bind_rows(catch_historical |> 
                     filter(sample_date < min(catch_late$sample_date, na.rm = T)),
                   catch_late |> 
                     filter(sample_date < min(catch_current$sample_date, na.rm = T)), 
                     catch_current) |> 
  relocate(c(sample_id, sample_date, station_code, count, r_catch), .before = fork_length) |> 
  filter(!is.na(sample_date)) |> 
  mutate(sample_date = as.Date(sample_date),
        fws_run = case_when(fws_run == "F" ~ "fall",
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
min(catch$count, na.rm = T)
max(catch$count, na.rm = T)
min(catch$r_catch, na.rm = T)
max(catch$r_catch, na.rm = T)
min(catch$fork_length, na.rm = T)
max(catch$fork_length, na.rm = T)
min(catch$weight, na.rm = T)
max(catch$weight, na.rm = T)
# trap
# trap_early <- read.csv(here::here("data", "trap_early.csv")) |> glimpse()
trap_current2025 <- read.csv(here::here("data", "trap_current2025.csv")) |> glimpse()
trap_current <- read.csv(here::here("data", "trap_current.csv")) |> glimpse()
trap_late <- read.csv(here::here("data", "trap_late.csv")) |> glimpse()
trap_historical <- read.csv(here::here("data", "trap_historical.csv")) |> glimpse()

# For this update we have new data in 2 databases. We have some from 2024 and some in 2025
trap_current <- bind_rows(trap_current,
                           trap_current2025 |> 
                             filter(sample_date >= "2024-10-01"))

trap <- bind_rows(trap_historical |> 
                    filter(sample_date < min(trap_late$sample_date, na.rm = T)),
                  trap_late |> 
                    filter(sample_date < min(trap_current$sample_date, na.rm = T)), 
                  trap_current) |> 
  #filter(as.Date(sample_date) <= as.Date("2022-09-30")) |> # until newer data is QC'd - check with Mike
  select(-c(lunar_phase, start_counter)) |> 
  rename(end_counter = counter) |> 
  mutate(sample_date = as.Date(sample_date),
         trap_start_date = as.Date(trap_start_date),
         thalweg = case_when(thalweg == "Y" ~ TRUE, 
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

# there are multiple sample ids because of how data are collected
ck <- trap |> 
  group_by(sample_id, station_code, year(sample_date)) |> 
  tally() |> 
  filter(n > 1)

trap |> filter(sample_id == "331_24")

min(trap$sample_date, na.rm = T)
max(trap$sample_date, na.rm = T)
min(trap$sample_time, na.rm = T)
max(trap$sample_time, na.rm = T)
min(trap$trap_start_date, na.rm = T)
max(trap$trap_start_date, na.rm = T)
min(trap$trap_start_time, na.rm = T)
max(trap$trap_start_time, na.rm = T)
min(trap$depth_adjust, na.rm = T)
max(trap$depth_adjust, na.rm = T)
min(trap$avg_time_per_rev, na.rm = T)
max(trap$avg_time_per_rev, na.rm = T)
min(trap$flow_start_meter, na.rm = T)
max(trap$flow_start_meter, na.rm = T)
min(trap$flow_end_meter, na.rm = T)
max(trap$flow_end_meter, na.rm = T)
min(trap$flow_set_time, na.rm = T)
max(trap$flow_set_time, na.rm = T)
min(trap$river_left_depth, na.rm = T)
max(trap$river_left_depth, na.rm = T)
min(trap$river_center_depth, na.rm = T)
max(trap$river_center_depth, na.rm = T)
min(trap$river_right_depth, na.rm = T)
max(trap$river_right_depth, na.rm = T)
min(trap$end_counter, na.rm = T)
max(trap$end_counter, na.rm = T)
min(trap$debris_tubs, na.rm = T)
max(trap$debris_tubs, na.rm = T)
min(trap$velocity, na.rm = T)
max(trap$velocity, na.rm = T)
min(trap$turbidity, na.rm = T)
max(trap$turbidity, na.rm = T)
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
                                  TRUE ~ release_site)) |> 
  relocate(c(release_id, date_recaptured, release_site, site), 
           .before = number_recaptured) |> 
  glimpse()

min(recapture$date_recaptured, na.rm = T)
max(recapture$date_recaptured, na.rm = T)
min(recapture$number_recaptured, na.rm = T)
max(recapture$number_recaptured, na.rm = T)
min(recapture$median_fork_length_recaptured, na.rm = T)
max(recapture$median_fork_length_recaptured, na.rm = T)
# to distinguish between LCC/UCC for Clear Creek and RM 8.3/8.4 for UBC

# release - clear
release_clear <- read_csv(here::here("data", "clear_release.csv")) |> 
  # filter(year(date_released) >= "2020") |> 
  mutate(time_released = as.character(time_released),
         release_turbidity = as.numeric(release_turbidity)) |> 
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
         time_released = hms::as_hms(time_released)) |> 
  relocate(c(release_id, date_released, time_released, release_site, site), 
           .before = number_released) |> # reorder columns
  glimpse()

min(release$date_released, na.rm = T)
max(release$date_released, na.rm = T)
min(release$time_released, na.rm = T)
max(release$time_released, na.rm = T)

min(release$number_released, na.rm = T)
max(release$number_released, na.rm = T)
min(release$median_fork_length_released, na.rm = T)
max(release$median_fork_length_released, na.rm = T)
min(release$days_held_post_mark, na.rm = T)
max(release$days_held_post_mark, na.rm = T)
min(release$release_temp, na.rm = T)
max(release$release_temp, na.rm = T)
min(release$release_turbidity, na.rm = T)
max(release$release_turbidity, na.rm = T)
min(release$release_flow, na.rm = T)
max(release$release_flow, na.rm = T)
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

# data are getting too big to push to GitHub to solve this we will store on Google Drive
drive_find(n_max = 30) # check your googledrive connection

drive_upload_function <- function(media, file_name) {
  drive_upload(media,
               path = "clear_battle_rst_edi/data",
               name = file_name,
               type = "spreadsheet",
               overwrite = T)
}
# Note that may need to update permissions
drive_upload_function("data/release.csv", "release.csv")
drive_upload_function("data/recapture.csv", "recapture.csv")
drive_upload_function("data/trap.csv", "trap.csv")
drive_upload_function("data/catch.csv", "catch.csv")
