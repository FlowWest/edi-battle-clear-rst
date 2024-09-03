# mark recapture tables

library(tidyverse)
library(lubridate)
library(googleCloudStorageR)
library(ggplot2)
library(scales)


# Mark recapture data for Upper Battle Creek ------------------------------------
# Timeframe 2003-2021

# This is the new excel that Mike sent August 2024
# read in data
raw_mark_recapture_battle <- readxl::read_excel(here::here("data-raw", "scripts_and_data_from_natasha",
                                                    "Mark-Recap_Database_MASTER_BC_2024.xlsx"), 
                                         sheet = 2, skip  = 2) |> glimpse()

mark_recapture_data_battle <- raw_mark_recapture_battle |> 
  janitor::clean_names() |> 
  filter(release_date != "No Mark/Recap Studies for 2014-2015 Season") |>
  mutate(release_date = janitor::excel_numeric_to_date(as.numeric(as.character(release_date)), date_system = "modern")) |>
  glimpse()

mark_recapture_data_battle |> 
  group_by(release_date) |>
  summarise(daily_flow = mean(flow_cfs_release),
            mean_efficency = mean(baileys_trap_efficiency)) |>
  ggplot() +
  geom_point(aes(x = daily_flow, y = mean_efficency)) + 
  theme_minimal()

# reformat
battle_mark_recapture <- mark_recapture_data_battle |> 
  select(release_date, day_or_night_release = d_ay_or_n_ight_release, release_time, number_marked,
         number_released, recaps, mortality, median_mark_fork_length_mm, median_recap_fork_length_mm, 
         origin, days_held_post_mark, release_water_temp_f, flow_cfs_release, release_turbidity_ntu, cone_status_h_f = cone_status_h_alf_f_ull, 
         mean_temp_day_of_rel = mean_daily_water_temp_f_day_of_release, mean_flow_day_of_rel = mean_flow_cfs_day_of_release, caught_day_1, caught_day_2, 
         caught_day_3, caught_day_4, caught_day_5) |>
  mutate(release_time = hms::as_hms(release_time),
         day_or_night_release = case_when(day_or_night_release == "?" ~ "unknown", 
                                          day_or_night_release == "D" ~ "day",
                                          day_or_night_release == "N" ~ "night"),
         origin = case_when(origin == "H" ~ "hatchery", 
                            origin == "N" ~ "natural"),
         release_temp = as.numeric(release_water_temp_f),
         cone_status = case_when(cone_status_h_f == "H" ~ "half", 
                                 cone_status_h_f == "F" ~ "full"),
         caught_day_2 = as.numeric(caught_day_2),
         release_id = paste0("BAT", row_number())) |> 
  pivot_longer(cols = caught_day_1:caught_day_5, names_to = "date_recaptured", values_to = "number_recaptured") %>%
  mutate(date_recaptured = case_when(date_recaptured == "caught_day_1" ~ release_date + 1, 
                                     date_recaptured == "caught_day_2" ~ release_date + 2,
                                     date_recaptured == "caught_day_3" ~ release_date + 3,
                                     date_recaptured == "caught_day_4" ~ release_date + 4,
                                     date_recaptured == "caught_day_5" ~ release_date + 5,),
         median_recap_fork_length_mm = ifelse(number_recaptured == 0 | is.na(number_recaptured), 
                                                NA, as.numeric(median_recap_fork_length_mm))) |> 
  select( -cone_status_h_f) |> 
  glimpse()
  
battle_released <- battle_mark_recapture |> 
  mutate(site = "Upper Battle Creek") |> 
  select(site, release_date, release_time, number_released, median_fork_length_released = median_mark_fork_length_mm, 
         release_id, days_held_post_mark, day_or_night_release, release_temp, release_flow = flow_cfs_release, release_turbidity = release_turbidity_ntu, 
         origin) %>% 
  filter(release_date >= as_date("2003-10-01")) %>%
  rename(date_released = release_date,
         origin_released = origin,
         time_released = release_time) %>% 
  distinct() %>% 
  glimpse()

battle_recaptured <-  battle_mark_recapture %>% 
  mutate(site = "Upper Battle Creek") |> 
  select(site, release_id, date_recaptured, number_recaptured, 
         median_fork_length_recaptured = median_recap_fork_length_mm) %>% 
  filter(date_recaptured >= as_date("2003-10-01")) %>%
  glimpse()

write_csv(battle_released, here::here("data", "battle_release.csv"))
write_csv(battle_recaptured, here::here("data", "battle_recapture.csv"))

# mark recapture table Clear Creek  -----------------------------------------------------------------

# read in data
mark_recapture_raw_clear <- readxl::read_excel(here::here("data-raw", "scripts_and_data_from_natasha",
                                                    "Mark-Recap_Database_MASTER_CC_2024.xlsx"), 
                                         sheet = 2, skip = 2) |> glimpse()

mark_recapture_data_clear <- mark_recapture_raw_clear |> 
  janitor::clean_names() |> 
  glimpse()

mark_recapture_data_clear |> 
  group_by(release_date) |>
  summarise(daily_flow = mean(flow_cfs_release),
            mean_efficency = mean(baileys_trap_efficiency)) |>
  ggplot() +
  geom_point(aes(x = daily_flow, y = mean_efficency)) + 
  theme_minimal()

# reformat
clear_mark_recapture <- mark_recapture_data_clear |> 
  select(release_date, day_or_night_release = d_ay_or_n_ight_release, release_time, number_marked,
         number_released, recaps, median_mark_fork_length_mm, median_recap_fork_length_mm, 
         clip = clip_status, days_held_post_mark, release_water_temp_f, flow_cfs_release, release_turbidity_ntu, cone_status_h_f_recap = cone_status_h_alf_f_ull, 
         mean_temp_day_of_rel = mean_daily_water_temp_f_day_of_release, mean_flow_day_of_rel = mean_flow_cfs_day_of_release, caught_day_1, caught_day_2, 
         caught_day_3, caught_day_4, caught_day_5, release_site = clear_creek_site) |>
  mutate(release_time = hms::hms(days = as.numeric(release_time)),
         day_or_night_release = case_when(day_or_night_release == "?" ~ "unknown", 
                                          day_or_night_release == "D" ~ "day",
                                          day_or_night_release == "N" ~ "night"),
         release_temp = as.numeric(release_water_temp_f),
         cone_status = case_when(cone_status_h_f_recap == "H" ~ "half", 
                                 cone_status_h_f_recap == "F" ~ "full"),
         release_id = paste0("CLR", row_number()),
         release_date = as.Date(release_date)) |> 
  pivot_longer(cols = caught_day_1:caught_day_5, names_to = "date_recaptured", values_to = "number_recaptured") %>%
  mutate(date_recaptured = case_when(date_recaptured == "caught_day_1" ~ release_date + 1, 
                                     date_recaptured == "caught_day_2" ~ release_date + 2,
                                     date_recaptured == "caught_day_3" ~ release_date + 3,
                                     date_recaptured == "caught_day_4" ~ release_date + 4,
                                     date_recaptured == "caught_day_5" ~ release_date + 5,),
         recap_med_fork_length_mm = ifelse(number_recaptured == 0 | is.na(number_recaptured), 
                                           NA, as.numeric(median_recap_fork_length_mm)),
         date_recaptured = as.character(date_recaptured),
         release_date = as.character(release_date),
         release_time = as.character(release_time)) |> 
  select(-cone_status_h_f_recap) |> 
  glimpse()


clear_released <- clear_mark_recapture |> 
  mutate(site = "Clear Creek") |> 
  select(site, release_site, release_date, release_time, number_released, median_fork_length_released = median_recap_fork_length_mm, 
         release_id, days_held_post_mark, day_or_night_release, release_temp, release_flow = flow_cfs_release, release_turbidity = release_turbidity_ntu) %>% 
  filter(release_date >= as_date("2003-10-01")) %>% 
  rename(date_released = release_date,
         time_released = release_time) %>% 
  distinct() %>% 
  glimpse()

clear_recaptured <-  clear_mark_recapture %>% 
  mutate(site = "Clear Creek") |> 
  select(site, release_site, release_id, date_recaptured, number_recaptured, 
         median_fork_length_recaptured = recap_med_fork_length_mm) %>% 
  filter(date_recaptured >= as_date("2003-10-01")) %>%
  glimpse()

write_csv(clear_released, here::here("data", "clear_release.csv"))
write_csv(clear_recaptured, here::here("data", "clear_recapture.csv"))

# notes from 11-23 mtg
# TODO clip status is in file that was sent over 1-11-2023
# TODO release sites are always the same 1-11-2023


  