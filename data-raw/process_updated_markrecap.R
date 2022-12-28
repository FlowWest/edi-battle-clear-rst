# mark recapture tables

library(tidyverse)
library(lubridate)
library(googleCloudStorageR)
library(ggplot2)
library(scales)


# Mark recapture data for Upper Battle Creek ------------------------------------
# Timeframe 2003-2021

# read in data
raw_mark_recapture <- readxl::read_excel(here::here("data-raw", "scripts_and_data_from_natasha",
                                                    "Mark-Recap_Database_MASTER_BC.xlsx"), 
                                         sheet = 2, skip  = 2) |> glimpse()
mark_recapture_data <- raw_mark_recapture |> 
  janitor::clean_names() |> 
  filter(release_date != "No Mark/Recap Studies for 2014-2015 Season") |>
  mutate(release_date = janitor::excel_numeric_to_date(as.numeric(as.character(release_date)), date_system = "modern")) |>
  glimpse()

mark_recapture_data |> 
  group_by(release_date) |>
  summarise(daily_flow = mean(flow_release),
            mean_efficency = mean(baileys_trap_efficiency)) |>
  ggplot() +
  geom_point(aes(x = daily_flow, y = mean_efficency)) + 
  theme_minimal()

# reformat
battle_mark_recapture <- mark_recapture_data |> 
  select(release_date, day_or_night_release = d_ay_or_n_ight_release, release_time, no_marked,
         no_released, recaps, mortality, mark_med_fork_length_mm, recap_med_fork_length_mm, 
         origin_h_n, days_held_post_mark, release_temp, flow_release, release_turbidity, cone_status_h_f, 
         mean_temp_day_of_rel, mean_flow_day_of_rel, caught_day_1, caught_day_2, 
         caught_day_3, caught_day_4, caught_day_5) |>
  mutate(release_time = hms::as_hms(release_time),
         day_or_night_release = case_when(day_or_night_release == "?" ~ "unknown", 
                                          day_or_night_release == "D" ~ "day",
                                          day_or_night_release == "N" ~ "night"),
         origin = case_when(origin_h_n == "H" ~ "hatchery", 
                            origin_h_n == "N" ~ "natural"),
         release_temp = as.numeric(release_temp),
         cone_status = case_when(cone_status_h_f == "H" ~ "half", 
                                 cone_status_h_f == "F" ~ "full")) |> 
  select(-origin_h_n, -cone_status_h_f) |> 
  glimpse()


# mark recapture table Clear Creek  -----------------------------------------------------------------

# read in data
mark_recapture_raw <- readxl::read_excel(here::here("data-raw", "scripts_and_data_from_natasha",
                                                    "Mark-Recap_Database_MASTER_CC.xlsx"), 
                                         sheet = 2, skip = 2) |> glimpse()
mark_recapture_data <- mark_recapture_raw |> 
  janitor::clean_names() |> 
  mutate(release_date = as.Date(release_date, format = "%m/%d/%Y"),
         release_time = hms::as_hms(release_time)) |>
  glimpse()

mark_recapture_data |> 
  group_by(release_date) |>
  summarise(daily_flow = mean(flow_release),
            mean_efficency = mean(baileys_trap_efficiency)) |>
  ggplot() +
  geom_point(aes(x = daily_flow, y = mean_efficency)) + 
  theme_minimal()

# reformat
clear_mark_recapture <- mark_recapture_data |> 
  select(release_date, day_or_night_release = d_ay_or_n_ight_release, release_time, no_marked,
         no_released, recaps, mark_med_fork_length_mm, recap_med_fork_length_mm, 
         clip, days_held_post_mark, release_temp, flow_release, release_turbidity, cone_status_h_f_recap, 
         mean_temp_day_of_rel, mean_flow_day_of_rel, caught_day_1, caught_day_2, 
         caught_day_3, caught_day_4, caught_day_5) |>
  mutate(release_time = hms::as_hms(release_time),
         day_or_night_release = case_when(day_or_night_release == "?" ~ "unknown", 
                                          day_or_night_release == "D" ~ "day",
                                          day_or_night_release == "N" ~ "night"),
         release_temp = as.numeric(release_temp),
         cone_status = case_when(cone_status_h_f_recap == "H" ~ "half", 
                                 cone_status_h_f_recap == "F" ~ "full")) |> 
  select(-cone_status_h_f_recap) |> 
  glimpse()
# TODO missing mortality, hatchery origin
# TODO fix issue w time




  