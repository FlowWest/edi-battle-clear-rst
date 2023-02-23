# clean_data.R
library(tidyverse)
library(janitor)
library(lubridate)

# TODO decide which catch to use
# catch
catch_2022 <- read_csv(here::here("data", "catch_2022.csv")) |> 
  mutate(dead = if_else(dead == "Y", "yes", "no", dead)) |> 
  glimpse()

catch <- read_csv(here::here("data", "catch.csv")) |> 
  glimpse()

# trap
# TODO confirm this is the table we should use
trap <- read_csv(here::here("data", "trap_2022.csv")) |> 
  mutate(sample_time = as.character(sample_time)) |> 
  glimpse()


# recapture - clear
recapture_clear <- read_csv(here::here("data", "clear_recapture.csv")) |> 
  glimpse()

# recapture - battle
recapture_battle <- read_csv(here::here("data", "battle_recapture.csv")) |> 
  glimpse()

# release - clear
release_clear <- read_csv(here::here("data", "clear_release.csv")) |> 
  mutate(time_released = as.character(time_released)) |> 
  glimpse()

# release - battle
release_battle <- read_csv(here::here("data", "battle_release.csv")) |> 
  mutate(time_released = as.character(time_released)) |> 
  glimpse()

