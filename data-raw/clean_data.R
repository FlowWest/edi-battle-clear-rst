# clean_data.R
library(tidyverse)
library(janitor)
library(lubridate)

# TODO decide which catch to use
# catch
catch <- read.csv(here::here("data", "catch_final.csv")) |> glimpse()

# trap
trap <- read.csv(here::here("data", "trap_final.csv")) |> glimpse()


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

