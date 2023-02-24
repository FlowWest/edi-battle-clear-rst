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

# compare two catch tables
catch_2022 |> summarise(range(date)) # 2022-10-01 : 2023-01-05
catch |> summarise(range(sample_date)) # 2022-10-01 : 2022-11-21

# catch 2022 does not have an explicit catch date - it's the FLTS variable

# trap
# TODO confirm this is the table we should use
trap_2022 <- read_csv(here::here("data", "trap_2022.csv")) |> 
  mutate(sample_time = as.character(sample_time)) |> 
  glimpse()

trap <- read_csv(here::here("data", "trap.csv")) |>
  glimpse()

# compare two catch tables
trap_2022 |> summarise(range(sample_date)) # 2022-10-01 : 2023-01-05
trap |> summarise(range(sample_date)) # 2022-10-01 : 2022-11-21


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
