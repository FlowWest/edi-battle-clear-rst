# clean_data.R
library(tidyverse)

# catch
catch <- read_csv(here::here("data", "catch.csv")) |> 
  mutate(sample_time = as.character(sample_time)) |> 
  glimpse()

# trap
trap <- read_csv(here::here("data", "trap.csv")) |> 
  glimpse()

# recapture - clear
recapture_clear <- read_csv(here::here("data", "clear_recapture.csv")) |> 
  glimpse()

# recapture - battle
recapture_battle <- read_csv(here::here("data", "battle_recapture.csv")) |> 
  glimpse()

# release - clear
release_clear <- read_csv(here::here("data", "clear_release.csv")) |> 
  glimpse()

# release - battle
release_battle <- read_csv(here::here("data", "battle_release.csv")) |> 
  glimpse()
