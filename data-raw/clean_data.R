# clean_data.R
library(tidyverse)
library(janitor)
library(lubridate)

# TODO decide which catch to use
# catch
catch_early <- read.csv(here::here("data", "catch_final.csv")) |> glimpse()
catch_late <- read.csv(here::here("data", "catch_2022.csv")) |> glimpse()

catch <- bind_rows(catch_early |> 
                     filter(date < min(catch_late$date)),
                   catch_late) |> 
  rename(run = race, fws_run = fws_race) |> 
  glimpse()

# trap
trap_early <- read.csv(here::here("data", "trap_final.csv")) |> glimpse()
trap_late <- read.csv(here::here("data", "trap_2022.csv")) |> glimpse()

trap <- bind_rows(trap_early, trap_late) |>
  select(-c(lunar_phase)) |> 
  glimpse()



# recapture and release ---------------------------------------------------
# TODO update date filter once we have more catch/trap data
# TODO link release sites to trap sites

# recapture - clear
recapture_clear <- read_csv(here::here("data", "clear_recapture.csv")) |> 
  filter(year(date_recaptured) >= "2020") |> 
  glimpse()

# recapture - battle
recapture_battle <- read_csv(here::here("data", "battle_recapture.csv")) |> 
  filter(year(date_recaptured) >= "2020") |> 
  glimpse()

recapture <- bind_rows(recapture_clear, recapture_battle) |> 
  glimpse()

# release - clear
release_clear <- read_csv(here::here("data", "clear_release.csv")) |> 
  filter(year(date_released) >= "2020") |> 
  mutate(time_released = as.character(time_released)) |> 
  glimpse()

# release - battle
release_battle <- read_csv(here::here("data", "battle_release.csv")) |> 
  filter(year(date_released) >= "2020") |> 
  mutate(time_released = as.character(time_released)) |> 
  glimpse()

release <- bind_rows(release_clear, release_battle) |> 
  glimpse()


# write full datasets -----------------------------------------------------

write.csv(catch, here::here("data", "catch.csv"), row.names = FALSE)
write.csv(trap, here::here("data", "trap.csv"), row.names = FALSE)
write.csv(recapture, here::here("data", "recapture.csv"), row.names = FALSE)
write.csv(release, here::here("data", "release.csv"), row.names = FALSE)


# read and glimpse --------------------------------------------------------

catch <- read_csv(here::here("data", "catch.csv")) |> glimpse()
trap <- read.csv(here::here("data", "trap.csv")) |> glimpse()
recapture <- read.csv(here::here("data", "recapture.csv")) |> glimpse()
release <- read.csv(here::here("data", "release.csv")) |> glimpse()
