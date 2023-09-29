# clean_data.R
library(tidyverse)
library(janitor)
library(lubridate)

# catch
# catch_early <- read.csv(here::here("data", "catch_early.csv")) |> glimpse() # no longer needed thanks to historical db
catch_late <- read.csv(here::here("data", "catch_late.csv")) |> glimpse()
catch_historical <- read.csv(here::here("data", "catch_historical.csv")) |> glimpse()

# to convert subsample column
frac_to_decimal <- function(x) {
  new_vals <- sapply(x, function(x) eval(parse(text = x)))
  return(new_vals)
}

catch <- bind_rows(catch_historical |> 
                     filter(sample_date < min(catch_late$sample_date, na.rm = T)),
                   catch_late |> 
                     mutate(subsample = ifelse(subsample %in% c("", "not provided"), NA, subsample),
                            subsample = as.character(frac_to_decimal(subsample)))) |> 
  filter(!is.na(sample_date)) |> 
  select(-run) |> 
  glimpse()

# trap
# trap_early <- read.csv(here::here("data", "trap_early.csv")) |> glimpse()
trap_late <- read.csv(here::here("data", "trap_late.csv")) |> glimpse()
trap_historical <- read.csv(here::here("data", "trap_historical.csv")) |> glimpse()

trap <- bind_rows(trap_historical, trap_late) |>
  select(-c(lunar_phase, ubc_site)) |> 
  mutate(thalweg = case_when(thalweg == "Y" ~ TRUE, 
                             thalweg == "N" ~ FALSE,
                             thalweg %in% c("", "R") ~ NA),
         trap_fishing = ifelse(trap_fishing == 1, TRUE, FALSE),
         partial_sample = ifelse(partial_sample == 1, TRUE, FALSE),
         baileys_efficiency = NA_real_) |> # placeholder for official trap efficiency
  glimpse()



# recapture and release ---------------------------------------------------
# TODO link release sites to trap sites

# recapture - clear
recapture_clear <- read_csv(here::here("data", "clear_recapture.csv")) |> 
  # filter(year(date_recaptured) >= "2020") |> 
  glimpse()

# recapture - battle
recapture_battle <- read_csv(here::here("data", "battle_recapture.csv")) |> 
  # filter(year(date_recaptured) >= "2020") |> 
  glimpse()

recapture <- bind_rows(recapture_clear, recapture_battle) |> 
  relocate(c(release_id, date_recaptured, release_site, site), 
           .before = number_recaptured) |> 
  #mutate(subsite = NA_character_) |> # to distinguish between LCC/UCC for Clear Creek and RM 8.3/8.4 for UBC
  glimpse()

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
  relocate(c(release_id, date_released, time_released, release_site), 
           .before = site) |> # reorder columns
  # mutate(subsite = NA_character_, # to distinguish between LCC/UCC in Clear Creek and RM 8.3/8.4 in UBC
  #        run = NA_character_) |> 
  glimpse()

# SacPAS daily passage summary
# passage_summary <- read.csv(here::here("data-raw", "BOR Daily Passage and FL.csv")) |> 
#   janitor::clean_names() |> 
#   select(date, station_code, common_name, fws_run, brood_year, passage,
#          minimum_fl, maximum_fl, discharge_volume_cfs, water_temperature_c, 
#          water_turbidity_ntu) |> 
#   glimpse()


# write full datasets -----------------------------------------------------

write.csv(catch, here::here("data", "catch.csv"), row.names = FALSE)
write.csv(trap, here::here("data", "trap.csv"), row.names = FALSE)
write.csv(recapture, here::here("data", "recapture.csv"), row.names = FALSE)
write.csv(release, here::here("data", "release.csv"), row.names = FALSE)
# write.csv(passage_summary, here::here("data", "passage_summary.csv"), row.names = FALSE)


# read and glimpse --------------------------------------------------------

catch <- read_csv(here::here("data", "catch.csv")) |> glimpse()
trap <- read.csv(here::here("data", "trap.csv")) |> glimpse()
recapture <- read.csv(here::here("data", "recapture.csv")) |> glimpse()
release <- read.csv(here::here("data", "release.csv")) |> glimpse()
passage_summary <- read.csv(here::here("data", "passage_summary.csv")) |> glimpse()
