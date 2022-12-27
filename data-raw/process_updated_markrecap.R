# mark recapture tables

library(tidyverse)
library(lubridate)
library(googleCloudStorageR)
library(ggplot2)
library(scales)

# Mark recapture data for battle creek
# Timeframe 2003-2022


# read in data ------------------------------------------------------------
raw_mark_recapture <- readxl::read_excel(here::here("data-raw", "scripts_and_data_from_natasha",
                                                    "Mark-Recap Database MASTER BC.xlsx"), 
                                         sheet = 2, skip  = 2) %>% glimpse()
mark_recapture <- raw_mark_recapture %>% 
  janitor::clean_names() %>% 
  filter(release_date != "No Mark/Recap Studies for 2014-2015 Season") %>%
  mutate(release_date = janitor::excel_numeric_to_date(as.numeric(as.character(release_date)), date_system = "modern")) %>%
  glimpse()

