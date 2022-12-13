
# This code will calculate daily catch, cumulative catch, and percent cumulative catch
# for brood years at the upper Clear Creek rotary screw trap. The data comes from 4 queries
# Report Data CHN, Report Data WCS, Report Data LFFCS, and Report Data RBT
# Mike Schraml 02/02/2022



# Load needed Packages
if (!require("readxl")) {            # for importing data sets
  install.packages("readxl")
  library(readxl)
}

if (!require("tidyverse")) {            # data management
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("openxlsx")) {         # for writing xlsx files to the desk top
  install.packages("openxlsx")
  library(openxlsx)
}

if (!require("padr")) {         # for for inserting missing dates
  install.packages("padr")
  library(padr)
}

if (!require("lubridate")) {         # for for dealing with dates
  install.packages("lubridate")
  library(lubridate)
}

# Load data sets
chn1 <- read_excel("C:/Users/mschraml/Desktop/R Studio Projects/Report Data/UCC Report Data CHN.xlsx", 
                                  col_types = c("text", "text", "date", 
                                                "numeric", "text", "numeric", "numeric", 
                                                "text", "text", "text", "numeric", 
                                                "numeric", "numeric", "text", "text", 
                                                "numeric", "numeric"))

lfc1 <- read_excel("C:/Users/mschraml/Desktop/R Studio Projects/Report Data/UBC Report Data LFCS.xlsx", 
                  col_types = c("text", "text", "date", 
                                "numeric", "text", "numeric", "numeric", 
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "text", "text", 
                                "numeric", "numeric"))

wcs1 <- read_excel("C:/Users/mschraml/Desktop/R Studio Projects/Report Data/UBC Report Data WCS.xlsx", 
                  col_types = c("text", "text", "date", 
                                "numeric", "text", "numeric", "numeric", 
                                "text", "text", "text", "numeric", 
                                "numeric", "numeric", "text", "text", 
                                "numeric", "numeric"))


rbt1 <- read_excel("C:/Users/mschraml/Desktop/R Studio Projects/Report Data/UBC Report Data RBT.xlsx", 
                  col_types = c("text", "text", "date", 
                                "numeric", "text", "numeric", "numeric", 
                                "text", "text", "numeric", "numeric", 
                                "numeric", "text", "text", "numeric", 
                                "numeric"))

# Set the brood year value
by <- 2019

# Filter  for brood year
chn <- filter(chn1, BroodYear == by)
lfc <- filter(lfc1, BroodYear == by)
rbt <- filter(rbt1, BroodYear == by)
wcs <- filter(wcs1, BroodYear == by)

# Filter out the interpolated data
chn <- filter(chn, Interp == "NO")
lfc <- filter(lfc, Interp == "NO")
rbt <- filter(rbt, Interp == "NO")
wcs <- filter(wcs, Interp == "NO")

# Filter late-fall, fall, winter and spring-run into data sets
wcs <- wcs %>% filter(FWSRace == "W")
scs <- chn %>% filter(FWSRace == "S")
fcs <- chn %>% filter(FWSRace == "F")
lfc <- lfc %>% filter(FWSRace == "L")

# Get rid of unneeded columns
rbt <- select(rbt, SampleDate, BroodYear, RCatch)
lfc <- select(lfc, SampleDate, BroodYear, FWSRace, RCatch)
wcs <- select(wcs, SampleDate, BroodYear, FWSRace, RCatch)
scs <- select(scs, SampleDate, BroodYear, FWSRace, RCatch)

# Insert missing dates and set new dates RCatch to zero
rbt <- pad(rbt, interval = "day",
           start_val = lubridate::ymd("2019-01-01"),
           end_val = lubridate::ymd("2020-06-30")) %>%
  fill_by_value(RCatch, value = 0)

lfc <- pad(lfc, interval = "day",
           start_val = lubridate::ymd("2019-04-01"),
           end_val = lubridate::ymd("2020-03-31")) %>%
  fill_by_value(RCatch, value = 0)

wcs <- pad(wcs, interval = "day",
           start_val = lubridate::ymd("2019-07-01"),
           end_val = lubridate::ymd("2020-06-30")) %>%
  fill_by_value(RCatch, value = 0)

scs <- pad(scs, interval = "day",
           start_val = lubridate::ymd("2019-10-01"),
           end_val = lubridate::ymd("2020-09-30")) %>%
  fill_by_value(RCatch, value = 0)

# Change FWSRace NAs to S, W or L for days no fish were captured, so that the dates are not lost when filtering
wcs$FWSRace[wcs$RCatch >-1] <- "W"
scs$FWSRace[scs$RCatch >-1] <- "S"
lfc$FWSRace[lfc$RCatch >-1] <- "L"

## Calculate Daily Catch, this will be used for the cumulative and % cumulative calculations.
# Select needed data, order by SampleID, group by SampleID, sum daily catches
 wcs <- wcs %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

scs <- scs %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

lfc <- lfc %>% arrange(SampleDate) %>%
  filter(FWSRace == "L") %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

rbt <- rbt %>% arrange(SampleDate) %>%
  select(SampleDate, RCatch) %>%
  group_by(SampleDate) %>%
  summarise(catch = sum(RCatch))

##################### Calculate cumulative catches #####################


# calculate the cumulative daily catch
scs <- mutate(scs, cum.scs = cumsum(catch))
wcs <- mutate(wcs, cum.wcs = cumsum(catch))
lfc <- mutate(lfc, cum.lfc = cumsum(catch))
rbt <- mutate(rbt, cum.rbt = cumsum(catch))

# Find total catch
t.catch.s <- sum(scs$catch)
t.catch.w <- sum(wcs$catch)
t.catch.l <- sum(lfc$catch)
t.catch.rbt <- sum(rbt$catch)

# calculate the cumulative percent of daily catch and round to 0.1
scs <- mutate(scs, scs.pdc = round(cum.scs/t.catch.s * 100, 1))
wcs <- mutate(wcs, wcs.pdc = round(cum.wcs/t.catch.w * 100, 1))
lfc <- mutate(lfc, lfc.pdc = round(cum.lfc/t.catch.l * 100, 1))
rbt <- mutate(rbt, rbt.pdc = round(cum.rbt/t.catch.rbt * 100, 1))

# Combine data sets
chn.cum.p <-cbind.data.frame(scs$SampleDate, scs$catch, scs$cum.scs, scs$scs.pdc)


# Rename columns
colnames(chn.cum.p) <- c("Date", "SCS catch", "SCS Cumulative", "SCS % cumulative")
colnames(wcs) <- c("Date", "WCS catch", "WCS Cumulative", "WCS % cumulative")
colnames(lfc) <- c("Date", "LFCS catch", "LFCS Cumulative", "LFCS % cumulative")
colnames(rbt) <- c("Date", "RBT catch", "RBT Cumulative", "RBT % cumulative")

# First create list of the data frames and the worksheet names to be written to desk top
cum.catch <- list("RBT Cumulative" = rbt, "lfcS Cumulative" = lfc, "WCS Cumulative" = wcs, "SCS Cumulative" = chn.cum.p)

# Run this Code to output the Excel file
write.xlsx(cum.catch, "C:/Users/mschraml/Desktop/ROutput/UCC Cumulative Catch.xlsx")













