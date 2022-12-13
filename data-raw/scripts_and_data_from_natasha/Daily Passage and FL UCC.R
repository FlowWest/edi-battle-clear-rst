
# This code will calculate the daily catch, passage, fork length range,
# weekly and brood year upper and lower 90% confidence limits 
# for the upper Clear Creek rotary screw trap site
# Use the BOR Passage query to obtain the needed catch data set.
# Use the BOR SID query to obtain the turbidity, and mark-recapture data set
# The data created by this code will be used for the BOR biweekly reports
# The flow and temperature data will be scraped from CDEC
# Create the SampleID.xls spreadsheet
# Created: Mike Schraml 02/08/2022


# Load needed Packages
if (!require("readxl")) {            # for importing data sets
  install.packages("readxl")
  library(readxl)
}

if (!require("tidyverse")) {            # Data management
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("lubridate")) {          # for handling dates
  install.packages("lubridate")
  library(lubridate)
}


# Set brood year, calendar year, Julian dates and strata
by1 <- 2021
by2 <- 2022
w1 <- 13
w2 <- 14
sid1 <- 85
sid2 <- 98

# Change brood year when needed
r.names.a <- c("Date", "BY21 RBT")
r.names.b <- c("Date", "BY22 RBT")
l.names.a <- c("Date", "BY21 Late-fall")
l.names.b <- c("Date", "BY22 Late-fall")
w.names.a <- c("Date", "BY21 Winter")
w.names.b <- c("Date", "BY22 Winter")
s.names <- c("Date", "BY21 Spring")

# Change brood year when needed
l.names.fl.a <- c("Date", "BY21 Late-Fall minimum FL", "BY21 Late-fall maximum FL")
l.names.fl.b <- c("Date", "BY22 Late-Fall minimum FL", "BY22 Late-fall maximum FL")
w.names.fl.a <- c("Date", "BY21 Winter minimum FL", "BY21 Winter maximum FL")
w.names.fl.b <- c("Date", "BY22 Winter minimum FL", "BY22 Winter maximum FL")
s.names.fl <- c("Date", "BY21 Spring minimum FL", "BY21 Spring maximum FL")
r.names.fla <- c("Date", "BY21 RBT FL", "BY21 RBT maximum FL")
r.names.flb <- c("Date", "BY22 RBT minimum FL", "BY22 RBT maximum FL")

# Load needed data sets
# Load data sets
cat <- read.csv("BOR UCC.csv")
sid <-  read.csv("UCC SampleID.csv")

# rename SampleDate
names(cat)[3] <- 'Date'
names(sid)[2] <- 'Date'

# Change RCatch to a double
cat$RCatch <- as.double(cat$RCatch)
                  
is.double(cat$RCatch)

# separate the SampleID
cat1 <- cat %>% separate(SampleID, c("jdate","year"), "_")

# Subset for the two week period
cat1 <- cat1 %>% filter(between(jdate, sid1, sid2))

# Turn the character string into a number
cat1$jdate <- as.numeric(cat1$jdate)
cat1$year <- as.numeric(cat1$year)

cat1$Date <- as.Date(as.POSIXct(cat1$Date, format = "%m/%d/%Y"))
sid$Date <- as.Date(as.POSIXct(sid$Date, format = "%m/%d/%Y"))

################################## Data Prep #################################################$$$$
# select the data you need 
# Filter for race and brood year
rbt.1 <- cat1 %>% filter(BroodYear == by1, OrganismCode == "RBT")
rbt.2 <- cat1 %>% filter(BroodYear == by2, OrganismCode == "RBT")
lfcs.1 <- cat1 %>% filter(BroodYear == by1, FWSRace == "L")
lfcs.2 <- cat1 %>% filter(BroodYear == by2, FWSRace == "L")
wcs.1 <- cat1 %>% filter(BroodYear == by1, FWSRace == "W")
wcs.2 <- cat1 %>% filter(BroodYear == by2, FWSRace == "W")
scs <- cat1 %>% filter(BroodYear == by1, FWSRace == "S")

############################# Daily passage ###################################

# Group by trap, race and  and sum the catch data
rbt.a <- rbt.1 %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

rbt.b <- rbt.2 %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

lfcs.a <- lfcs.1 %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

lfcs.b <- lfcs.2 %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

wcs.a <- wcs.1 %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

wcs.b <- wcs.2 %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

scs.a <- scs %>%
  arrange(jdate) %>%
  group_by(jdate) %>%
  summarise(catch = sum(RCatch))

# Merge the dates back in
rbt.a <- merge(sid, rbt.a, all=T)
rbt.b <- merge(sid, rbt.b, all=T)
lfcs.a <- merge(sid, lfcs.a, all=T)
lfcs.b <- merge(sid, lfcs.b, all=T)
wcs.a <- merge(sid, wcs.a, all=T)
wcs.b <- merge(sid, wcs.b, all=T)
scs.a <- merge(sid, scs.a, all=T)

# Get rid of the NAs
rbt.a <- rbt.a %>%
  mutate(catch = if_else(is.na(catch), 0, catch, BaileysEff)) %>%
  select(SampleID, Date, catch ,BaileysEff)

rbt.b <- rbt.b %>%
  mutate(catch = if_else(is.na(catch), 0, catch, BaileysEff)) %>%
  select(SampleID, Date, catch, BaileysEff)

lfcs.a <- lfcs.a %>%
  mutate(catch = if_else(is.na(catch), 0, catch)) %>%
  select(SampleID, Date, catch, BaileysEff)

lfcs.b <- lfcs.b %>%
  mutate(catch = if_else(is.na(catch), 0, catch)) %>%
  select(SampleID, Date, catch, BaileysEff)

wcs.a <- wcs.a %>%
  mutate(catch = if_else(is.na(catch), 0, catch, BaileysEff)) %>%
  select(SampleID, Date, catch, BaileysEff)

wcs.b <- wcs.b %>%
  mutate(catch = if_else(is.na(catch), 0, catch, BaileysEff)) %>%
  select(SampleID, Date, catch, BaileysEff)

scs.a <- scs.a %>%
  mutate(catch = if_else(is.na(catch), 0, catch, BaileysEff)) %>%
  select(SampleID, Date, catch,  BaileysEff)

# Calculate daily passage
rbt.pa <- rbt.a %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage)

rbt.pb <- rbt.b %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage)

lfcs.pa <- lfcs.a %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage)

lfcs.pb <- lfcs.b %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage)

wcs.pa <- wcs.a %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage)

wcs.pb <- wcs.b %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage) 

scs.p <- scs.a %>%
  mutate(passage = round(catch / BaileysEff), 0) %>%
  select(SampleID, Date, passage) 

# Select the the columns needed
lfcs.pa <- lfcs.pa %>%
  select(Date, passage)

lfcs.pb <- lfcs.pb %>%
  select(Date, passage)

wcs.pa <- wcs.pa %>%
  select(Date, passage)

wcs.pb <- wcs.pb %>%
  select(Date, passage)

scs.p <- scs.p %>%
  select(Date, passage)

rbt.pa <- rbt.pa %>%
  select(Date, passage)

rbt.pb <- rbt.pb %>%
  select(Date, passage)

# Remane headers
names(rbt.pa) <- r.names.a
names(rbt.pb) <- r.names.b
names(lfcs.pa) <- l.names.a
names(lfcs.pb) <- l.names.b
names(wcs.pa) <- w.names.a
names(wcs.pb) <- w.names.b
names(scs.p) <- s.names

# Merge data frames
passage <- merge(wcs.pa, wcs.pb, all=T)
passage <- merge(passage, scs.p, all=T)
passage <- merge(passage, lfcs.pa, all=T)
passage <- merge(passage, lfcs.pb, all=T)
passage <- merge(passage, rbt.pa, all=T)
passage <- merge(passage, rbt.pb, all=T)


############################## Fork Lengths #########################################

# select the data you need 
cat1.fl <- as.data.frame(select(cat1, Date, BroodYear, IDYear, FWSRace, ForkLength, OrganismCode))

# subset data sets for fish with fork length data (remove zero fork lengths)
cat1.fl <- subset(cat1.fl, ForkLength != 0)

# Filter for brood year and race
rbt.1 <- cat1.fl %>% filter(BroodYear == by1, OrganismCode == "RBT")
rbt.2 <- cat1.fl %>% filter(BroodYear == by2, OrganismCode == "RBT")
lfcs.1 <- cat1.fl %>% filter(BroodYear == by1, FWSRace == "L")
lfcs.2 <- cat1.fl %>% filter(BroodYear == by2, FWSRace == "L")
wcs.1 <- cat1.fl %>% filter(BroodYear == by1, FWSRace == "W")
wcs.2 <- cat1.fl %>% filter(BroodYear == by2, FWSRace == "W")
scs <- cat1.fl %>% filter(BroodYear == by1, FWSRace == "S")

# Merge sid and fl data frames
fl.wa <- merge(sid, wcs.1, all=T)
fl.wb <- merge(sid, wcs.2, all=T)
fl.s <- merge(sid, scs, all=T)
fl.lfa <- merge(sid, lfcs.1, all=T)
fl.lfb <- merge(sid, lfcs.2, all=T)
rbt.fla <- merge(sid, rbt.1, all=T)
rbt.flb <- merge(sid, rbt.2, all=T)

# Daily minimum and maximum fork lengths
fl.wa <- fl.wa %>%
  group_by (Date) %>%
  summarise(minimum = min(ForkLength), maximum = max(ForkLength))

fl.wb <- fl.wb %>%
  group_by (Date) %>%
  summarise(minimum = min(ForkLength), maximum = max(ForkLength))

fl.s <- fl.s %>%
  group_by (Date) %>%
  summarise(minimum = min(ForkLength), maximum = max(ForkLength))

fl.lfa <- fl.lfa %>%
  group_by (Date) %>%
  summarise(minimum = min(ForkLength), maximum = max(ForkLength))

fl.lfb <- fl.lfb %>%
  group_by (Date) %>%
  summarise(minimum = min(ForkLength), maximum = max(ForkLength))

rbt.fla <- rbt.fla %>%
  group_by (Date) %>%
  summarise(minimum = min(ForkLength), maximum = max(ForkLength))

rbt.flb <- rbt.flb %>%
  group_by (Date) %>%
    summarise(minimum = min(ForkLength), maximum = max(ForkLength))

# merge with sid
fl.wa <- merge(sid, fl.wa, all=T)
fl.wb <- merge(sid, fl.wb, all=T)
fl.s <- merge(sid, fl.s, all=T)
fl.lfa <- merge(sid, fl.lfa, all=T)
fl.lfb <- merge(sid, fl.lfb, all=T)
rbt.fla <- merge(sid, rbt.fla, all=T)
rbt.flb <- merge(sid, rbt.flb, all=T)

# Select the columns you want
fl.wa <- fl.wa %>% select(Date, minimum, maximum)
fl.wb <- fl.wb %>% select(Date, minimum, maximum)
fl.s <- fl.s %>% select(Date, minimum, maximum)
fl.lfa <- fl.lfa %>% select(Date, minimum, maximum)
fl.lfb <- fl.lfb %>% select(Date, minimum, maximum)
rbt.fla <- rbt.fla %>% select(Date, minimum, maximum)
rbt.flb <- rbt.flb %>% select(Date, minimum, maximum)

# Rename headers
names(fl.wa) <- w.names.fl.a
names(fl.wb) <- w.names.fl.b
names(fl.s) <- s.names.fl
names(fl.lfa) <- l.names.fl.a
names(fl.lfb) <- l.names.fl.b
names(rbt.fla) <- r.names.fla
names(rbt.flb) <- r.names.flb

# Merge fork length data frames
fork <- merge(fl.wa, fl.wb, all=T)
fork <- merge(fork, fl.s, all=T)
fork <- merge(fork, fl.lfa, all=T)
fork <- merge(fork, fl.lfb, all=T)
fork <- merge(fork, rbt.fla, all=T)
fork <- merge(fork, rbt.flb, all=T)

# Remove NAs in Date
fork <- fork %>% drop_na(Date)

fork$Date <- as.Date(as.POSIXct(fork$Date, format = "%m/%d/%Y"))

# Merge fork length and passage data frames
fork.p <- merge(passage, fork, all=T)

fork.p$Date <- as.Date(as.POSIXct(fork.p$Date, format = "%m/%d/%Y"))

################ Igo hourly water temperatures ################
# Laod needed data 
temp <- read.csv("CLEAR CREEK NEAR IGO (temp).csv")
flow <- read.csv("CLEAR CREEK NEAR IGO (flow).csv")

#  Give the columns better names
names(temp)[1] <- 'Date'
names(temp)[2] <- 'nothing'
names(temp)[3] <- 'Temperature'

names(flow)[1] <- 'Date'
names(flow)[2] <- 'nothing'
names(flow)[3] <- 'cfs'

# loose the second column
temp <- temp %>% select(-(nothing))
flow <- flow %>% select(-(nothing))

# Clean up missing data
temp$Temperature <- ifelse(temp$Temperature == "--", NA, temp$Temperature)
flow$cfs <- ifelse(flow$cfs == "--", NA, flow$cfs)

# Remove NAs
temp <- na.omit(temp)
flow <- na.omit(flow)

# Calculate peak daily flow (discharge) at Igo
# Strip time off the date
flow$Date <- as.Date(as.POSIXct(temp$Date, format = "%m/%d/%Y"))
temp$Date <- as.Date(as.POSIXct(temp$Date, format = "%m/%d/%Y"))

# Calculate daily maximum flow (discharge) at Igo
pf <- flow %>% group_by(Date) %>%
  summarise(maximum = max(cfs))

# Change temperature to numeric
temp$Temperature <- as.numeric(temp$Temperature)

# Calculate daily maximum flow (discharge) at Igo
mdt <- temp %>% group_by(Date) %>%
  summarise(mean = round(mean(Temperature), 1))

# Convert to Celsius
mdt <- mdt %>% mutate(mean = round((mean - 32) * (5/9), 1))


################################ Turbidity ################################
# Slect the culumns you need
turb <- sid %>% select(Date, Turbidity)

turb$Date <- as.Date(as.POSIXct(turb$Date, format = "%m/%d/%Y"))

################################### Merge data frames #######################
# Merge passage, fork length, and temp.flow data frames
temp.flow <- merge(pf, mdt, all=T)
temp.flow <- merge(temp.flow, turb, all=T)
t.f.p.fl <-merge(temp.flow, fork.p)

# Get names that correspond to main stem
names(t.f.p.fl)[2] <- 'Discharge volumn (cfs)'
names(t.f.p.fl)[3] <- 'Water temperature (C)'
names(t.f.p.fl)[4] <- 'Water turbidity (NTU)'

# Run this Code to output the Excel file
write.csv(t.f.p.fl, "C:/Users/mschraml/Desktop/ROutput/BOR Daily Passage and FL UCC.csv", row.names = FALSE)







