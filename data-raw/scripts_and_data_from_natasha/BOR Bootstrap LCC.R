
# This code will calculate the biweekly and brood year
# passage indices (to date), 95% confidence limits and standard error.
# At the lower Clear Creek RST site.
# Notes:  For this to run correctly the IDWeek must be a character and be from 01 to 52.
# Data will come from the current year's database
# Use the following query BOR Passage
# Mike Schraml, 05/04/2022

# Load needed Packages
if (!require("readxl")) {            # for importing data sets
  install.packages("readxl")
  library(readxl)
}

if (!require("tidyverse")) {            # for data manipulations and summaries
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("openxlsx")) {         # for writing xlsx files to the desk top
  install.packages("openxlsx")
  library(openxlsx)
}

# Load data sets
catch <- read.csv("BOR Data.csv")
#catch<- read.csv("C:/Users/nwingerter/OneDrive - DOI/Desktop/BOR Biweekly Reports/2022 Weeks 13-14/BOR Data.csv")

# Change IDWeeks 1-9 to IDWeeks 01-09
catch$IDWeek <- ifelse (catch$IDWeek=="1", "01",
                          ifelse (catch$IDWeek=="2", "02",
                                  ifelse (catch$IDWeek=="3", "03",
                                          ifelse (catch$IDWeek=="4", "04",
                                                  ifelse (catch$IDWeek=="5", "05",
                                                          ifelse (catch$IDWeek=="6", "06",
                                                                  ifelse (catch$IDWeek=="7", "07",
                                                                          ifelse (catch$IDWeek=="8", "08",
                                                                                  ifelse (catch$IDWeek=="9", "09",catch$IDWeek)))))))))


#  What is the current working  directory
getwd()

# Find library directory
.libPaths()

by1 <- 2021
by2 <- 2022
w1 <- 14
w2 <- 15

# Create a data frame for the biweekly passage and confidence limits
biweekly <- data.frame(matrix(NA, nrow = 3, ncol = 9))
names(biweekly) <- c( "Biweekly run", "Winter-run Chinook Salmon 21", "Winter-run Chinook Salmon 22", "Spring-run Chinook Salmon 21",
                      "Fall-run Chinook Salmon 21", "Late-fall run Chinook Salmon 21", "Late-fall run Chinook Salmon 22",
                      "RainbowTrout/steelhead 21", "RainbowTrout/steelhead 22")

biweekly [1,1] <- "Passage index"
biweekly [2,1] <- "90% LCL"
biweekly [3,1] <- "90% UCL"

# Create a data frame for the brood year passage and confidence limits
brood.year <- data.frame(matrix(NA, nrow = 3, ncol = 9))
names(brood.year) <- c("Brood year run", "Winter-run Chinook Salmon 21", "Winter-run Chinook Salmon 22", "Spring-run Chinook Salmon 21",
                       "Fall-run Chinook Salmon 21", "Late-fall run Chinook Salmon 21", "Late-fall run Chinook Salmon 22",
                       "RainbowTrout/steelhead 21", "RainbowTrout/steelhead 22")

brood.year [1,1] <- "Passage index"
brood.year [2,1] <- "90% LCL"
brood.year [3,1] <- "90% UCL"

# Select for only LCC data
catch <- catch %>% filter(StationCode == "LCC")

# Filter for brood year in case you forgot to in the queries
rbt.1 <- catch %>% filter(BroodYear == by1, OrganismCode == "RBT")
rbt.2 <- catch %>% filter(BroodYear == by2, OrganismCode == "RBT")
lfcs.1 <- catch %>% filter(BroodYear == by1, FWSRace == "L")
lfcs.2 <- catch %>% filter(BroodYear == by2, FWSRace == "L")
wcs.1 <- catch %>% filter(BroodYear == by1, FWSRace == "W")
wcs.2 <- catch %>% filter(BroodYear == by2, FWSRace == "W")
scs.1 <- catch %>% filter(BroodYear == by1, FWSRace == "S")
fcs.1 <- catch %>% filter(BroodYear == by1, FWSRace == "F")

# Break catches into the weeks you want
rbt.1a <- rbt.1 %>% filter(IDWeek == w1)
rbt.1b <- rbt.1 %>% filter(IDWeek == w2)
rbt.2a <- rbt.2 %>% filter(IDWeek == w1)
rbt.2b <- rbt.2 %>% filter(IDWeek == w2)

lfcs.1a <- lfcs.1 %>% filter(IDWeek == w1)
lfcs.1b <- lfcs.1 %>% filter(IDWeek == w2)
lfcs.2a <- lfcs.2 %>% filter(IDWeek == w1)
lfcs.2b <- lfcs.2 %>% filter(IDWeek == w2)

wcs.1a <- wcs.1 %>% filter(IDWeek == w1)
wcs.1b <- wcs.1 %>% filter(IDWeek == w2)
wcs.2a <- wcs.2 %>% filter(IDWeek == w1)
wcs.2b <- wcs.2 %>% filter(IDWeek == w2)

scs.a <- scs.1 %>% filter(IDWeek == w1)
scs.b <- scs.1 %>% filter(IDWeek == w2)

fcs.a <- fcs.1 %>% filter(IDWeek == w1)
fcs.b <- fcs.1 %>% filter(IDWeek == w2)

# Bind them back for the two-week period
rbt.21 <- rbind(rbt.1a, rbt.1b)
rbt.22 <- rbind(rbt.2a, rbt.2b)
lfcs.21 <- rbind(lfcs.1a, lfcs.1b)
lfcs.22 <- rbind(lfcs.2a, lfcs.2b)
wcs.21 <- rbind(wcs.1a, wcs.1b)
wcs.22 <- rbind(wcs.2a, wcs.2b)
scs.21 <- rbind(scs.a, scs.b)
fcs.21 <- rbind(fcs.a, fcs.b)


######################################################## Biweekly Bootstraps

#####################################################################################
################### Rainbow Trout/steelhead first brood year ########################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
rbt.1c <- mutate(rbt.21,
                 strata.week = paste(rbt.21$IDWeek, rbt.21$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
rbt.eff.1 <- select(rbt.1c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
rbt.strataeff.1 <- unique(rbt.eff.1)

# Order strataeff by strata
rbt.strataeff.1 <- arrange(rbt.strataeff.1, strata.week)

############# Calculate strata catches #############

# Group data by strata
rbt.1c <- group_by(rbt.1c, strata.week)

# Summarize catch data by strata 
rbt.1c <- summarise(rbt.1c, 
                    w.catch = sum(RCatch))

# Order catch data by strata
rbt.1c <- arrange(rbt.1c, strata.week)

# Combine efficiency and catch data sets
rbt.1c <- left_join(rbt.1c, rbt.strataeff.1, by = c('strata.week'))


############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
rbt.p1 <- mutate(rbt.1c,
                 rbt.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrrbt.1 <- nrow(rbt.p1)

# Create a data frame for confidence interval and standard error results of the loop
rbt.int.1 <- data.frame(matrix(NA, nrow=nrrbt.1, ncol=5))
names(rbt.int.1) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
rbt.unordered.boots.1 <- data.frame(matrix(NA, nrow=1000, ncol=nrrbt.1))
names(rbt.unordered.boots.1) <- c(rbt.p1$strata.week)


########################################### Rainbow Trout/steelhead first brood year Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Rainbow Trout/steelhead first brood year
for (i in 1:nrrbt.1) {
  
  ############## Calculate Rainbow Trout/steelhead first brood year bootstraps ##############
  
  # set variables
  ST <- rbt.p1[i,1]                        # Get get name of strata
  WC <- as.numeric(rbt.p1[i,2])            # Get catch for strata
  BE <- as.numeric(signif(rbt.p1[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(rbt.p1[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  rbt.int.1[i, 1] <- rbt.p1$strata.week[i]            # The strata name
  rbt.int.1[i, 2] <- round(lcl.90)                    # Lower 950 confidence limit
  rbt.int.1[i, 3] <- rbt.p1$rbt.pass[i]               # The weekly passage
  rbt.int.1[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  rbt.int.1[i, 5] <- round(se,2)                      # Standard error
  
    # Input the strata bootstrap data into the unordered.boots data frame created above
  rbt.unordered.boots.1[,i] <- Weekly.unordered.boot
  
}

########################################### End  Rainbow Trout/steelhead first brood year Bootstrap Loop ##################################################

# Sum across rows of the Boots data
rbt.sum.boots.1 <- rowSums(rbt.unordered.boots.1, na.rm = TRUE)
rbt.sum.boots.1

rbt.Alcl.90.1 <- quantile(rbt.sum.boots.1, 0.050)
rbt.Aucl.90.1 <- quantile(rbt.sum.boots.1, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
rbt.Ordered.Boots.1 <- sort(rbt.sum.boots.1)
rbt.Alcl.90a.1 <- rbt.Ordered.Boots.1[50]
rbt.Aucl.90a.1 <- rbt.Ordered.Boots.1[950]

rbt.Alcl.90.1
rbt.Alcl.90a.1
rbt.Aucl.90.1
rbt.Aucl.90a.1

# Calculate biweekly passage Index
rbt.biweekly.passage.1 <- colSums(rbt.p1["rbt.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
rbt.se1 <- sd(rbt.sum.boots.1)/sqrt(1000)

############## Add data to biweekly Table ##############R
biweekly[1, 8] <- rbt.biweekly.passage.1          # biweekly passage
biweekly[2, 8] <- round(rbt.Alcl.90a.1, 0)        # biweekly Lower 90% confidence limit
biweekly[3, 8] <- round(rbt.Aucl.90a.1, 0)        # biweekly Upper 90% confidence limit



#####################################################################################
################### Rainbow Trout/steelhead second brood year #######################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
rbt.2c <- mutate(rbt.22,
                  strata.week = paste(rbt.22$IDWeek, rbt.22$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
rbt.eff.2 <- select(rbt.2c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
rbt.strataeff.2 <- unique(rbt.eff.2)

# Order strataeff by strata
rbt.strataeff.2 <- arrange(rbt.strataeff.2, strata.week)

############ Calculate strata catches #############

# Group data by strata
rbt.2c <- group_by(rbt.2c, strata.week)

# Summarize catch data by strata 
rbt.2c <- summarise(rbt.2c, 
                     w.catch = sum(RCatch))

# Order catch data by strata
rbt.2c <- arrange(rbt.2c)

# Combine efficiency and catch data sets
rbt.2c <- left_join(rbt.2c, rbt.strataeff.2, by = c('strata.week'))

# Calculate biweekly passage indices
rbt.p2 <- mutate(rbt.2c,
                  rbt.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrrbt.2 <- nrow(rbt.2c)

# Create a data frame for confidence interval and standard error results of the loop
rbt.int.2 <- data.frame(matrix(NA, nrow=nrrbt.2, ncol=5))
names(rbt.int.2) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
rbt.unordered.boots.2 <- data.frame(matrix(NA, nrow=1000, ncol=nrrbt.2))
names(rbt.unordered.boots.2) <- c(rbt.p2$strata.week)

########################################### Rainbow Trout/steelhead second brood year Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Rainbow Trout/steelhead second brood year
for (i in 1:nrrbt.2) {

  ############## Calculate Rainbow Trout/steelhead second brood year bootstraps ##############
  
  # set variables
  ST <- rbt.p2[i,1]                        # Get get name of strata
  WC <- as.numeric(rbt.p2[i,2])            # Get catch for strata
  BE <- as.numeric(signif(rbt.p2[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(rbt.p2[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame (lfcs.int) created above
  rbt.int.2[i, 1] <- rbt.p2$strata.week[i]            # The strata name
  rbt.int.2[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  rbt.int.2[i, 3] <- rbt.p2$rbt.pass[i]               # The weekly passage
  rbt.int.2[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  rbt.int.2[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  rbt.unordered.boots.2[,i] <- Weekly.unordered.boot
  
}

########################################### End Rainbow Trout/steelhead second brood year Bootstrap Loop ##################################################

# Sum across rows of the Boots data
rbt.sum.boots.2 <- rowSums(rbt.unordered.boots.2, na.rm = TRUE)
rbt.sum.boots.2

rbt.Alcl.90.2 <- quantile(rbt.sum.boots.2, 0.050)
rbt.Aucl.90.2 <- quantile(rbt.sum.boots.2, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
rbt.Ordered.Boots.2 <- sort(rbt.sum.boots.2)
rbt.Alcl.90a.2 <- rbt.Ordered.Boots.2[50]
rbt.Aucl.90a.2 <- rbt.Ordered.Boots.2[950]

rbt.Alcl.90.2
rbt.Alcl.90a.2
rbt.Aucl.90.2
rbt.Aucl.90a.2

# Calculate the biweekly passage Index
rbt.biweekly.passage.2 <- colSums(rbt.p2["rbt.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
rbt.se2 <- sd(rbt.sum.boots.2)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 9] <- rbt.biweekly.passage.2           # biweekly passage
biweekly[2, 9] <- round(rbt.Alcl.90a.2, 0)         # biweekly Lower 90% confidence limit
biweekly[3, 9] <- round(rbt.Aucl.90a.2, 0)         # biweekly Upper 90% confidence limit



#####################################################################################
################### late-fall run Chinook Salmon first brood year ##################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
lfcs.1c <- mutate(lfcs.21,
                  strata.week = paste(lfcs.21$IDWeek, lfcs.21$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############
# Create a new a data frame with only the data below
lfcs.eff.1 <- select(lfcs.1c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
lfcs.strataeff.1 <- unique(lfcs.eff.1)

# Order strataeff by strata
lfcs.strataeff.1 <- arrange(lfcs.strataeff.1, strata.week)

############# Calculate strata catches #############

# Group data by strata
lfcs.1c <- group_by(lfcs.1c, strata.week)

# Summarize catch data by strata 
lfcs.1c <- summarise(lfcs.1c, 
                     w.catch = sum(RCatch))

# Order catch data by strata
lfcs.1c <- arrange(lfcs.1c, strata.week)

# Combine efficiency and catch data sets
lfcs.1c <- left_join(lfcs.1c, lfcs.strataeff.1, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
lfcs.p1 <- mutate(lfcs.1c,
                  lfcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrlfcs.1 <- nrow(lfcs.p1)

# Create a data frame for confidence interval and standard error results of the loop
lfcs.int.1 <- data.frame(matrix(NA, nrow=nrlfcs.1, ncol=5))
names(lfcs.int.1) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
lfcs.unordered.boots.1 <- data.frame(matrix(NA, nrow=1000, ncol=nrlfcs.1))
names(lfcs.unordered.boots.1) <- c(lfcs.p1$strata.week)

########################################### late-fall run Chinook Salmon second brood year Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for late-fall run Chinook Salmon second brood year
for (i in 1:nrlfcs.1) {
  
    ############## Calculate late-fall run Chinook Salmon second brood year bootstraps ##############
  
  # set variables
  ST <- lfcs.p1[i,1]                        # Get get name of strata
  WC <- as.numeric(lfcs.p1[i,2])            # Get catch for strata
  BE <- as.numeric(signif(lfcs.p1[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(lfcs.p1[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame (lfcs.int) created above
  lfcs.int.1[i, 1] <- lfcs.p1$strata.week[i]           # The strata name
  lfcs.int.1[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  lfcs.int.1[i, 3] <- lfcs.p1$lfcs.pass[i]             # The weekly passage
  lfcs.int.1[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  lfcs.int.1[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  lfcs.unordered.boots.2[,i] <- Weekly.unordered.boot
  
}


########################################### End late-fall run Chinook Salmon second brood year Bootstrap Loop ##################################################

# Sum across rows of the Boots data
lfcs.sum.boots.1 <- rowSums(lfcs.unordered.boots.1, na.rm = TRUE)
lfcs.sum.boots.1

lfcs.Alcl.90.1 <- quantile(lfcs.sum.boots.1, 0.050)
lfcs.Aucl.90.1 <- quantile(lfcs.sum.boots.1, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
lfcs.Ordered.Boots.1 <- sort(lfcs.sum.boots.1)
lfcs.Alcl.90a.1 <- lfcs.Ordered.Boots.1[50]
lfcs.Aucl.90a.1 <- lfcs.Ordered.Boots.1[950]

lfcs.Alcl.90.1
lfcs.Alcl.90a.1
lfcs.Aucl.90.1
lfcs.Aucl.90a.1

# Calculate the biweekly passage Index
lfcs.biweekly.passage.1 <- colSums(lfcs.p1["lfcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
lfcs.se1 <- sd(lfcs.sum.boots.1)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 6] <- lfcs.biweekly.passage.1           # biweekly passage
biweekly[2, 6] <- round(lfcs.Alcl.90a.1, 0)         # biweekly Lower 90% confidence limit
biweekly[3, 6] <- round(lfcs.Aucl.90a.1, 0)         # biweekly Upper 90% confidence limit



#####################################################################################
################### late-fall run Chinook Salmon second brood year ##################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
lfcs.2c <- mutate(lfcs.22,
                     strata.week = paste(lfcs.22$IDWeek, lfcs.22$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
lfcs.eff.2 <- select(lfcs.2c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
lfcs.strataeff.2 <- unique(lfcs.eff.2)

# Order strataeff by strata
lfcs.strataeff.2 <- arrange(lfcs.strataeff.2, strata.week)

############# Calculate strata catches #############

# Group data by strata
lfcs.2c <- group_by(lfcs.2c, strata.week)

# Summarize catch data by strata 
lfcs.2c <- summarise(lfcs.2c, 
                        w.catch = sum(RCatch))

# Order catch data by strata
lfcs.2c <- arrange(lfcs.2c, strata.week)

# Combine efficiency and catch data sets
lfcs.2c <- left_join(lfcs.2c, lfcs.strataeff.2, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
lfcs.p2 <- mutate(lfcs.2c,
                       lfcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrlfcs.2 <- nrow(lfcs.p2)

# Create a data frame for confidence interval and standard error results of the loop
lfcs.int.2 <- data.frame(matrix(NA, nrow=nrlfcs.2, ncol=5))
names(lfcs.int.2) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
lfcs.unordered.boots.2 <- data.frame(matrix(NA, nrow=1000, ncol=nrlfcs.2))
names(lfcs.unordered.boots.2) <- c(lfcs.p2$strata.week)

########################################### late-fall run Chinook Salmon second brood year Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for late-fall run Chinook Salmon second brood year
for (i in 1:nrlfcs.2) {
  
    ############## Calculate late-fall run Chinook Salmon second brood year bootstraps ##############
  
  # set variables
  ST <- lfcs.p2[i,1]                        # Get get name of strata
  WC <- as.numeric(lfcs.p2[i,2])            # Get catch for strata
  BE <- as.numeric(signif(lfcs.p2[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(lfcs.p2[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame (lfcs.int) created above
  lfcs.int.2[i, 1] <- lfcs.p2$strata.week[i]           # The strata name
  lfcs.int.2[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  lfcs.int.2[i, 3] <- lfcs.p2$lfcs.pass[i]             # The weekly passage
  lfcs.int.2[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  lfcs.int.2[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  lfcs.unordered.boots.2[,i] <- Weekly.unordered.boot
  
}

########################################### End late-fall run Chinook Salmon second brood year Bootstrap Loop ##################################################


# Sum across rows of the Boots data
lfcs.sum.boots.2 <- rowSums(lfcs.unordered.boots.2, na.rm = TRUE)
lfcs.sum.boots.2

lfcs.Alcl.90.2 <- quantile(lfcs.sum.boots.2, 0.050)
lfcs.Aucl.90.2 <- quantile(lfcs.sum.boots.2, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
lfcs.Ordered.Boots.2 <- sort(lfcs.sum.boots.2)
lfcs.Alcl.90a.2 <- lfcs.Ordered.Boots.2[50]
lfcs.Aucl.90a.2 <- lfcs.Ordered.Boots.2[950]

lfcs.Alcl.90.2
lfcs.Alcl.90a.2
lfcs.Aucl.90.2
lfcs.Aucl.90a.2

############## Calculate the total passage index ##############

# Calculate the biweekly passage Index
lfcs.biweekly.passage.2 <- colSums(lfcs.p2["lfcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
lfcs.se2 <- sd(lfcs.sum.boots.2)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 7] <- lfcs.biweekly.passage.2            # biweekly passage
biweekly[2, 7] <- round(lfcs.Alcl.90a.2, 0)          # biweekly Lower 90% confidence limit
biweekly[3, 7] <- round(lfcs.Aucl.90a.2, 0)          # biweekly Upper 90% confidence limit



#####################################################################################
################### Winter-run Chinook Salmon first brood year ######################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
wcs.1c <- mutate(wcs.21,
                  strata.week = paste(wcs.21$IDWeek, wcs.21$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
wcs.eff.1 <- select(wcs.1c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
wcs.strataeff.1 <- unique(wcs.eff.1)

# Order strataeff by strata
wcs.strataeff.1 <- arrange(wcs.strataeff.1, strata.week)

############# Calculate strata catches #############

# Group data by strata
wcs.1c <- group_by(wcs.1c, strata.week)

# Summarize catch data by strata 
wcs.1c <- summarise(wcs.1c, 
                     w.catch = sum(RCatch))

# Order catch data by strata
wcs.1c <- arrange(wcs.1c, strata.week)

# Combine efficiency and catch data sets
wcs.1c <- left_join(wcs.1c, wcs.strataeff.1, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
wcs.p1 <- mutate(wcs.1c,
                  wcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrwcs.1 <- nrow(wcs.p1)

# Create a data frame for confidence interval and standard error results of the loop
wcs.int.1 <- data.frame(matrix(NA, nrow=nrwcs.1, ncol=5))
names(wcs.int.1) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
wcs.unordered.boots.1 <- data.frame(matrix(NA, nrow=1000, ncol=nrwcs.1))
names(wcs.unordered.boots.1) <- c(wcs.p1$strata.week)

########################################### Winter-run Chinook Salmon first brood year Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Winter-run Chinook Salmon first brood year
for (i in 1:nrwcs.1) {
  
    ############## Calculate Winter-run Chinook Salmon first brood year bootstraps ##############
  
  # set variables
  ST <- wcs.p1[i,1]                        # Get get name of strata
  WC <- as.numeric(wcs.p1[i,2])            # Get catch for strata
  BE <- as.numeric(signif(wcs.p1[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(wcs.p1[i,4])            # Get number of fish released for strata
  
   # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame (lfcs.int) created above
  wcs.int.1[i, 1] <- wcs.p1$strata.week[i]            # The strata name
  wcs.int.1[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  wcs.int.1[i, 3] <- wcs.p1$wcs.pass[i]               # The weekly passage
  wcs.int.1[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  wcs.int.1[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  wcs.unordered.boots.1[,i] <- Weekly.unordered.boot
  
}


########################################### End Winter-run Chinook Salmon first brood year Bootstrap Loop ##################################################

# Sum across rows of the Boots data
wcs.sum.boots.1 <- rowSums(wcs.unordered.boots.1, na.rm = TRUE)
wcs.sum.boots.1

wcs.Alcl.90.1 <- quantile(wcs.sum.boots.1, 0.050)
wcs.Aucl.90.1 <- quantile(wcs.sum.boots.1, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
wcs.Ordered.Boots.1 <- sort(wcs.sum.boots.1)
wcs.Alcl.90a.1 <- wcs.Ordered.Boots.1[50]
wcs.Aucl.90a.1 <- wcs.Ordered.Boots.1[950]

wcs.Alcl.90.1
wcs.Alcl.90a.1
wcs.Aucl.90.1
wcs.Aucl.90a.1

# Calculate the biweekly passage Index
wcs.biweekly.passage.1 <- colSums(wcs.p1["wcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
wcs.se1 <- sd(wcs.sum.boots.1)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 2] <- wcs.biweekly.passage.1            # biweekly passage
biweekly[2, 2] <- round(wcs.Alcl.90a.1, 0)          # biweekly Lower 90% confidence limit
biweekly[3, 2] <- round(wcs.Aucl.90a.1, 0)          # biweekly Upper 90% confidence limit



#####################################################################################
################### Winter-run Chinook Salmon second brood year #####################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
wcs.2c <- mutate(wcs.22,
                 strata.week = paste(wcs.22$IDWeek, wcs.22$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
wcs.eff.2 <- select(wcs.2c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
wcs.strataeff.2 <- unique(wcs.eff.2)

# Order strataeff by strata
wcs.strataeff.2 <- arrange(wcs.strataeff.2, strata.week)

############# Calculate strata catches #############

# Group data by strata
wcs.2c <- group_by(wcs.2c, strata.week)

# Summarize catch data by strata 
wcs.2c <- summarise(wcs.2c, 
                    w.catch = sum(RCatch))

# Order catch data by strata
wcs.2c <- arrange(wcs.2c, strata.week)

# Combine efficiency and catch data sets
wcs.2c <- left_join(wcs.2c, wcs.strataeff.2, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
wcs.p2 <- mutate(wcs.2c,
                 wcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrwcs.2 <- nrow(wcs.p2)

# Create a data frame for confidence interval and standard error results of the loop
wcs.int.2 <- data.frame(matrix(NA, nrow=nrwcs.2, ncol=5))
names(wcs.int.2) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
wcs.unordered.boots.2 <- data.frame(matrix(NA, nrow=1000, ncol=nrwcs.2))
names(wcs.unordered.boots.2) <- c(wcs.p2$strata.week)


########################################## Winter-run Chinook Salmon second brood year Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Winter-run Chinook Salmon second brood year
for (i in 1:nrwcs.2) {
  
  ############## Calculate Winter-run Chinook Salmon second brood year bootstraps ##############
  
  # set variables
  ST <- wcs.p2[i,1]                        # Get get name of strata
  WC <- as.numeric(wcs.p2[i,2])            # Get catch for strata
  BE <- as.numeric(signif(wcs.p2[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(wcs.p2[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  wcs.int.2[i, 1] <- wcs.p2$strata.week[i]            # The strata name
  wcs.int.2[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  wcs.int.2[i, 3] <- wcs.p2$wcs.pass[i]               # The weekly passage
  wcs.int.2[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  wcs.int.2[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  wcs.unordered.boots.2[,i] <- Weekly.unordered.boot
  
}


########################################### End Winter-run Chinook Salmon second brood year Bootstrap Loop ##################################################


# Sum across rows of the Boots data
wcs.sum.boots.2 <- rowSums(wcs.unordered.boots.2, na.rm = TRUE)
wcs.sum.boots.2

wcs.Alcl.90.2 <- quantile(wcs.sum.boots.2, 0.050)
wcs.Aucl.90.2 <- quantile(wcs.sum.boots.2, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
wcs.Ordered.Boots.2 <- sort(wcs.sum.boots.2)
wcs.Alcl.90a.2 <- wcs.Ordered.Boots.2[50]
wcs.Aucl.90a.2 <- wcs.Ordered.Boots.2[950]

wcs.Alcl.90.2
wcs.Alcl.90a.2
wcs.Aucl.90.2
wcs.Aucl.90a.2

############## Calculate the total passage index ##############

# Calculate the biweekly passage Index
wcs.biweekly.passage.2 <- colSums(wcs.p2["wcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
wcs.se2 <- sd(wcs.sum.boots.2)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 3] <- wcs.biweekly.passage.2          # biweekly passage
biweekly[2, 3] <- round(wcs.Alcl.90a.2, 0)        # biweekly Lower 90% confidence limit
biweekly[3, 3] <- round(wcs.Aucl.90a.2, 0)        # biweekly Upper 90% confidence limit



#####################################################################################
######################### Spring-run Chinook Salmon #################################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
scs.c <- mutate(scs.21,
                 strata.week = paste(scs.21$IDWeek, scs.21$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
scs.eff <- select(scs.c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
scs.strataeff <- unique(scs.eff)

# Order strataeff by strata
scs.strataeff <- arrange(scs.strataeff, strata.week)

############# Calculate strata catches #############

# Group data by strata
scs.c <- group_by(scs.c, strata.week)

# Summarize catch data by strata 
scs.c <- summarise(scs.c, 
                    w.catch = sum(RCatch))

# Order catch data by strata
scs.c <- arrange(scs.c, strata.week)

# Combine efficiency and catch data sets
scs.c <- left_join(scs.c, scs.strataeff, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
scs.p <- mutate(scs.c,
                 scs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrscs <- nrow(scs.p)

# Create a data frame for confidence interval and standard error results of the loop
scs.int <- data.frame(matrix(NA, nrow=nrscs, ncol=5))
names(scs.int) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
scs.unordered.boots <- data.frame(matrix(NA, nrow=1000, ncol=nrscs))
names(scs.unordered.boots) <- c(scs.p$strata.week)


########################################### Spring-run Chinook Salmon Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Spring-run Chinook Salmon
for (i in 1:nrscs) {
  
  ############## Calculate Spring-run Chinook Salmon bootstraps ##############
  
  # set variables
  ST <- scs.p[i,1]                        # Get get name of strata
  WC <- as.numeric(scs.p[i,2])            # Get catch for strata
  BE <- as.numeric(signif(scs.p[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(scs.p[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  scs.int[i, 1] <- scs.p$strata.week[i]             # The strata name
  scs.int[i, 2] <- round(lcl.90)                    # Lower 90%confidence limit
  scs.int[i, 3] <- scs.p$scs.pass[i]                # The weekly passage
  scs.int[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  scs.int[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  scs.unordered.boots[,i] <- Weekly.unordered.boot
  
}


########################################### End Spring-run Chinook Salmon Bootstrap Loop ##################################################


# Sum across rows of the Boots data
scs.sum.boots <- rowSums(scs.unordered.boots, na.rm = TRUE)
scs.sum.boots

scs.Alcl.90 <- quantile(scs.sum.boots, 0.050)
scs.Aucl.90 <- quantile(scs.sum.boots, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
scs.Ordered.Boots <- sort(scs.sum.boots)
scs.Alcl.90a <- scs.Ordered.Boots[50]
scs.Aucl.90a <- scs.Ordered.Boots[950]

scs.Alcl.90
scs.Alcl.90a
scs.Aucl.90
scs.Aucl.90a

# Calculate the biweeklypassage Index
scs.biweekly.passage <- colSums(scs.p["scs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
scs.se <- sd(scs.sum.boots)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 4] <- scs.biweekly.passage          # biweekly passage
biweekly[2, 4] <- round(scs.Alcl.90a, 0)        # biweekly Lower 90% confidence limit
biweekly[3, 4] <- round(scs.Aucl.90a, 0)        # biweekly Upper 90% confidence limit



#####################################################################################
######################### Fall-run Chinook Salmon ###################################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
fcs.c <- mutate(fcs.21,
                strata.week = paste(fcs.21$IDWeek, fcs.21$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
fcs.eff <- select(fcs.c, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
fcs.strataeff <- unique(fcs.eff)

# Order strataeff by strata
fcs.strataeff <- arrange(fcs.strataeff, strata.week)

############# Calculate strata catches #############

# Group data by strata
fcs.c <- group_by(fcs.c, strata.week)

# Summarize catch data by strata 
fcs.c <- summarise(fcs.c, 
                   w.catch = sum(RCatch))

# Order catch data by strata
fcs.c <- arrange(fcs.c, strata.week)

# Combine efficiency and catch data sets
fcs.c <- left_join(fcs.c, fcs.strataeff, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
fcs.p <- mutate(fcs.c,
                fcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data and assign to i
nrfcs <- nrow(fcs.p)

# Create a data frame for confidence interval and standard error results of the loop
fcs.int <- data.frame(matrix(NA, nrow=nrfcs, ncol=5))
names(fcs.int) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
fcs.unordered.boots <- data.frame(matrix(NA, nrow=1000, ncol=nrfcs))
names(fcs.unordered.boots) <- c(fcs.p$strata.week)


########################################### Fall-run Chinook Salmon Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Fall-run Chinook Salmon
for (i in 1:nrfcs) {
  
  ############## Calculate Fall-run Chinook Salmon bootstraps ##############
  
  # set variables
  ST <- fcs.p[i,1]                        # Get get name of strata
  WC <- as.numeric(fcs.p[i,2])            # Get catch for strata
  BE <- as.numeric(signif(fcs.p[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(fcs.p[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  fcs.int[i, 1] <- fcs.p$strata.week[i]             # The strata name
  fcs.int[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  fcs.int[i, 3] <- fcs.p$fcs.pass[i]                # The weekly passage
  fcs.int[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  fcs.int[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  fcs.unordered.boots[,i] <- Weekly.unordered.boot
  
}


########################################### End Fall-run Chinook Salmon Bootstrap Loop ##################################################

# Sum across rows of the Boots data
fcs.sum.boots <- rowSums(fcs.unordered.boots, na.rm = TRUE)
fcs.sum.boots

fcs.Alcl.90 <- quantile(fcs.sum.boots, 0.050)
fcs.Aucl.90 <- quantile(fcs.sum.boots, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
fcs.Ordered.Boots <- sort(fcs.sum.boots)
fcs.Alcl.90a <- fcs.Ordered.Boots[50]
fcs.Aucl.90a <- fcs.Ordered.Boots[950]

fcs.Alcl.90
fcs.Alcl.90a
fcs.Aucl.90
fcs.Aucl.90a

############## Calculate the total passage index ##############

# Calculate the biweekly passage Index
fcs.biweekly.passage <- colSums(fcs.p["fcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
fcs.se <- sd(fcs.sum.boots)/sqrt(1000)

############## Add data to biweekly Table ##############
biweekly[1, 5] <- fcs.biweekly.passage            # biweekly passage
biweekly[2, 5] <- round(fcs.Alcl.90a, 0)          # biweekly Lower 90% confidence limit
biweekly[3, 5] <- round(fcs.Aucl.90a, 0)          # biweekly Upper 90% confidence limit



######################################################## Brood year Bootstraps



#####################################################################################
################### Rainbow Trout/steelhead first brood year BY ########################
#####################################################################################
# Combine IDWeek and SubWeek into one new column
rbt.1d <- mutate(rbt.1,
                 strata.week = paste(rbt.1$IDWeek, rbt.1$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
rbt.eff.d <- select(rbt.1d, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
rbt.strataeff.d <- unique(rbt.eff.d)

# Order strataeff by strata
rbt.strataeff.d <- arrange(rbt.strataeff.d, strata.week)

############# Calculate strata catches #############

# Group data by strata
rbt.1d <- group_by(rbt.1d, strata.week)

# Summarize catch data by strata 
rbt.1d <- summarise(rbt.1d, 
                    w.catch = sum(RCatch))

# Order catch data by strata
rbt.1d <- arrange(rbt.1d, strata.week)

# Combine efficiency and catch data sets
rbt.1d <- left_join(rbt.1d, rbt.strataeff.d, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
rbt.pd <- mutate(rbt.1d,
                 rbt.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrrbt.d <- nrow(rbt.pd)

# Create a data frame for confidence interval and standard error results of the loop
rbt.int.d <- data.frame(matrix(NA, nrow=nrrbt.d, ncol=5))
names(rbt.int.d) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
rbt.unordered.boots.d <- data.frame(matrix(NA, nrow=1000, ncol=nrrbt.d))
names(rbt.unordered.boots.d) <- c(rbt.pd$strata.week)


########################################### Rainbow Trout/steelhead first brood year BY Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Rainbow Trout/steelhead first brood year
for (i in 1:nrrbt.d) {
  
  ############## Calculate Rainbow Trout/steelhead first brood year bootstraps ##############
  
  # set variables
  ST <- rbt.pd[i,1]                        # Get get name of strata
  WC <- as.numeric(rbt.pd[i,2])            # Get catch for strata
  BE <- as.numeric(signif(rbt.pd[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(rbt.pd[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  rbt.int.d[i, 1] <- rbt.pd$strata.week[i]            # The strata name
  rbt.int.d[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  rbt.int.d[i, 3] <- rbt.pd$rbt.pass[i]               # The weekly passage
  rbt.int.d[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  rbt.int.d[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  rbt.unordered.boots.d[,i] <- Weekly.unordered.boot
  
}

########################################### End Rainbow Trout/steelhead first brood year BY Bootstrap Loop ##################################################


# Sum across rows of the Boots data
rbt.sum.boots.d <- rowSums(rbt.unordered.boots.d, na.rm = TRUE)
rbt.sum.boots.d

rbt.Alcl.90.d <- quantile(rbt.sum.boots.d, 0.050)
rbt.Aucl.90.d <- quantile(rbt.sum.boots.d, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
rbt.Ordered.Boots.d <- sort(rbt.sum.boots.d)
rbt.Alcl.90a.d <- rbt.Ordered.Boots.d[50]
rbt.Aucl.90a.d <- rbt.Ordered.Boots.d[950]

rbt.Alcl.90.d
rbt.Alcl.90a.d
rbt.Aucl.90.d
rbt.Aucl.90a.d

# Calculate the brood year passage Index
rbt.brood.passage.d <- colSums(rbt.pd["rbt.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
rbt.sed <- sd(rbt.sum.boots.d)/sqrt(1000)

############## Add Rainbow Trout/steelhead first brood year data to brood year table  ##############
brood.year[1, 8] <- rbt.brood.passage.d               # brood year passage
brood.year[2, 8] <- round(rbt.Alcl.90a.d, 0)          # brood year Lower 90% confidence limit
brood.year[3, 8] <- round(rbt.Aucl.90a.d, 0)          # brood year Upper 90% confidence limit



#####################################################################################
################### Rainbow Trout/steelhead second brood year BY ########################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
rbt.2e <- mutate(rbt.2,
                 strata.week = paste(rbt.2$IDWeek, rbt.2$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
rbt.eff.e <- select(rbt.2e, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
rbt.strataeff.e <- unique(rbt.eff.e)

# Order strataeff by strata
rbt.strataeff.e <- arrange(rbt.strataeff.e, strata.week)

############# Calculate strata catches #############

# Group data by strata
rbt.2e <- group_by(rbt.2e, strata.week)

# Summarize catch data by strata 
rbt.2e <- summarise(rbt.2e, 
                    w.catch = sum(RCatch))

# Order catch data by strata
rbt.2e <- arrange(rbt.2e, strata.week)

# Combine efficiency and catch data sets
rbt.2e <- left_join(rbt.2e, rbt.strataeff.e, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
rbt.pe <- mutate(rbt.2e,
                 rbt.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrrbt.e <- nrow(rbt.pe)

# Create a data frame for confidence interval and standard error results of the loop
rbt.int.e <- data.frame(matrix(NA, nrow=nrrbt.e, ncol=5))
names(rbt.int.e) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
rbt.unordered.boots.e <- data.frame(matrix(NA, nrow=1000, ncol=nrrbt.e))
names(rbt.unordered.boots.e) <- c(rbt.pe$strata.week)


########################################### Rainbow Trout/steelhead second brood year BY Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Rainbow Trout/steelhead second brood year BY
for (i in 1:nrrbt.e) {
  
  ############## Calculate Rainbow Trout/steelhead second brood year BY bootstraps ##############
  
  # set variables
  ST <- rbt.pe[i,1]                        # Get get name of strata
  WC <- as.numeric(rbt.pe[i,2])            # Get catch for strata
  BE <- as.numeric(signif(rbt.pe[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(rbt.pe[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  rbt.int.e[i, 1] <- rbt.pe$strata.week[i]            # The strata name
  rbt.int.e[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  rbt.int.e[i, 3] <- rbt.pe$rbt.pass[i]               # The weekly passage
  rbt.int.e[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  rbt.int.e[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  rbt.unordered.boots.e[,i] <- Weekly.unordered.boot
  
}

########################################### End Rainbow Trout/steelhead first brood year BY Bootstrap Loop ##################################################

# Sum across rows of the Boots data
rbt.sum.boots.e <- rowSums(rbt.unordered.boots.e, na.rm = TRUE)
rbt.sum.boots.e

rbt.Alcl.90.e <- quantile(rbt.sum.boots.e, 0.050)
rbt.Aucl.90.e <- quantile(rbt.sum.boots.e, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
rbt.Ordered.Boots.e <- sort(rbt.sum.boots.e)
rbt.Alcl.90a.e <- rbt.Ordered.Boots.e[50]
rbt.Aucl.90a.e <- rbt.Ordered.Boots.e[950]

rbt.Alcl.90.e
rbt.Alcl.90a.e
rbt.Aucl.90.e
rbt.Aucl.90a.e

# Calculate the brood year passage Index
rbt.brood.passage.e <- colSums(rbt.pe["rbt.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
rbt.see <- sd(rbt.sum.boots.e)/sqrt(1000)

############## Add Rainbow Trout/steelhead second brood year data to brood year table Table BY ##############
brood.year[1, 9] <- rbt.brood.passage.e             # brood year passage
brood.year[2, 9] <- round(rbt.Alcl.90a.e, 0)        # brood year Lower 90% confidence limit
brood.year[3, 9] <- round(rbt.Aucl.90a.e, 0)        # brood year Upper 90% confidence limit



#####################################################################################
################### late-fall run Chinook Salmon first brood year BY ##################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
lfcs.1d <- mutate(lfcs.1,
                  strata.week = paste(lfcs.1$IDWeek, lfcs.1$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
lfcs.eff.d <- select(lfcs.1d, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
lfcs.strataeff.d <- unique(lfcs.eff.d)

# Order strataeff by strata
lfcs.strataeff.d <- arrange(lfcs.strataeff.d, strata.week)

############# Calculate strata catches #############

# Group data by strata
lfcs.1d <- group_by(lfcs.1d, strata.week)

# Summarize catch data by strata 
lfcs.1d <- summarise(lfcs.1d, 
                     w.catch = sum(RCatch))

# Order catch data by strata
lfcs.1d <- arrange(lfcs.1d, strata.week)

# Combine efficiency and catch data sets
lfcs.1d <- left_join(lfcs.1d, lfcs.strataeff.d, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
lfcs.pd <- mutate(lfcs.1d,
                  lfcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrlfcs.d <- nrow(lfcs.pd)

# Create a data frame for confidence interval and standard error results of the loop
lfcs.int.d <- data.frame(matrix(NA, nrow=nrlfcs.d, ncol=5))
names(lfcs.int.d) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
lfcs.unordered.boots.d <- data.frame(matrix(NA, nrow=1000, ncol=nrlfcs.d))
names(lfcs.unordered.boots.d) <- c(lfcs.p1$strata.week)

########################################### late-fall run Chinook Salmon second brood year Bootstrap Loop BY ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for late-fall run Chinook Salmon second brood year
for (i in 1:nrlfcs.d) {
  
  ############## Calculate late-fall run Chinook Salmon second brood year bootstraps BY ##############
  
  # set variables
  ST <- lfcs.pd[i,1]                        # Get get name of strata
  WC <- as.numeric(lfcs.pd[i,2])            # Get catch for strata
  BE <- as.numeric(signif(lfcs.pd[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(lfcs.pd[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  lfcs.int.d[i, 1] <- lfcs.pd$strata.week[i]           # The strata name
  lfcs.int.d[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  lfcs.int.d[i, 3] <- lfcs.pd$lfcs.pass[i]             # The weekly passage
  lfcs.int.d[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  lfcs.int.d[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the lfcs.unordered.boots data frame created above
  lfcs.unordered.boots.2[,i] <- Weekly.unordered.boot
  
}


########################################### End late-fall run Chinook Salmon second brood year Bootstrap Loop BY ##################################################


# Sum across rows of the Boots data
lfcs.sum.boots.d <- rowSums(lfcs.unordered.boots.d, na.rm = TRUE)
lfcs.sum.boots.d

lfcs.Alcl.90.d <- quantile(lfcs.sum.boots.d, 0.050)
lfcs.Aucl.90.d <- quantile(lfcs.sum.boots.d, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
lfcs.Ordered.Boots.d <- sort(lfcs.sum.boots.d)
lfcs.Alcl.90a.d <- lfcs.Ordered.Boots.d[50]
lfcs.Aucl.90a.d <- lfcs.Ordered.Boots.d[950]

lfcs.Alcl.90.d
lfcs.Alcl.90a.d
lfcs.Aucl.90.d
lfcs.Aucl.90a.d

############## Calculate the total passage index ##############

# Calculate the brood year passage Index
lfcs.brood.passage.d <- colSums(lfcs.pd["lfcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
lfcs.sed <- sd(lfcs.sum.boots.d)/sqrt(1000)

############## Add Late-fall data to brood year Table ##############
brood.year[1, 6] <- lfcs.brood.passage.d             # brood year passage
brood.year[2, 6] <- round(lfcs.Alcl.90a.d, 0)        # brood year Lower 90% confidence limit
brood.year[3, 6] <- round(lfcs.Aucl.90a.d, 0)        # brood year Upper 90% confidence limit



#####################################################################################
################### late-fall run Chinook Salmon second brood year BY ##################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
lfcs.e <- mutate(lfcs.2,
                  strata.week = paste(lfcs.2$IDWeek, lfcs.2$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
lfcs.eff.e <- select(lfcs.e, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
lfcs.strataeff.e <- unique(lfcs.eff.e)

# Order strataeff by strata
lfcs.strataeff.e <- arrange(lfcs.strataeff.e, strata.week)

############# Calculate strata catches #############

# Group data by strata
lfcs.e <- group_by(lfcs.e, strata.week)

# Summarize catch data by strata 
lfcs.e <- summarise(lfcs.e, 
                     w.catch = sum(RCatch))

# Order catch data by strata
lfcs.e <- arrange(lfcs.e, strata.week)

# Combine efficiency and catch data sets
lfcs.e <- left_join(lfcs.e, lfcs.strataeff.e, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
lfcs.pe <- mutate(lfcs.e,
                  lfcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrlfcs.e <- nrow(lfcs.pe)

# Create a data frame for confidence interval and standard error results of the loop
lfcs.int.e <- data.frame(matrix(NA, nrow=nrlfcs.e, ncol=5))
names(lfcs.int.e) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
lfcs.unordered.boots.e <- data.frame(matrix(NA, nrow=1000, ncol=nrlfcs.e))
names(lfcs.unordered.boots.e) <- c(lfcs.pe$strata.week)



########################################### late-fall run Chinook Salmon second brood year Bootstrap Loop BY ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for late-fall run Chinook Salmon second brood year
for (i in 1:nrlfcs.e) {
  
  ############## Calculate late-fall run Chinook Salmon second brood year bootstraps BY ##############
  
  # set variables
  ST <- lfcs.pe[i,1]                        # Get get name of strata
  WC <- as.numeric(lfcs.pe[i,2])            # Get catch for strata
  BE <- as.numeric(signif(lfcs.pe[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(lfcs.pe[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  lfcs.int.e[i, 1] <- lfcs.pe$strata.week[i]           # The strata name
  lfcs.int.e[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  lfcs.int.e[i, 3] <- lfcs.pe$lfcs.pass[i]             # The weekly passage
  lfcs.int.e[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  lfcs.int.e[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the lfcs.unordered.boots data frame created above
  lfcs.unordered.boots.e[,i] <- Weekly.unordered.boot
  
}

########################################### End late-fall run Chinook Salmon second brood year Bootstrap Loop BY ##################################################


# Sum across rows of the Boots data
lfcs.sum.boots.e <- rowSums(lfcs.unordered.boots.e, na.rm = TRUE)
lfcs.sum.boots.e

lfcs.Alcl.90.e <- quantile(lfcs.sum.boots.e, 0.050)
lfcs.Aucl.90.e <- quantile(lfcs.sum.boots.e, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
lfcs.Ordered.Boots.e <- sort(lfcs.sum.boots.e)
lfcs.Alcl.90a.e <- lfcs.Ordered.Boots.e[50]
lfcs.Aucl.90a.e <- lfcs.Ordered.Boots.e[950]

lfcs.Alcl.90.e
lfcs.Alcl.90a.e
lfcs.Aucl.90.e
lfcs.Aucl.90a.e

# Calculate the brood year passage Index
lfcs.brood.passage.e <- colSums(lfcs.pe["lfcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
lfcs.see <- sd(lfcs.sum.boots.e)/sqrt(1000)

############## Add Late-fall data to brood year Table ##############
brood.year[1, 7] <- lfcs.brood.passage.e            # brood year passage
brood.year[2, 7] <- round(lfcs.Alcl.90a.e, 0)       # brood year Lower 90% confidence limit
brood.year[3, 7] <- round(lfcs.Aucl.90a.e, 0)       # brood year Upper 90% confidence limit



#####################################################################################
################### Winter-run Chinook Salmon first brood year BY ######################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
wcs.1d <- mutate(wcs.1,
                 strata.week = paste(wcs.1$IDWeek, wcs.1$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
wcs.eff.d <- select(wcs.1d, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
wcs.strataeff.d <- unique(wcs.eff.d)

# Order strataeff by strata
wcs.strataeff.d <- arrange(wcs.strataeff.d, strata.week)

############# Calculate strata catches #############

# Group data by strata
wcs.1d <- group_by(wcs.1d, strata.week)

# Summarize catch data by strata 
wcs.1d <- summarise(wcs.1d, 
                    w.catch = sum(RCatch))

# Order catch data by strata
wcs.1d <- arrange(wcs.1d, strata.week)

# Combine efficiency and catch data sets
wcs.1d <- left_join(wcs.1d, wcs.strataeff.d, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
wcs.pd <- mutate(wcs.1d,
                 wcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrwcs.d <- nrow(wcs.pd)

# Create a data frame for confidence interval and standard error results of the loop
wcs.int.d <- data.frame(matrix(NA, nrow=nrwcs.d, ncol=5))
names(wcs.int.d) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
wcs.unordered.boots.d <- data.frame(matrix(NA, nrow=1000, ncol=nrwcs.d))
names(wcs.unordered.boots.d) <- c(wcs.pd$strata.week)


########################################### Winter-run Chinook Salmon first brood year Bootstrap Loop BY ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Winter-run Chinook Salmon first brood year BY
for (i in 1:nrwcs.d) {
  
  ############## Calculate Winter-run Chinook Salmon first brood year bootstraps By ##############
  
  # set variables
  ST <- wcs.pd[i,1]                        # Get get name of strata
  WC <- as.numeric(wcs.pd[i,2])            # Get catch for strata
  BE <- as.numeric(signif(wcs.pd[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(wcs.pd[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame (lfcs.int) created above
  wcs.int.d[i, 1] <- wcs.pd$strata.week[i]            # The strata name
  wcs.int.d[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  wcs.int.d[i, 3] <- wcs.pd$wcs.pass[i]               # The weekly passage
  wcs.int.d[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  wcs.int.d[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  wcs.unordered.boots.d[,i] <- Weekly.unordered.boot
  
}


########################################### End Winter-run Chinook Salmon first brood year Bootstrap Loop BY ##################################################

# Sum across rows of the Boots data
wcs.sum.boots.d <- rowSums(wcs.unordered.boots.d, na.rm = TRUE)
wcs.sum.boots.d

wcs.Alcl.90.d <- quantile(wcs.sum.boots.d, 0.050)
wcs.Aucl.90.d <- quantile(wcs.sum.boots.d, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
wcs.Ordered.Boots.d <- sort(wcs.sum.boots.d)
wcs.Alcl.90a.d <- wcs.Ordered.Boots.d[50]
wcs.Aucl.90a.d <- wcs.Ordered.Boots.d[950]

wcs.Alcl.90.d
wcs.Alcl.90a.d
wcs.Aucl.90.d
wcs.Aucl.90a.d

# Calculate the brood year passage Index
wcs.brood.passage.d <- colSums(wcs.pd["wcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
wcs.sed <- sd(wcs.sum.boots.d)/sqrt(1000)

############## Add data to brood year Table ##############
brood.year[1, 2] <- wcs.brood.passage.d             # brood year passage
brood.year[2, 2] <- round(wcs.Alcl.90a.d, 0)        # brood year Lower 90% confidence limit
brood.year[3, 2] <- round(wcs.Aucl.90a.d, 0)        # brood year Upper 90% confidence limit



#####################################################################################
################### Winter-run Chinook Salmon second brood year BY #####################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
wcs.2e <- mutate(wcs.2,
                 strata.week = paste(wcs.2$IDWeek, wcs.2$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
wcs.eff.e <- select(wcs.2e, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
wcs.strataeff.e <- unique(wcs.eff.e)

# Order strataeff by strata
wcs.strataeff.e <- arrange(wcs.strataeff.e, strata.week)

############# Calculate strata catches #############

# Group data by strata
wcs.2e <- group_by(wcs.2e, strata.week)

# Summarize catch data by strata 
wcs.2e <- summarise(wcs.2e, 
                    w.catch = sum(RCatch))

# Order catch data by strata
wcs.2e <- arrange(wcs.2e, strata.week)

# Combine efficiency and catch data sets
wcs.2e <- left_join(wcs.2e, wcs.strataeff.e, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
wcs.pe <- mutate(wcs.2e,
                 wcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrwcs.e <- nrow(wcs.2e)

# Create a data frame for confidence interval and standard error results of the loop
wcs.int.e <- data.frame(matrix(NA, nrow=nrwcs.e, ncol=5))
names(wcs.int.e) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
wcs.unordered.boots.e <- data.frame(matrix(NA, nrow=1000, ncol=nrwcs.e))
names(wcs.unordered.boots.e) <- c(wcs.pe$strata.week)


########################################## Winter-run Chinook Salmon second brood year Bootstrap Loop BY ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Winter-run Chinook Salmon second brood year BY
for (i in 1:nrwcs.e) {
  
  ############## Calculate Winter-run Chinook Salmon second brood year bootstraps BY ##############
  
  # set variables
  ST <- wcs.pe[i,1]                        # Get get name of strata
  WC <- as.numeric(wcs.pe[i,2])            # Get catch for strata
  BE <- as.numeric(signif(wcs.pe[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(wcs.pe[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  wcs.int.e[i, 1] <- wcs.pe$strata.week[i]            # The strata name
  wcs.int.e[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  wcs.int.e[i, 3] <- wcs.pe$wcs.pass[i]               # The weekly passage
  wcs.int.e[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  wcs.int.e[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  wcs.unordered.boots.e[,i] <- Weekly.unordered.boot
  
}


########################################### End Winter-run Chinook Salmon second brood year Bootstrap Loop BY ##################################################

# Sum across rows of the Boots data
wcs.sum.boots.e <- rowSums(wcs.unordered.boots.e, na.rm = TRUE)
wcs.sum.boots.e

wcs.Alcl.90.e <- quantile(wcs.sum.boots.e, 0.050)
wcs.Aucl.90.e <- quantile(wcs.sum.boots.e, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
wcs.Ordered.Boots.e <- sort(wcs.sum.boots.e)
wcs.Alcl.90a.e <- wcs.Ordered.Boots.e[50]
wcs.Aucl.90a.e <- wcs.Ordered.Boots.e[950]

wcs.Alcl.90.e
wcs.Alcl.90a.e
wcs.Aucl.90.e
wcs.Aucl.90a.e

# Calculate the brood year passage Index
wcs.brood.passage.e <- colSums(wcs.pe["wcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
wcs.see <- sd(wcs.sum.boots.e)/sqrt(1000)

############## Add data to brood year Table ##############
brood.year[1, 3] <- wcs.brood.passage.e             # brood year passage
brood.year[2, 3] <- round(wcs.Alcl.90a.e, 0)        # brood year Lower 90% confidence limit
brood.year[3, 3] <- round(wcs.Aucl.90a.e, 0)        # brood year Upper 90% confidence limit



#####################################################################################
######################### Spring-run Chinook Salmon BY #################################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
scs.e <- mutate(scs.1,
                strata.week = paste(scs.1$IDWeek, scs.1$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
scs.eff1 <- select(scs.e, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
scs.strataeff1 <- unique(scs.eff1)

# Order strataeff by strata
scs.strataeff1 <- arrange(scs.strataeff1, strata.week)

############# Calculate strata catches #############

# Group data by strata
scs.e <- group_by(scs.e, strata.week)

# Summarize catch data by strata 
scs.e <- summarise(scs.e, 
                   w.catch = sum(RCatch))

# Order catch data by strata
scs.e <- arrange(scs.e, strata.week)

# Combine efficiency and catch data sets
scs.e <- left_join(scs.e, scs.strataeff1, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
scs.p1 <- mutate(scs.e,
                scs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data (lfcs.passage) and assign to i
nrscs1 <- nrow(scs.p1)

# Create a data frame for confidence interval and standard error results of the loop
scs.int1 <- data.frame(matrix(NA, nrow=nrscs1, ncol=5))
names(scs.int1) <- c("strata", "90% LCL", "passage", "90% UCL", "se")

# Create a data frame for bootstrapping results of the loop
scs.unordered.boots1 <- data.frame(matrix(NA, nrow=1000, ncol=nrscs1))
names(scs.unordered.boots1) <- c(scs.p1$strata.week)


########################################### Spring-run Chinook Salmon Bootstrap Loop BY ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Spring-run Chinook Salmon
for (i in 1:nrscs1) {
  
  ############## Calculate Spring-run Chinook Salmon bootstraps BY ##############
  
  # set variables
  ST <- scs.p1[i,1]                        # Get get name of strata
  WC <- as.numeric(scs.p1[i,2])            # Get catch for strata
  BE <- as.numeric(signif(scs.p1[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(scs.p1[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  scs.int1[i, 1] <- scs.p1$strata.week[i]            # The strata name
  scs.int1[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  scs.int1[i, 3] <- scs.p1$scs.pass[i]               # The weekly passage
  scs.int1[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  scs.int1[i, 5] <- round(se,2)                      # Standard error
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  scs.unordered.boots1[,i] <- Weekly.unordered.boot
  
}


########################################### End Spring-run Chinook Salmon Bootstrap Loop BY ##################################################


# Sum across rows of the Boots data
scs.sum.boots1 <- rowSums(scs.unordered.boots1, na.rm = TRUE)
scs.sum.boots1

scs.Alcl.90.1 <- quantile(scs.sum.boots1, 0.050)
scs.Aucl.90.1 <- quantile(scs.sum.boots1, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
scs.Ordered.Boots1 <- sort(scs.sum.boots1)
scs.Alcl.90a1 <- scs.Ordered.Boots1[50]
scs.Aucl.90a1 <- scs.Ordered.Boots1[950]

scs.Alcl.90.1
scs.Alcl.90a1
scs.Aucl.90.1
scs.Aucl.90a1

# Calculate the brood year passage Index
scs.brood.passage1 <- colSums(scs.p1["scs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
scs.se1 <- sd(scs.sum.boots1)/sqrt(1000)

############## Add data to brood year Table ##############
brood.year[1, 4] <- scs.brood.passage1             # brood year passage
brood.year[2, 4] <- round(scs.Alcl.90a1, 0)        #b rood year Lower 90% confidence limit
brood.year[3, 4] <- round(scs.Aucl.90a1, 0)        # brood year Upper 90% confidence limit



#####################################################################################
######################### Fall-run Chinook Salmon BY ###################################
#####################################################################################

# Combine IDWeek and SubWeek into one new column
fcs.e <- mutate(fcs.1,
                strata.week = paste(fcs.1$IDWeek, fcs.1$SubWeek))

############# Create a new a data frame with only data for efficiency to be added to the strata catch summary #############

# Create a new a data frame with only the data below
fcs.eff1 <- select(fcs.e, strata.week, BaileysEff, NumReleased)

# Get unique efficiency and strata values
fcs.strataeff1 <- unique(fcs.eff1)

# Order strataeff by strata
fcs.strataeff1 <- arrange(fcs.strataeff1, strata.week)

############# Calculate strata catches #############

# Group data by strata
fcs.e <- group_by(fcs.e, strata.week)

# Summarize catch data by strata 
fcs.e <- summarise(fcs.e, 
                   w.catch = sum(RCatch))

# Order catch data by strata
fcs.e <- arrange(fcs.e, strata.week)

# Combine efficiency and catch data sets
fcs.e <- left_join(fcs.e, fcs.strataeff1, by = c('strata.week'))

############## Calculate weekly passage indices ##############

# Calculate weekly passage indices and the passage data and round to the nearest fish
fcs.p1 <- mutate(fcs.e,
                fcs.pass = round (w.catch / BaileysEff))

# Find the number of strata (rows) in the catch and passage data and assign to i
nrfcs1 <- nrow(fcs.p1)

# Create a data frame for confidence interval and standard error results of the loop
fcs.int1 <- data.frame(matrix(NA, nrow=nrfcs1, ncol=5))
names(fcs.int1) <- c("strata", "90% LCL", "passage", "90% UCL")

# Create a data frame for bootstrapping results of the loop
fcs.unordered.boots1 <- data.frame(matrix(NA, nrow=1000, ncol=nrfcs1))
names(fcs.unordered.boots) <- c(fcs.p1$strata.week)


########################################### Fall-run Chinook Salmon Bootstrap Loop ##################################################
# set seed for random number generator
set.seed(2323)

# Reset the value for i
i <- 1

# Start loop to calculate 95% confidence limits for Fall-run Chinook Salmon
for (i in 1:nrfcs1) {
  
  ############## Calculate Fall-run Chinook Salmon bootstraps ##############
  
  # set variables
  ST <- fcs.p1[i,1]                        # Get get name of strata
  WC <- as.numeric(fcs.p1[i,2])            # Get catch for strata
  BE <- as.numeric(signif(fcs.p1[i,3], 4)) # Get efficiency for strata
  FR <- as.numeric(fcs.p1[i,4])            # Get number of fish released for strata
  
  # Create a vector of 1,000 strata catches 
  fish <- rep(WC, 1000)
  
  # Create a vector of 1,000 strata efficiencies 
  efficiency <- rep(BE, 1000)
  
  # This function generates required number (1,000) of random values (recaptures) of given probability 
  # (Bailey's efficiency) from a given sample.
  recapture <- round(rbinom(1000, FR, BE),  0)
  
  # passage data generated from the 1,000 random samples generated above
  Weekly.unordered.boot <- round(WC * (FR + 1)/(recapture + 1), 0)
  
  # Two ways to get at the upper and lower 95% confidence intervals
  
  # use the qauntile function
  lcl.90 <- quantile(Weekly.unordered.boot, .050)
  lcl.90
  ucl.90 <- quantile(Weekly.unordered.boot, .950)
  ucl.90
  
  # or sort the Boot.Bailey vector and select the 25th and 975th element of the vector 
  weekly.ordered.boot <- sort(Weekly.unordered.boot)
  lcl.90a <- weekly.ordered.boot[50]
  lcl.90a
  ucl.90a <- weekly.ordered.boot[950]
  ucl.90a
  
  # Calculate the standard error (standard deviation / square root of the number of instances)
  se <- sd(Weekly.unordered.boot)/sqrt(1000)
  
  # Input the weekly actual catch, total catch, 95% lower confidence limit,
  # passage, 95% upper confidence limit and standard error data
  # for the run into a the strata data frame created above
  fcs.int1[i, 1] <- fcs.p1$strata.week[i]            # The strata name
  fcs.int1[i, 2] <- round(lcl.90)                    # Lower 90% confidence limit
  fcs.int1[i, 3] <- fcs.p1$fcs.pass[i]               # The weekly passage
  fcs.int1[i, 4] <- round(ucl.90)                    # Upper 90% confidence limit
  fcs.int1[i, 5] <- round(se,2)                      # Standard error
  
  
  # Input the strata bootstrap data into the unordered.boots data frame created above
  fcs.unordered.boots1[,i] <- Weekly.unordered.boot
  
}


########################################### End Fall-run Chinook Salmon Bootstrap Loop ##################################################

# Sum across rows of the Boots data
fcs.sum.boots1 <- rowSums(fcs.unordered.boots1, na.rm = TRUE)
fcs.sum.boots1

fcs.Alcl.90.1 <- quantile(fcs.sum.boots1, 0.050)
fcs.Aucl.90.1 <- quantile(fcs.sum.boots1, 0.950)

# Sort the matrix this will get the confidence limits like the old way, off the excel spreadsheets
fcs.Ordered.Boots1 <- sort(fcs.sum.boots1)
fcs.Alcl.90a1 <- fcs.Ordered.Boots1[50]
fcs.Aucl.90a1 <- fcs.Ordered.Boots1[950]

fcs.Alcl.90.1
fcs.Alcl.90a1
fcs.Aucl.90.1
fcs.Aucl.90a1

############## Calculate the total passage index ##############

# Calculate the brood year passage Index
fcs.brood.passage1 <- colSums(fcs.p1["fcs.pass"])

# Calculate the standard error (standard deviation / square root of the number of instances)
fcs.se1 <- sd(fcs.sum.boots1)/sqrt(1000)

############## Add data to brood year Table ##############
brood.year[1, 5] <- fcs.brood.passage1             # Brood year passage
brood.year[2, 5] <- round(fcs.Alcl.90a1, 0)        # Brood year Lower 90% confidence limit
brood.year[3, 5] <- round(fcs.Aucl.90a1, 0)        # Brood year Upper 90% confidence limit



# First create list of the data frames and the worksheet names to be written to desk top
l <- list("Biweekly" = biweekly, "Brood Year" = brood.year)

          # second run this Code
write.csv(l, "C:/Users/nwingerter/OneDrive - DOI/Desktop/BOR Biweekly Reports/2022 Weeks 13-14/LCC/Output/BOR LCC Bootstraps Weeks 13-14.csv", row.names = FALSE)









