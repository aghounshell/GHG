### Script to format data for GHG concentration and flux calculations: keeping reps ###
### Primary Author of this script: Ryan P McClure ###
### Last updated: 070518 ###
### Adapted, A.G. Hounshell, 19 Sep 2019 ####

# load libraries (Only need tidyverse for now)
library(dplyr)
library(ggplot2)
library(zoo)
library(gridExtra)
library(grid)
library(tidyverse)
library(lubridate)

#setWD
setwd("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data")

# Read in the Dissolved GHG data from '15 through '17
GHG <- read_csv("Initial Dissolved DATA 15_17.csv")

# Convert excel dates to date-time in R
GHG$datetime <- as.POSIXct(strptime(GHG$datetime, "%m/%d/%Y", tz = "EST"))

# Separate FCR and BVR data (FCR = 1; BVR = 2)
GHG_FCR <- GHG %>% filter(reservoir == 1)
GHG_BVR <- GHG %>% filter(reservoir == 2)

# Separate data by depth (0.1 m); aggregate data by date and average if two reps, use single rep when only 1 rep collected
GHG_FCR0.1co2_umol_L <- GHG_FCR %>% filter(depth == 0.1) %>% select(datetime, co2_umol_L,rep) 
GHG_FCR0.1co2_umol_L <- mutate(GHG_FCR0.1co2_umol_L, depth = 0.1)

GHG_FCR0.1ch4_umol_L <- GHG_FCR %>% filter(depth == 0.1) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR0.1ch4_umol_L <- mutate(GHG_FCR0.1ch4_umol_L, depth = 0.1)

# Separate data by depth (1.6 m)
GHG_FCR1.6co2_umol_L <- GHG_FCR %>% filter(depth == 1.6) %>% select(datetime, co2_umol_L,rep)
GHG_FCR1.6co2_umol_L <- mutate(GHG_FCR1.6co2_umol_L, depth = 1.6)

GHG_FCR1.6ch4_umol_L <- GHG_FCR %>% filter(depth == 1.6) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR1.6ch4_umol_L <- mutate(GHG_FCR1.6ch4_umol_L, depth = 1.6)

# Separate by depth (3.8 m)
GHG_FCR3.8co2_umol_L <- GHG_FCR %>% filter(depth == 3.8) %>% select(datetime, co2_umol_L,rep)
GHG_FCR3.8co2_umol_L <- mutate(GHG_FCR3.8co2_umol_L, depth = 3.8)

GHG_FCR3.8ch4_umol_L <- GHG_FCR %>% filter(depth == 3.8) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR3.8ch4_umol_L <- mutate(GHG_FCR3.8ch4_umol_L, depth = 3.8)

# Separate by depth (5 m)
GHG_FCR5co2_umol_L <- GHG_FCR %>% filter(depth == 5) %>% select(datetime, co2_umol_L,rep)
GHG_FCR5co2_umol_L <- mutate(GHG_FCR5co2_umol_L, depth = 5)

GHG_FCR5ch4_umol_L <- GHG_FCR %>% filter(depth == 5) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR5ch4_umol_L <- mutate(GHG_FCR5ch4_umol_L, depth = 5)

# Separate by depth (6.2 m)
GHG_FCR6.2co2_umol_L <- GHG_FCR %>% filter(depth == 6.2) %>% select(datetime, co2_umol_L,rep)
GHG_FCR6.2co2_umol_L <- mutate(GHG_FCR6.2co2_umol_L, depth = 6.2)

GHG_FCR6.2ch4_umol_L <- GHG_FCR %>% filter(depth == 6.2) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR6.2ch4_umol_L <- mutate(GHG_FCR6.2ch4_umol_L, depth = 6.2)

# Separate by depth (8 m)
GHG_FCR8co2_umol_L <- GHG_FCR %>% filter(depth == 8) %>% select(datetime, co2_umol_L,rep)
GHG_FCR8co2_umol_L <- mutate(GHG_FCR8co2_umol_L, depth = 8)

GHG_FCR8ch4_umol_L <- GHG_FCR %>% filter(depth == 8) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR8ch4_umol_L <- mutate(GHG_FCR8ch4_umol_L, depth = 8)

# Separate by depth (9 m)
GHG_FCR9co2_umol_L <- GHG_FCR %>% filter(depth == 9) %>% select(datetime, co2_umol_L,rep)
GHG_FCR9co2_umol_L <- mutate(GHG_FCR9co2_umol_L, depth = 9)

GHG_FCR9ch4_umol_L <- GHG_FCR %>% filter(depth == 9) %>% select(datetime, ch4_umol_L,rep)
GHG_FCR9ch4_umol_L <- mutate(GHG_FCR9ch4_umol_L, depth = 9)

# Separate inflow data (inf)
GHG_FCRinfco2_umol_L <- GHG_FCR %>% filter(depth == "Inf") %>% select(datetime, co2_umol_L,rep)
GHG_FCRinfco2_umol_L <- mutate(GHG_FCRinfco2_umol_L, depth = "inf")

GHG_FCRinfch4_umol_L <- GHG_FCR %>% filter(depth == "Inf") %>% select(datetime, ch4_umol_L,rep)
GHG_FCRinfch4_umol_L <- mutate(GHG_FCRinfch4_umol_L, depth = "inf")

# Re-combine all depths
GHG_FCR_co2_umol_L <- rbind(GHG_FCR0.1co2_umol_L,GHG_FCR1.6co2_umol_L,GHG_FCR3.8co2_umol_L,GHG_FCR5co2_umol_L,GHG_FCR6.2co2_umol_L,GHG_FCR8co2_umol_L,GHG_FCR9co2_umol_L,GHG_FCRinfco2_umol_L, deparse.level = 1)
GHG_FCR_ch4_umol_L <- rbind(GHG_FCR0.1ch4_umol_L,GHG_FCR1.6ch4_umol_L,GHG_FCR3.8ch4_umol_L,GHG_FCR5ch4_umol_L,GHG_FCR6.2ch4_umol_L,GHG_FCR8ch4_umol_L,GHG_FCR9ch4_umol_L,GHG_FCRinfch4_umol_L, deparse.level = 1)

# Re-combine all data (Co2 and CH4)
GHG_FCR_2 <- cbind(GHG_FCR_co2_umol_L, GHG_FCR_ch4_umol_L, deparse.level = 1)
GHG_FCR_2 <- GHG_FCR_2[,c(1,2,6,7,8)]

names(GHG_FCR_2)[1] <- "datetime"
names(GHG_FCR_2)[2] <- "co2_umol_L"
names(GHG_FCR_2)[3] <- "ch4_umol_L"
names(GHG_FCR_2)[4] <- "rep"
names(GHG_FCR_2)[5] <- "depth"
GHG_FCR_2$co2_umol_L <- na.approx(GHG_FCR_2$co2_umol_L)

GHG_FCR_15_17 <- GHG_FCR_2 %>% select(datetime, depth, rep, ch4_umol_L, co2_umol_L) %>% arrange(datetime)

# Export 'cleaned' version of GHG_FCR data (removes NaN values; calculates mean between replicates; organizes by time and depth)
write_csv(GHG_FCR_15_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_15_17_reps.csv")

# Export data for 2016-2018 ONLY (aka: include Summer 2016 and Summer 2017 data)
GHG_FCR_16_17 <- GHG_FCR_15_17 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_FCR_16_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_16_17_reps.csv")

# Select and export out data from 0.1 for FCR ONLY
GHG_FCR_0.1 <- GHG_FCR_15_17 %>% filter(depth==0.1)
write_csv(GHG_FCR_0.1, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_0.1_reps.csv")

# Select and export out data from 0.1 for FCR from 2016-2018 ONLY
GHG_FCR_0.1_16_17 <- GHG_FCR_0.1 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_FCR_0.1_16_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_FCR_0.1_16_17_reps.csv")

# Export BVR data at 0.1 m
GHG_BVR0.1co2_umol_L <- GHG_BVR %>% filter(depth == 0.1) %>% select(datetime, co2_umol_L,rep) 
GHG_BVR0.1co2_umol_L <- mutate(GHG_BVR0.1co2_umol_L, depth = 0.1)

GHG_BVR0.1ch4_umol_L <- GHG_BVR %>% filter(depth == 0.1) %>% select(datetime, ch4_umol_L,rep)
GHG_BVR0.1ch4_umol_L <- mutate(GHG_BVR0.1ch4_umol_L, depth = 0.1)

GHG_BVR_0.1 <- cbind(GHG_BVR0.1ch4_umol_L,GHG_BVR0.1co2_umol_L,deparse.level = 1)
GHG_BVR_0.1 <- GHG_BVR_0.1[,c(1,2,6,7,8)]

names(GHG_BVR_0.1)[1] <- "datetime"
names(GHG_BVR_0.1)[2] <- "ch4_umol_L"
names(GHG_BVR_0.1)[3] <- "co2_umol_L"
names(GHG_BVR_0.1)[4] <- "rep"
names(GHG_BVR_0.1)[5] <- "depth"
GHG_BVR_0.1$co2_umol_L <- na.approx(GHG_BVR_0.1$co2_umol_L)

# Export out BVR 0.1 GHG data
# Export 'cleaned' version of GHG_FCR data (removes NaN values; calculates mean between replicates; organizes by time and depth)
write_csv(GHG_BVR_0.1, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_0.1_reps.csv")

# Export data from BVR at 0.1 m for 2016-2017 only
GHG_BVR_0.1_16_17 <- GHG_BVR_0.1 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_BVR_0.1_16_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_0.1_16_17_reps.csv")

# Separate out data from remaining BVR depths (3,6,9,11 m)
# Export BVR data at 3 m
GHG_BVR3co2_umol_L <- GHG_BVR %>% filter(depth == 3) %>% select(datetime, co2_umol_L,rep) 
GHG_BVR3co2_umol_L <- mutate(GHG_BVR3co2_umol_L, depth = 3)

GHG_BVR3ch4_umol_L <- GHG_BVR %>% filter(depth == 3) %>% select(datetime, ch4_umol_L,rep)
GHG_BVR3ch4_umol_L <- mutate(GHG_BVR3ch4_umol_L, depth = 3)

# Export BVR data at 6 m
GHG_BVR6co2_umol_L <- GHG_BVR %>% filter(depth == 6) %>% select(datetime, co2_umol_L,rep) 
GHG_BVR6co2_umol_L <- mutate(GHG_BVR6co2_umol_L, depth = 6)

GHG_BVR6ch4_umol_L <- GHG_BVR %>% filter(depth == 6) %>% select(datetime, ch4_umol_L,rep)
GHG_BVR6ch4_umol_L <- mutate(GHG_BVR6ch4_umol_L, depth = 6)

# Export BVR data at 9 m
GHG_BVR9co2_umol_L <- GHG_BVR %>% filter(depth == 9) %>% select(datetime, co2_umol_L,rep) 
GHG_BVR9co2_umol_L <- mutate(GHG_BVR9co2_umol_L, depth = 9)

GHG_BVR9ch4_umol_L <- GHG_BVR %>% filter(depth == 9) %>% select(datetime, ch4_umol_L,rep)
GHG_BVR9ch4_umol_L <- mutate(GHG_BVR9ch4_umol_L, depth = 9)

# Export BVR data at 11 m
GHG_BVR11co2_umol_L <- GHG_BVR %>% filter(depth == 11) %>% select(datetime, co2_umol_L,rep) 
GHG_BVR11co2_umol_L <- mutate(GHG_BVR11co2_umol_L, depth = 11)

GHG_BVR11ch4_umol_L <- GHG_BVR %>% filter(depth == 11) %>% select(datetime, ch4_umol_L,rep)
GHG_BVR11ch4_umol_L <- mutate(GHG_BVR11ch4_umol_L, depth = 11)

# Re-combine all depths
GHG_BVR_co2_umol_L <- rbind(GHG_BVR0.1co2_umol_L,GHG_BVR3co2_umol_L,GHG_BVR6co2_umol_L,GHG_BVR9co2_umol_L,GHG_BVR11co2_umol_L, deparse.level = 1)
GHG_BVR_ch4_umol_L <- rbind(GHG_BVR0.1ch4_umol_L,GHG_BVR3ch4_umol_L,GHG_BVR6ch4_umol_L,GHG_BVR9ch4_umol_L,GHG_BVR11ch4_umol_L, deparse.level = 1)

# Re-combine all data (Co2 and CH4)
GHG_BVR_2 <- cbind(GHG_BVR_co2_umol_L, GHG_BVR_ch4_umol_L, deparse.level = 1)
GHG_BVR_2 <- GHG_BVR_2[,c(1,2,6,7,8)]

names(GHG_BVR_2)[1] <- "datetime"
names(GHG_BVR_2)[2] <- "co2_umol_L"
names(GHG_BVR_2)[3] <- "ch4_umol_L"
names(GHG_BVR_2)[4] <- "rep"
names(GHG_BVR_2)[5] <- "depth"

GHG_BVR_15_17 <- GHG_BVR_2 %>% select(datetime, depth, ch4_umol_L, co2_umol_L,rep) %>% arrange(datetime)

# Export 'cleaned' version of GHG_FCR data (removes NaN values; calculates mean between replicates; organizes by time and depth)
write_csv(GHG_BVR_15_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_15_17_reps")

# Export data for 2016-2018 ONLY (aka: include Summer 2016 and Summer 2017 data)
GHG_BVR_16_17 <- GHG_BVR_15_17 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_BVR_16_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_16_17_reps.csv")

# Save as Rfile: pGHG_TS_reps