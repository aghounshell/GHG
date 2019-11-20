### Script to format data for GHG concentration and flux calculations: keeping reps ###
### Following: GHG_DataReps
### Updated to include data from 2016, 2017, and 2018
### A Hounshell, 20 Nov 2019

### Saved RFile as: 20Nov19_GHGReps

# load libraries
pacman::p_load(dplyr,ggplot2,zoo,gridExtra,grid,tidyverse,lubridate)

# Read in the Dissolved GHG data from '15 through '19
GHG <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data/20Nov19_DissolvedGHG_Reps.csv")

# Convert excel dates to date-time in R
GHG$datetime <- as.POSIXct(strptime(GHG$DateTime, "%m/%d/%Y", tz = "EST"))

# Separate FCR and BVR data (FCR = 1; BVR = 2)
GHG_FCR <- GHG %>% filter(Reservoir == "FCR")
GHG_BVR <- GHG %>% filter(Reservoir == "BVR")

#### Start with FCR #####
# Separate data by depth (0.1 m)
GHG_FCR0.1co2_umol_L <- GHG_FCR %>% filter(Depth_m == 0.1) %>% select(datetime, co2_umolL,Rep) 
GHG_FCR0.1co2_umol_L <- mutate(GHG_FCR0.1co2_umol_L, Depth_m = 0.1)

GHG_FCR0.1ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 0.1) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR0.1ch4_umol_L <- mutate(GHG_FCR0.1ch4_umol_L, Depth_m = 0.1)

# Separate data by depth (1.6 m)
GHG_FCR1.6co2_umol_L <- GHG_FCR %>% filter(Depth_m == 1.6) %>% select(datetime, co2_umolL,Rep)
GHG_FCR1.6co2_umol_L <- mutate(GHG_FCR1.6co2_umol_L, Depth_m = 1.6)

GHG_FCR1.6ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 1.6) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR1.6ch4_umol_L <- mutate(GHG_FCR1.6ch4_umol_L, Depth_m = 1.6)

# Separate by depth (3.8 m)
GHG_FCR3.8co2_umol_L <- GHG_FCR %>% filter(Depth_m == 3.8) %>% select(datetime, co2_umolL,Rep)
GHG_FCR3.8co2_umol_L <- mutate(GHG_FCR3.8co2_umol_L, Depth_m = 3.8)

GHG_FCR3.8ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 3.8) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR3.8ch4_umol_L <- mutate(GHG_FCR3.8ch4_umol_L, Depth_m = 3.8)

# Separate by depth (5 m)
GHG_FCR5co2_umol_L <- GHG_FCR %>% filter(Depth_m == 5) %>% select(datetime, co2_umolL,Rep)
GHG_FCR5co2_umol_L <- mutate(GHG_FCR5co2_umol_L, Depth_m = 5)

GHG_FCR5ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 5) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR5ch4_umol_L <- mutate(GHG_FCR5ch4_umol_L, Depth_m = 5)

# Separate by depth (6.2 m)
GHG_FCR6.2co2_umol_L <- GHG_FCR %>% filter(Depth_m == 6.2) %>% select(datetime, co2_umolL,Rep)
GHG_FCR6.2co2_umol_L <- mutate(GHG_FCR6.2co2_umol_L, Depth_m = 6.2)

GHG_FCR6.2ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 6.2) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR6.2ch4_umol_L <- mutate(GHG_FCR6.2ch4_umol_L, Depth_m = 6.2)

# Separate by depth (8 m)
GHG_FCR8co2_umol_L <- GHG_FCR %>% filter(Depth_m == 8) %>% select(datetime, co2_umolL,Rep)
GHG_FCR8co2_umol_L <- mutate(GHG_FCR8co2_umol_L, Depth_m = 8)

GHG_FCR8ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 8) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR8ch4_umol_L <- mutate(GHG_FCR8ch4_umol_L, Depth_m = 8)

# Separate by depth (9 m)
GHG_FCR9co2_umol_L <- GHG_FCR %>% filter(Depth_m == 9) %>% select(datetime, co2_umolL,Rep)
GHG_FCR9co2_umol_L <- mutate(GHG_FCR9co2_umol_L, Depth_m = 9)

GHG_FCR9ch4_umol_L <- GHG_FCR %>% filter(Depth_m == 9) %>% select(datetime, ch4_umolL,Rep)
GHG_FCR9ch4_umol_L <- mutate(GHG_FCR9ch4_umol_L, Depth_m = 9)

# Re-combine all depths
GHG_FCR_co2_umol_L <- rbind(GHG_FCR0.1co2_umol_L,GHG_FCR1.6co2_umol_L,GHG_FCR3.8co2_umol_L,
                            GHG_FCR5co2_umol_L,GHG_FCR6.2co2_umol_L,GHG_FCR8co2_umol_L,
                            GHG_FCR9co2_umol_L, deparse.level = 1)
GHG_FCR_ch4_umol_L <- rbind(GHG_FCR0.1ch4_umol_L,GHG_FCR1.6ch4_umol_L,GHG_FCR3.8ch4_umol_L,
                            GHG_FCR5ch4_umol_L,GHG_FCR6.2ch4_umol_L,GHG_FCR8ch4_umol_L,
                            GHG_FCR9ch4_umol_L, deparse.level = 1)

# Re-combine all data (Co2 and CH4)
GHG_FCR_2 <- cbind(GHG_FCR_co2_umol_L, GHG_FCR_ch4_umol_L, deparse.level = 1)
GHG_FCR_2 <- GHG_FCR_2[,c(1,2,6,7,8)]

names(GHG_FCR_2)[1] <- "datetime"
names(GHG_FCR_2)[2] <- "co2_umol_L"
names(GHG_FCR_2)[3] <- "ch4_umol_L"
names(GHG_FCR_2)[4] <- "rep"
names(GHG_FCR_2)[5] <- "depth"

GHG_FCR_15_19 <- GHG_FCR_2 %>% select(datetime, depth, rep, ch4_umol_L, co2_umol_L) %>% arrange(datetime)

# Export 'cleaned' version of GHG_FCR data: keep reps
write_csv(GHG_FCR_15_19, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_15_19_reps.csv")

# Export data for 2016-2018 ONLY (aka: include Summer 2016 and Summer 2017 data)
GHG_FCR_16_18 <- GHG_FCR_15_19 %>% filter(datetime>=as.Date("2016-01-01")&datetime<=as.Date("2018-12-31"))
write_csv(GHG_FCR_16_18, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_16_18_reps.csv")

# Select and export out data from 0.1 for FCR ONLY
GHG_FCR_0.1 <- GHG_FCR_15_19 %>% filter(depth==0.1)
write_csv(GHG_FCR_0.1, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_0.1_15to19_reps.csv")

# Select and export out data from 0.1 for FCR from 2016-2018 ONLY
GHG_FCR_0.1_16_18 <- GHG_FCR_0.1 %>% filter(datetime>=as.Date("2016-01-01")&datetime<=as.Date("2018-12-31"))
write_csv(GHG_FCR_0.1_16_18, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_0.1_16to18_reps.csv")

##### BVR data ######
# BVR data at 0.1 m
GHG_BVR0.1co2_umol_L <- GHG_BVR %>% filter(Depth_m == 0.1) %>% select(datetime, co2_umolL,Rep) 
GHG_BVR0.1co2_umol_L <- mutate(GHG_BVR0.1co2_umol_L, Depth_m = 0.1)

GHG_BVR0.1ch4_umol_L <- GHG_BVR %>% filter(Depth_m == 0.1) %>% select(datetime, ch4_umolL,Rep)
GHG_BVR0.1ch4_umol_L <- mutate(GHG_BVR0.1ch4_umol_L, Depth_m = 0.1)

# Export BVR data at 3 m
GHG_BVR3co2_umol_L <- GHG_BVR %>% filter(Depth_m == 3) %>% select(datetime, co2_umolL,Rep) 
GHG_BVR3co2_umol_L <- mutate(GHG_BVR3co2_umol_L, Depth_m = 3)

GHG_BVR3ch4_umol_L <- GHG_BVR %>% filter(Depth_m == 3) %>% select(datetime, ch4_umolL,Rep)
GHG_BVR3ch4_umol_L <- mutate(GHG_BVR3ch4_umol_L, Depth_m = 3)

# Export BVR data at 6 m
GHG_BVR6co2_umol_L <- GHG_BVR %>% filter(Depth_m == 6) %>% select(datetime, co2_umolL,Rep) 
GHG_BVR6co2_umol_L <- mutate(GHG_BVR6co2_umol_L, Depth_m = 6)

GHG_BVR6ch4_umol_L <- GHG_BVR %>% filter(Depth_m == 6) %>% select(datetime, ch4_umolL,Rep)
GHG_BVR6ch4_umol_L <- mutate(GHG_BVR6ch4_umol_L, Depth_m = 6)

# Export BVR data at 9 m
GHG_BVR9co2_umol_L <- GHG_BVR %>% filter(Depth_m == 9) %>% select(datetime, co2_umolL,Rep) 
GHG_BVR9co2_umol_L <- mutate(GHG_BVR9co2_umol_L, Depth_m = 9)

GHG_BVR9ch4_umol_L <- GHG_BVR %>% filter(Depth_m == 9) %>% select(datetime, ch4_umolL,Rep)
GHG_BVR9ch4_umol_L <- mutate(GHG_BVR9ch4_umol_L, Depth_m = 9)

# Export BVR data at 11 m
GHG_BVR11co2_umol_L <- GHG_BVR %>% filter(Depth_m == 11) %>% select(datetime, co2_umolL,Rep) 
GHG_BVR11co2_umol_L <- mutate(GHG_BVR11co2_umol_L, Depth_m = 11)

GHG_BVR11ch4_umol_L <- GHG_BVR %>% filter(Depth_m == 11) %>% select(datetime, ch4_umolL,Rep)
GHG_BVR11ch4_umol_L <- mutate(GHG_BVR11ch4_umol_L, Depth_m = 11)

# Re-combine all depths
GHG_BVR_co2_umol_L <- rbind(GHG_BVR0.1co2_umol_L,GHG_BVR3co2_umol_L,GHG_BVR6co2_umol_L,
                            GHG_BVR9co2_umol_L,GHG_BVR11co2_umol_L, deparse.level = 1)
GHG_BVR_ch4_umol_L <- rbind(GHG_BVR0.1ch4_umol_L,GHG_BVR3ch4_umol_L,GHG_BVR6ch4_umol_L,
                            GHG_BVR9ch4_umol_L,GHG_BVR11ch4_umol_L, deparse.level = 1)

# Re-combine all data (Co2 and CH4)
GHG_BVR_2 <- cbind(GHG_BVR_co2_umol_L, GHG_BVR_ch4_umol_L, deparse.level = 1)
GHG_BVR_2 <- GHG_BVR_2[,c(1,2,6,7,8)]

names(GHG_BVR_2)[1] <- "datetime"
names(GHG_BVR_2)[2] <- "co2_umol_L"
names(GHG_BVR_2)[3] <- "ch4_umol_L"
names(GHG_BVR_2)[4] <- "rep"
names(GHG_BVR_2)[5] <- "depth"

GHG_BVR_15_19 <- GHG_BVR_2 %>% select(datetime, depth, ch4_umol_L, co2_umol_L,rep) %>% arrange(datetime)

# Export 'cleaned' version of GHG_FCR data - include reps
write_csv(GHG_BVR_15_19, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_15_19_reps.csv")

# Export data for 2016-2018 ONLY (aka: include Summer 2016-2018 data)
GHG_BVR_16_18 <- GHG_BVR_15_19 %>% filter(datetime>=as.Date("2016-01-01")&datetime<=as.Date("2018-12-31"))
write_csv(GHG_BVR_16_18, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_16_18_reps.csv")

# Select and export out data from 0.1 for FCR ONLY
GHG_BVR_0.1 <- GHG_BVR_15_19 %>% filter(depth==0.1)
write_csv(GHG_BVR_0.1, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_0.1_16to19_reps.csv")

# Select and export out data from 0.1 for FCR from 2016-2018 ONLY
GHG_BVR_0.1_16_18 <- GHG_BVR_0.1 %>% filter(datetime>=as.Date("2016-01-01")&datetime<=as.Date("2018-12-31"))
write_csv(GHG_FCR_0.1_16_18, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_0.1_16_18_reps.csv")
