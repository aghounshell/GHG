### Script to explore updated Dissolved GHG data from RPM (11Nov19)
# Includes GHG data from FCR and BVR for 2015-2019
# Goal is to use GHG data from 2016-2018 for BVR and FCR
# A Hounshell, 11 Nov 2019

# load libraries
pacman::p_load(dplyr,ggplot2,zoo,gridExtra,grid,tidyverse,lubridate)

# Read in the Dissolved GHG data from '15 through '17
GHG_master <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data/11Nov19_DissolvedGHG_Reps.csv")

# Convert excel dates to date-time in R
GHG_master$DateTime <- as.POSIXct(strptime(GHG_master$DateTime, "%m/%d/%Y", tz = "EST"))

# Separate by reservoir, year, and select 'correct' depths
fcr <- GHG_master %>% filter(Reservoir=='FCR')
bvr <- GHG_master %>% filter(Reservoir=='BVR')

# Check GHG dates against secchi dates (from EDI) for FCR and BVR:
# Aka: Do we have all the GHG data?
# Separate data by reservoir, a single depth, and year (2016, 2017, 2018)
fcr_16 <- fcr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2016-01-01')&
                                                                       DateTime<=as.Date('2016-12-31'))
fcr_17 <- fcr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2017-01-01')&
                                                                       DateTime<=as.Date('2017-12-31'))
fcr_18 <- fcr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2018-01-01')&
                                                                       DateTime<=as.Date('2018-12-31'))

bvr_16 <- bvr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2016-01-01')&
                                                                       DateTime<=as.Date('2016-12-31'))
bvr_17 <- bvr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2017-01-01')&
                                                                       DateTime<=as.Date('2017-12-31'))
bvr_18 <- bvr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2018-01-01')&
                                                                       DateTime<=as.Date('2018-12-31'))

# Filter for each depth
fcr_0.1_1 <- fcr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))
fcr_1.6
fcr_3.8
fcr_5
fcr_6.2
fcr_8
fcr_9

bvr_0.1_1 <- bvr %>% filter(Depth_m==0.1) %>% filter(Rep==1) %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))


fcr_15 <- fcr %>% filter(DateTime>=as.Date("2015-01-01")&DateTime<=as.Date("2015-12-31")) 
fcr_16
fcr_17
fcr_18
fcr_19

bvr_15
bvr_16
bvr_17
bvr_18
bvr_19