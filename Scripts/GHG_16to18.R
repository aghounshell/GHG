### Script to format data for GHG concentration and flux calculations: keeping reps ###
### Following: GHG_DataReps
### Updated to include data from 2016, 2017, and 2018
### A Hounshell, 20 Nov 2019

# load libraries
pacman::p_load(dplyr,ggplot2,zoo,gridExtra,grid,tidyverse,lubridate)

# Read in the Dissolved GHG data from '15 through '19
GHG <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data/20Nov19_DissolvedGHG_Reps.csv")

# Convert excel dates to date-time in R
GHG$datetime <- as.POSIXct(strptime(GHG$datetime, "%m/%d/%Y", tz = "EST"))

# Separate FCR and BVR data (FCR = 1; BVR = 2)
GHG_FCR <- GHG %>% filter(reservoir == 1)
GHG_BVR <- GHG %>% filter(reservoir == 2)