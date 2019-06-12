## Script to format CTD and YSI data (as needed) to determine thermocline depth
## in Lake Analyzer for both BVR and FCR
## A Hounshell, 12 Jun 2019

# Load in libraries
# install.packages('pacman')
pacman::p_load(tidyverse,zoo)

# Set working directory
setwd("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data")

# Load in CTD file: for all reservoirs, 2013-2018
ysi <- read_csv('YSI_Profiles.csv')
ysi$DateTime <- format(as.POSIXct(strptime(ysi$DateTime, "%m/%d/%Y %H:%M", tz = "EST")),format="%m/%d/%Y")
ysi$DateTime <- as.POSIXct(strptime(ysi$DateTime, "%m/%d/%Y", tz = "EST"))
ctd <- read_csv('CTD_Casts.csv')
ctd$Date <- format(as.POSIXct(strptime(ctd$Date, "%m/%d/%Y", tz="EST")),format="%m/%d/%Y")
ctd$Date <- as.POSIXct(strptime(ctd$Date,"%m/%d/%Y", tz="EST"))

# Select BVR and FCR for both CTD and YSI casts
ysi_bvr <- ysi %>% filter(Reservoir=="BVR"&Site==50)
ysi_fcr <- ysi %>% filter(Reservoir=="FCR"&Site==50)
ctd_bvr <- ctd %>% filter(Reservoir=="BVR"&Site==50)
ctd_fcr <- ctd %>% filter(Reservoir=="FCR"&Site==50)

# Filter dates from 2016 to 2017
ysi_bvr_date <- ysi_bvr %>% filter(ysi_bvr$DateTime>=as.Date("2016-01-01")&ysi_bvr$DateTime<=as.Date("2017-12-31"))
ysi_fcr_date <- ysi_fcr %>% filter(ysi_fcr$DateTime>=as.Date("2016-01-01")&ysi_fcr$DateTime<=as.Date("2017-12-31"))
ctd_bvr_date <- ctd_bvr %>% filter(ctd_bvr$Date>=as.Date("2016-01-01")&ctd_bvr$Date<=as.Date("2017-12-31"))
ctd_fcr_date <- ctd_fcr %>% filter(ctd_fcr$Date>=as.Date("2016-01-01")&ctd_fcr$Date<=as.Date("2017-12-31"))

# Select unique dates from both CTD and YSI casts
ysi_bvr_date_list <- as.data.frame(unique(as.Date(ysi_bvr_date$DateTime)))
names(ysi_bvr_date_list)[1] <- "dates"
ysi_bvr_date_list$ysi_bvr <- rep(-99,length(ysi_bvr_date_list$dates))

ysi_fcr_date_list <- as.data.frame(unique(as.Date(ysi_fcr_date$DateTime)))
names(ysi_fcr_date_list)[1] <- "dates"
ysi_fcr_date_list$ysi_fcr <- rep(-99,length(ysi_fcr_date_list$dates))

ctd_bvr_date_list <- as.data.frame(unique(as.Date(ctd_bvr_date$Date)))
names(ctd_bvr_date_list)[1] <- "dates"
ctd_bvr_date_list$ctd_bvr <- rep(-99,length(ctd_bvr_date_list$dates))

ctd_fcr_date_list <- as.data.frame(unique(as.Date(ctd_fcr_date$Date)))
names(ctd_fcr_date_list)[1] <- "dates"
ctd_fcr_date_list$ctd_fcr <- rep(-99,length(ctd_fcr_date_list$dates))

# Combine Unique dates list by date
bvr_dates <- merge(ysi_bvr_date_list, ctd_bvr_date_list, by="dates", all.x=TRUE, all.y=TRUE)
fcr_dates <- merge(ysi_fcr_date_list, ctd_fcr_date_list, by="dates", all.x=TRUE, all.y=TRUE)

## Need to merge the CTD and YSI data sets by date, then 'fill in' missing CTD
## data with YSI data
names(ysi_fcr_date)[3] <- "Date"
fcr_merge <- merge(ctd_fcr_date, ysi_fcr_date, by="Date", all.x=TRUE, all.y=TRUE)

#### Need to think about how I want to merge the CTD and YSI data
#### (and what data to select and if the merge function actually worked...)

# Save Rfile as Thermocline_Data