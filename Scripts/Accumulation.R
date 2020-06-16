### Script to estimate lag time between anoxia and CH4 accumulation
### For BVR 2016, 2017, and 2018; FCR 2018
### A Hounshell, 16 June 2020

# Load libraries
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load CTD/YSI data for BVR and FCR
fcr_casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18.csv")
fcr_casts$Date <- as.POSIXct(strptime(fcr_casts$Date, "%Y-%m-%d", tz = "EST"))

bvr_casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18.csv")
bvr_casts$Date <- as.POSIXct(strptime(bvr_casts$Date, "%Y-%m-%d", tz = "EST"))

# Select bottom waters for FCR and BVR (same designation as VW calculations)
# FCR
fcr_bottom <- fcr_casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date)

ggplot(fcr_bottom,mapping=aes(x=Date,y=DO_mgL))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 1)+
  theme_classic(base_size = 15)

# Anoxia onset (DO<1mg/L): 2018 = 2018-08-02

# BVR
bvr_bottom <- bvr_casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=8) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date)

ggplot(bvr_bottom,mapping=aes(x=Date,y=DO_mgL))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 1)+
  theme_classic(base_size = 15)

# Anoxia onset (DO<1mg/L): 2016 = 2016-05-12; 2017 = 2017-05-05;  2018 = 2018-05-11

# Load GHG data
fcr_ch4 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/FCR_VW_ch4_stats_16to18_Revs.csv")
fcr_ch4$datetime <- as.POSIXct(strptime(fcr_ch4$datetime, "%Y-%m-%d", tz = "EST"))

bvr_ch4 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/BVR_VW_ch4_stats_16to18_Revs.csv")
bvr_ch4$datetime <- as.POSIXct(strptime(bvr_ch4$datetime, "%Y-%m-%d", tz = "EST"))

# Plot w/ vline for anoxia onset
ggplot(fcr_ch4,mapping=aes(x=datetime,y=hypo_avg))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.POSIXct("2018-08-02"))+
  geom_hline(yintercept = 10)+
  ylim(0,600)+
  theme_classic(base_size=15)

# GHG accumulation > 10 mg/L: 2018 = 2018-08-27 (3.57 weeks)

ggplot(bvr_ch4,mapping=aes(x=datetime,y=hypo_avg))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = as.POSIXct("2016-05-12"))+
  geom_vline(xintercept = as.POSIXct("2017-05-05"))+
  geom_vline(xintercept = as.POSIXct("2018-05-11"))+
  geom_hline(yintercept = 10)+
  ylim(0,600)+
  theme_classic(base_size=15)

# GHG Acumulation > 10 mg/L: 2016 = 2016-06-30 (7 weeks); 2017 = 2017-06-09 (5 weeks); 
# 2018 = 2018-05-31 (3 weeks)
