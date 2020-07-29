### Script to estimate percent increase assuming 11m samples were included in analyses
### Following Reviewer comments round 2
### A Hounshell, 29 July 2020

# Following BVR_Depths_Revs

# Load pacakges
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# BVR GHG data from 2016-2018
# Updated to include 'final' GHG Database from RPM
ghg <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/DATASET_for_EDI_LOL_MS_10May20.csv")
ghg$DateTime <- as.POSIXct(strptime(ghg$DateTime, "%m/%d/%Y", tz="EST"))

# Select BVR data only
ghg <- ghg %>% filter(Reservoir == "BVR")

# Separated by depth (9 m and 11 m ONLY) then recombine
# Average reps
ghg_9 <- ghg %>% filter(Depth_m == 9.0) %>% select(DateTime,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarise_all(mean,na.rm=TRUE)

ghg_11 <- ghg %>% filter(Depth_m == 11.0) %>% select(DateTime,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarise_all(mean,na.rm=TRUE)

ghg_hypo <- merge(ghg_9,ghg_11,by="DateTime")

names(ghg_hypo)[2] <- "ch4_9m"
names(ghg_hypo)[3] <- "co2_9m"
names(ghg_hypo)[4] <- "ch4_11m"
names(ghg_hypo)[5] <- "co2_11m"

# Final dataset with days that ONLY include averaged data from 9 m AND 11 m in BVR for both CH4 and CO2
ghg_hypo <- na.omit(ghg_hypo)

# Plot to see difference
ggplot()+
  geom_point(ghg_hypo,mapping=aes(x=DateTime,y=ch4_9m,color="9m"))+
  geom_line(ghg_hypo,mapping=aes(x=DateTime,y=ch4_9m,color="9m"))+
  geom_point(ghg_hypo,mapping=aes(x=DateTime,y=ch4_11m,color="11m"))+
  geom_line(ghg_hypo,mapping=aes(x=DateTime,y=ch4_11m,color="11m"))+
  theme_classic(base_size = 15)

ggplot()+
  geom_point(ghg_hypo,mapping=aes(x=DateTime,y=co2_9m,color="9m"))+
  geom_line(ghg_hypo,mapping=aes(x=DateTime,y=co2_9m,color="9m"))+
  geom_point(ghg_hypo,mapping=aes(x=DateTime,y=co2_11m,color="11m"))+
  geom_line(ghg_hypo,mapping=aes(x=DateTime,y=co2_11m,color="11m"))+
  theme_classic(base_size = 15)

# Calculate VW Hypo concentration w/ 11 m
# (VW Hypo w/o 11m = concenctration at 9 m)
vw_ch4 <- rep(-99,length(ghg_hypo$DateTime))
for(i in 1:length(ghg_hypo$DateTime)){
  vw_ch4[i] <- sum(ghg_hypo$ch4_9m[i]*29406.62888,
                        ghg_hypo$ch4_11m[i]*3164.753978)/sum(29406.62888,3164.753978)
}

vw_co2 <- rep(-99,length(ghg_hypo$DateTime))
for(i in 1:length(ghg_hypo$DateTime)){
  vw_co2[i] <- sum(ghg_hypo$co2_9m[i]*29406.62888,
                   ghg_hypo$co2_11m[i]*3164.753978)/sum(29406.62888,3164.753978)
}

ghg_hypo_all <- cbind(ghg_hypo,vw_ch4,vw_co2)

# Calculate percent difference between two methods
perc_diff_ch4 <- rep(-99,length(ghg_hypo$DateTime))
for(i in 1:length(ghg_hypo$DateTime)){
  perc_diff_ch4[i] <- ((ghg_hypo_all$ch4_9m[i]-ghg_hypo_all$vw_ch4[i])/
    (sum(ghg_hypo_all$ch4_9m[i],ghg_hypo_all$vw_ch4[i])/2))*100
}

perc_diff_co2 <- rep(-99,length(ghg_hypo$DateTime))
for(i in 1:length(ghg_hypo$DateTime)){
  perc_diff_co2[i] <- ((ghg_hypo_all$co2_9m[i]-ghg_hypo_all$vw_co2[i])/
                         (sum(ghg_hypo_all$co2_9m[i],ghg_hypo_all$vw_co2[i])/2))*100
}


ghg_hypo_all <- cbind(ghg_hypo_all,perc_diff_ch4,perc_diff_co2)

# Plot differences
ggplot()+
  geom_line(ghg_hypo_all,mapping=aes(x=DateTime,y=ch4_9m,color="9m"))+
  geom_point(ghg_hypo_all,mapping=aes(x=DateTime,y=ch4_9m,color="9m"))+
  geom_line(ghg_hypo_all,mapping=aes(x=DateTime,y=vw_ch4,color="9m + 11m"))+
  geom_point(ghg_hypo_all,mapping=aes(x=DateTime,y=vw_ch4,color="9m + 11m"))+
  theme_classic(base_size=15)

ggplot()+
  geom_line(ghg_hypo_all,mapping=aes(x=DateTime,y=co2_9m,color="9m"))+
  geom_point(ghg_hypo_all,mapping=aes(x=DateTime,y=co2_9m,color="9m"))+
  geom_line(ghg_hypo_all,mapping=aes(x=DateTime,y=vw_co2,color="9m + 11m"))+
  geom_point(ghg_hypo_all,mapping=aes(x=DateTime,y=vw_co2,color="9m + 11m"))+
  theme_classic(base_size=15)

# Plot percent difference
ggplot(ghg_hypo_all,mapping=aes(x=DateTime,y=perc_diff_ch4))+
  geom_point()+
  geom_line()+
  theme_classic(base_size = 15)

ggplot(ghg_hypo_all,mapping=aes(x=DateTime,y=perc_diff_co2))+
  geom_point()+
  geom_line()+
  theme_classic(base_size = 15)

# Underestimates CH4 concentrations by ~35%
mean(ghg_hypo_all$perc_diff_ch4)

# Underestimates CO2 concentrations by ~2%
mean(ghg_hypo_all$perc_diff_co2)
