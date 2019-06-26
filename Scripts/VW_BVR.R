### Script to calculate volume weighted parameters for BVR 2016-2017
### A Hounshell, 21 Jun 2019

# Load libraries needed
pacman::p_load(tidyverse,ggplot2)

# Load in data
# BVR Volumes by depth
depth <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data/BVR_Vol.csv")
# BVR Thermocline data (as calculated by Lake Analyzer)
thermo <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data/BVR_Thermo.csv")
# BVR merged YSI and CTD casts
casts <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/BVR_CTDysi_merge.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d %H:%M", tz = "EST"))
# BVR GHG data from 2016-2017
ghg <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_BVR_16_17.csv")

## Calculate volume weighted averages for the entire water column
# First: average concentrations between 1 m depth intervals (depth intervals
# used to calculate the volume in BVR)
casts_1 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=0 & Depth_m<1) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_1")

casts_2 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=1 & Depth_m<2) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_2")

casts_3 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=2 & Depth_m<3) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_3")

casts_4 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=3 & Depth_m<4) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_4")

casts_5 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=4 & Depth_m<5) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_5")

casts_6 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=5 & Depth_m<6) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_6")

casts_7 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=6 & Depth_m<7) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_7")

casts_8 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=7 & Depth_m<8) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_8")

casts_9 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=8 & Depth_m<9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_9")

casts_10 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_10")

## Don't use: combined the last several bottom layers - (11.5 = bottom layer)
#casts_11 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=10 & Depth_m<11) %>% group_by(Date) %>% 
#  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_11")
#casts_12 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=11) %>% group_by(Date) %>% 
#  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_12")
#casts_13 <- casts %>% filter(Depth_m>=12 & Depth_m<13) %>% group_by(Date) %>% 
#  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_13")

# Re-combine all casts then use spread to convert to wide form
casts_merge <- rbind(casts_1,casts_2,casts_3,casts_4,casts_5,casts_6,casts_7,casts_8,
                     casts_9,casts_10)
casts_merge <- casts_merge %>% select(Date,Temp_C,grouping) %>% arrange(Date)
casts_layers <- casts_merge %>% spread(grouping,Temp_C)

# Change columns to increase from smallest to largest depth
casts_layers <- casts_layers[,c(1:2,4:11,3)]

# Select complete cases
casts_layers <- na.omit(casts_layers)

# Calculate volume weighted average temperature for the entire water column
# Add volume from the bottom-most layers (10-13)
depth_2 <- depth[c(1:10),]
depth_2[10,2] <- sum(depth[c(10:13),2])

# Use for loop to calculate volume weighted averages for each time point
vw_temp <- rep(-99,length(casts_layers$Date))
for(i in 1:length(casts_layers$Date)){
  vw_temp[i] <- sum(casts_layers$BVR_1[i]*depth_2$LayerVol_m3[1],
                    casts_layers$BVR_2[i]*depth_2$LayerVol_m3[2],
                    casts_layers$BVR_3[i]*depth_2$LayerVol_m3[3],
                    casts_layers$BVR_4[i]*depth_2$LayerVol_m3[4],
                    casts_layers$BVR_5[i]*depth_2$LayerVol_m3[5],
                    casts_layers$BVR_6[i]*depth_2$LayerVol_m3[6],
                    casts_layers$BVR_7[i]*depth_2$LayerVol_m3[7],
                    casts_layers$BVR_8[i]*depth_2$LayerVol_m3[8],
                    casts_layers$BVR_9[i]*depth_2$LayerVol_m3[9],
                    casts_layers$BVR_10[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_temp <- cbind(casts_layers,vw_temp)

# Check vw_temp values
ggplot(vw_temp,aes(Date,vw_temp))+geom_line()+theme_classic()

## Calculate VW Temp values for the: Epi, Meta, and Hypo using variable thermocline 
## depths
# First round to the nearest whole depth (i.e., 1 m) for the Upper and Lower Meta
thermo$SmetaB_round <- round(thermo$SmetaB_m,0)
thermo$SmetaT_round <- round(thermo$SmetaT_m,0)

# Tell R to match the meta Top to the correct depth and volume weight above that depth
vw_epi <- rep(-99,length(casts_layers$Date))
for(i in 1:length(casts_layers$Date)){
  for (j in 1:length(depth_2$Depth_m)){
    if(depth_2$Depth_m[j]==thermo$SmetaT_round[i]){
      vw_epi[i] <- j
    }
  }
}
