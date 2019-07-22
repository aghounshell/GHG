### Script to calculate Epi, Meta, and Hypo volume weighted averaged Temp, DO, pCH4, and pCO2
### For BVR Summer 2016 and Summer 2017
### Defined Epi, Meta, and Hypo as follows:
###     Epi: 0.1 m + 3.0 m (0 - 4m)
###     Meta: 6.0 m (4 - 7m)
###     Hypo: 9.0 m + 11.0 m (7 - 11m)
### A Hounshell, 16Jul19

# Load libraries needed
pacman::p_load(tidyverse,ggplot2)

# Load in data
# BVR Volumes by depth
depth <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/BVR_Vol.csv")
# BVR merged YSI and CTD casts
casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_CTDysi_merge.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d", tz = "EST"))
# BVR GHG data from 2016-2017
ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_16_17.csv")
ghg$datetime <- as.POSIXct(strptime(ghg$datetime, "%Y-%m-%d", tz="EST"))

### Start with temperature as a check...(code modified from VW_BVR.R)
## Calculate volume weighted averages for the entire water column
# First: average concentrations between 1 m depth intervals (depth intervals
# used to calculate the volume in BVR)
# Also average across casts when multiple casts/day
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
names(vw_temp)[1] <- "DateTime"

# Check vw_temp values
ggplot(vw_temp,aes(DateTime,vw_temp))+geom_line()+theme_classic()

### Calculate Epi VW Temp: defined as 0-4 m
vw_epi_temp <- rep(-99,length(casts_layers$Date))
for(i in 1:57){
  vw_epi_temp[i] <- sum(casts_layers$BVR_1[i]*depth_2$LayerVol_m3[1],
                    casts_layers$BVR_2[i]*depth_2$LayerVol_m3[2],
                    casts_layers$BVR_3[i]*depth_2$LayerVol_m3[3],
                    casts_layers$BVR_4[i]*depth_2$LayerVol_m3[4])/sum(depth_2$LayerVol_m3[c(1:4)])
}

### Calculate Meta VW Temp: defined as 4-7m
vw_meta_temp <- rep(-99,length(casts_layers$Date))
for(i in 1:57){
  vw_meta_temp[i] <- sum(casts_layers$BVR_5[i]*depth_2$LayerVol_m3[5],
                        casts_layers$BVR_6[i]*depth_2$LayerVol_m3[6],
                        casts_layers$BVR_7[i]*depth_2$LayerVol_m3[7])/sum(depth_2$LayerVol_m3[c(5:7)])
}

### Calculate Hypo VW Temp: defined as 7-11m
vw_hypo_temp <- rep(-99,length(casts_layers$Date))
for(i in 1:length(casts_layers$Date)){
  vw_hypo_temp[i] <- sum(casts_layers$BVR_8[i]*depth_2$LayerVol_m3[8],
                    casts_layers$BVR_9[i]*depth_2$LayerVol_m3[9],
                    casts_layers$BVR_10[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(8:10)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_tempf <- cbind.data.frame(vw_temp$DateTime,vw_temp$vw_temp,vw_epi_temp,vw_meta_temp,vw_hypo_temp)
names(vw_tempf)[1] <- "DateTime"
names(vw_tempf)[2] <- "WaterCol"
names(vw_tempf)[3] <- "Epi"
names(vw_tempf)[4] <- "Meta"
names(vw_tempf)[5] <- "Hypo"

# Plot to check
ggplot(vw_tempf,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('Temp')+
  theme_classic()

# Export out Temp Data VW averaged by depth
write_csv(vw_tempf, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_Temp")

### Calculate the same for oxygen
# Re-combine all casts then use spread to convert to wide form
casts_o2 <- rbind(casts_1,casts_2,casts_3,casts_4,casts_5,casts_6,casts_7,casts_8,
                     casts_9,casts_10)
casts_o2 <- casts_o2 %>% select(Date,DO_mgL,grouping) %>% arrange(Date)
casts_layers_o2 <- casts_o2 %>% spread(grouping,DO_mgL)

# Change columns to increase from smallest to largest depth
casts_layers_o2 <- casts_layers_o2[,c(1:2,4:11,3)]

# Select complete cases
casts_layers_o2 <- na.omit(casts_layers_o2)

# Calculate volume weighted average O2 for the entire water column
# Use for loop to calculate volume weighted averages for each time point
vw_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:56){
  vw_o2[i] <- sum(casts_layers_o2$BVR_1[i]*depth_2$LayerVol_m3[1],
                    casts_layers_o2$BVR_2[i]*depth_2$LayerVol_m3[2],
                    casts_layers_o2$BVR_3[i]*depth_2$LayerVol_m3[3],
                    casts_layers_o2$BVR_4[i]*depth_2$LayerVol_m3[4],
                    casts_layers_o2$BVR_5[i]*depth_2$LayerVol_m3[5],
                    casts_layers_o2$BVR_6[i]*depth_2$LayerVol_m3[6],
                    casts_layers_o2$BVR_7[i]*depth_2$LayerVol_m3[7],
                    casts_layers_o2$BVR_8[i]*depth_2$LayerVol_m3[8],
                    casts_layers_o2$BVR_9[i]*depth_2$LayerVol_m3[9],
                    casts_layers_o2$BVR_10[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_o2 <- cbind(casts_layers_o2,vw_o2)
names(vw_o2)[1] <- "DateTime"

# Check vw_o2 values
ggplot(vw_o2,aes(DateTime,vw_o2))+geom_line()+theme_classic()

### Calculate Epi VW O2: defined as 0-4 m
vw_epi_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:56){
  vw_epi_o2[i] <- sum(casts_layers_o2$BVR_1[i]*depth_2$LayerVol_m3[1],
                        casts_layers_o2$BVR_2[i]*depth_2$LayerVol_m3[2],
                        casts_layers_o2$BVR_3[i]*depth_2$LayerVol_m3[3],
                        casts_layers_o2$BVR_4[i]*depth_2$LayerVol_m3[4])/sum(depth_2$LayerVol_m3[c(1:4)])
}

### Calculate Meta VW O2: defined as 4-7m
vw_meta_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:56){
  vw_meta_o2[i] <- sum(casts_layers_o2$BVR_5[i]*depth_2$LayerVol_m3[5],
                         casts_layers_o2$BVR_6[i]*depth_2$LayerVol_m3[6],
                         casts_layers_o2$BVR_7[i]*depth_2$LayerVol_m3[7])/sum(depth_2$LayerVol_m3[c(5:7)])
}

### Calculate Hypo VW O2: defined as 7-11m
vw_hypo_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_hypo_o2[i] <- sum(casts_layers_o2$BVR_8[i]*depth_2$LayerVol_m3[8],
                         casts_layers_o2$BVR_9[i]*depth_2$LayerVol_m3[9],
                         casts_layers_o2$BVR_10[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(8:10)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_o2f <- cbind.data.frame(vw_o2$DateTime,vw_o2$vw_o2,vw_epi_o2,vw_meta_o2,vw_hypo_o2)
names(vw_o2f)[1] <- "DateTime"
names(vw_o2f)[2] <- "WaterCol"
names(vw_o2f)[3] <- "Epi"
names(vw_o2f)[4] <- "Meta"
names(vw_o2f)[5] <- "Hypo"

# Plot to check
ggplot(vw_o2f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('DO')+
  theme_classic()

# Export out Temp Data VW averaged by depth
write_csv(vw_o2f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_o2")

### Calculate pCH4 for full water column and by depth: depths designated as:
# Epi = 0.1, 3.0 m
# Meta = 6.0 m
# Hypo = 9.0, 11.0 m
## Start with full water column
# Define each collected sample as follows (in terms of designating volume)
# 0.1 m = 1 + 2 m
# 3.0 m = 3 + 4 m
# 6.0 m = 5 + 6 + 7 m
# 9.0 m = 8 + 9 m
# 11.0 m = 10 m and below

# Separate by depth then recombine
ghg_1 <- ghg %>% filter(depth==0.1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_1")

ghg_2 <- ghg %>% filter(depth==3) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_2")

ghg_3 <- ghg %>% filter(depth==6) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_3")

ghg_4 <- ghg %>% filter(depth==9) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_4")

ghg_5 <- ghg %>% filter(depth==11) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_5")

# Re-combine all depths then use spread to convert to wide form
ghg_merge <- rbind(ghg_1,ghg_2,ghg_3,ghg_4,ghg_5)
ghg_merge_ch4 <- ghg_merge %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
ghg_layers_ch4 <- ghg_merge_ch4 %>% spread(grouping,ch4_umol_L)

# Remove row 32 (only have 0.1m depth)
ghg_layers_ch4 <- ghg_layers_ch4[-c(32),]

# Calculate VW pCH4 for the entire water column
vw_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:45){
  vw_ch4[i] <- sum(ghg_layers_ch4$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                  ghg_layers_ch4$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                  ghg_layers_ch4$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                  ghg_layers_ch4$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                  ghg_layers_ch4$BVR_5[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])
}

# Need to re-calculate VW pCH4 when there are Na values
# Includes: 2016-04-02(2), 2017-03-17(31), Rows 38-45
vw_ch4[2] <- sum(ghg_layers_ch4$BVR_2[2]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                  ghg_layers_ch4$BVR_3[2]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                  ghg_layers_ch4$BVR_4[2]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                  ghg_layers_ch4$BVR_5[2]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])
vw_ch4[31] <- sum(ghg_layers_ch4$BVR_2[31]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                 ghg_layers_ch4$BVR_3[31]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                 ghg_layers_ch4$BVR_4[31]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                 ghg_layers_ch4$BVR_5[31]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])

for(i in 38:44){
  vw_ch4[i] <- sum(ghg_layers_ch4$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                   ghg_layers_ch4$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                   ghg_layers_ch4$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                   ghg_layers_ch4$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9],depth_2$LayerVol_m3[10]))/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_ch4[45] <- sum(ghg_layers_ch4$BVR_1[45]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                 ghg_layers_ch4$BVR_3[45]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                 ghg_layers_ch4$BVR_4[45]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9],depth_2$LayerVol_m3[10]))/sum(depth_2$LayerVol_m3[c(1:10)])

vw_ch4 <- cbind(ghg_layers_ch4,vw_ch4)

# Plot to check
ggplot(vw_ch4,aes(datetime,vw_ch4))+geom_line()+theme_classic()

### Calculate VW CH4 for Epi, Meta, and Hypo
### Calculate Epi VW CH4: defined as 0-4 m
vw_epi_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:length(ghg_layers_ch4$datetime)){
  vw_epi_ch4[i] <- sum(ghg_layers_ch4$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                      ghg_layers_ch4$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])
}

vw_epi_ch4[2] <- sum(ghg_layers_ch4$BVR_2[2]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])

vw_epi_ch4[31] <- sum(ghg_layers_ch4$BVR_2[31]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])

vw_epi_ch4[45] <- sum(ghg_layers_ch4$BVR_1[45]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])

### Calculate Meta VW CH4: defined as 4-7m
vw_meta_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:length(ghg_layers_ch4$datetime)){
  vw_meta_ch4[i] <- sum(ghg_layers_ch4$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]))/sum(depth_2$LayerVol_m3[c(5:7)])
}

### Calculate Hypo VW CH4: defined as 7-11m
vw_hypo_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:length(ghg_layers_ch4$datetime)){
  vw_hypo_ch4[i] <- sum(ghg_layers_ch4$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                       ghg_layers_ch4$BVR_5[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(8:10)])
}

for(i in 38:45){
  vw_hypo_ch4[i] <- sum(ghg_layers_ch4$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9],depth_2$LayerVol_m3[10]))/sum(depth_2$LayerVol_m3[c(8:10)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_ch4f <- cbind.data.frame(vw_ch4$datetime,vw_ch4$vw_ch4,vw_epi_ch4,vw_meta_ch4,vw_hypo_ch4)
names(vw_ch4f)[1] <- "DateTime"
names(vw_ch4f)[2] <- "WaterCol"
names(vw_ch4f)[3] <- "Epi"
names(vw_ch4f)[4] <- "Meta"
names(vw_ch4f)[5] <- "Hypo"

# Plot to check
ggplot(vw_ch4f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('CH4')+
  theme_classic()

# Export out Temp Data VW averaged by depth
write_csv(vw_ch4f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_ch4")

### Calculate VW CO2 for FCR
# Re-combine all depths then use spread to convert to wide form
ghg_merge_co2 <- ghg_merge %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
ghg_layers_co2 <- ghg_merge_co2 %>% spread(grouping,co2_umol_L)

# Remove row 32 (only have 0.1m depth)
ghg_layers_co2 <- ghg_layers_co2[-c(32),]

# Calculate VW pCH4 for the entire water column
vw_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:45){
  vw_co2[i] <- sum(ghg_layers_co2$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                   ghg_layers_co2$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                   ghg_layers_co2$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                   ghg_layers_co2$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                   ghg_layers_co2$BVR_5[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])
}

# Need to re-calculate VW pCH4 when there are Na values
# Includes: Rows: 2, 18, 26, 31, 38-45
vw_co2[2] <- sum(ghg_layers_co2$BVR_2[2]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                 ghg_layers_co2$BVR_3[2]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                 ghg_layers_co2$BVR_4[2]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                 ghg_layers_co2$BVR_5[2]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])

vw_co2[18] <- sum(ghg_layers_co2$BVR_1[18]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                 ghg_layers_co2$BVR_3[18]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                 ghg_layers_co2$BVR_4[18]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                 ghg_layers_co2$BVR_5[18]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])

vw_co2[26] <- sum(ghg_layers_co2$BVR_2[26]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                 ghg_layers_co2$BVR_3[26]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                 ghg_layers_co2$BVR_4[26]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                 ghg_layers_co2$BVR_5[26]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])

vw_co2[31] <- sum(ghg_layers_co2$BVR_2[31]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                  ghg_layers_co2$BVR_3[31]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                  ghg_layers_co2$BVR_4[31]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                  ghg_layers_co2$BVR_5[31]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(1:10)])

for(i in 38:44){
  vw_co2[i] <- sum(ghg_layers_co2$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                   ghg_layers_co2$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                   ghg_layers_co2$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                   ghg_layers_co2$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9],depth_2$LayerVol_m3[10]))/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_co2[45] <- sum(ghg_layers_co2$BVR_1[45]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2],depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                  ghg_layers_co2$BVR_3[45]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                  ghg_layers_co2$BVR_4[45]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9],depth_2$LayerVol_m3[10]))/sum(depth_2$LayerVol_m3[c(1:10)])

vw_co2 <- cbind(ghg_layers_co2,vw_co2)

# Plot to check
ggplot(vw_co2,aes(datetime,vw_co2))+geom_line()+theme_classic()

### Calculate VW CO2 for Epi, Meta, and Hypo
### Calculate Epi VW CO2: defined as 0-4 m
vw_epi_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_epi_co2[i] <- sum(ghg_layers_co2$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                       ghg_layers_co2$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])
}

vw_epi_co2[2] <- sum(ghg_layers_co2$BVR_2[2]*sum(depth_2$LayerVol_m3[1:4]))/sum(depth_2$LayerVol_m3[c(1:4)])

vw_epi_co2[18] <- sum(ghg_layers_co2$BVR_1[18]*sum(depth_2$LayerVol_m3[1:4]))/sum(depth_2$LayerVol_m3[c(1:4)])

vw_epi_co2[26] <- sum(ghg_layers_co2$BVR_2[26]*sum(depth_2$LayerVol_m3[1:4]))/sum(depth_2$LayerVol_m3[c(1:4)])

vw_epi_co2[31] <- sum(ghg_layers_co2$BVR_2[31]*sum(depth_2$LayerVol_m3[1:4]))/sum(depth_2$LayerVol_m3[c(1:4)])

vw_epi_co2[45] <- sum(ghg_layers_co2$BVR_1[45]*sum(depth_2$LayerVol_m3[1:4]))/sum(depth_2$LayerVol_m3[c(1:4)])

### Calculate Meta VW CO2: defined as 4-7m
vw_meta_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_meta_co2[i] <- sum(ghg_layers_co2$BVR_3[i]*sum(depth_2$LayerVol_m3[5:7]))/sum(depth_2$LayerVol_m3[c(5:7)])
}

### Calculate Hypo VW CO2: defined as 7-11m
vw_hypo_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_hypo_co2[i] <- sum(ghg_layers_co2$BVR_4[i]*sum(depth_2$LayerVol_m3[8],depth_2$LayerVol_m3[9]),
                        ghg_layers_co2$BVR_5[i]*depth_2$LayerVol_m3[10])/sum(depth_2$LayerVol_m3[c(8:10)])
}

for(i in 38:45){
  vw_hypo_co2[i] <- sum(ghg_layers_co2$BVR_4[i]*sum(depth_2$LayerVol_m3[8:10]))/sum(depth_2$LayerVol_m3[c(8:10)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_co2f <- cbind.data.frame(vw_co2$datetime,vw_co2$vw_co2,vw_epi_co2,vw_meta_co2,vw_hypo_co2)
names(vw_co2f)[1] <- "DateTime"
names(vw_co2f)[2] <- "WaterCol"
names(vw_co2f)[3] <- "Epi"
names(vw_co2f)[4] <- "Meta"
names(vw_co2f)[5] <- "Hypo"

# Plot to check
ggplot(vw_co2f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('CO2')+
  theme_classic()

# Export out Temp Data VW averaged by depth
write_csv(vw_co2f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_co2.csv")
