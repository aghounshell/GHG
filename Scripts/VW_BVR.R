### Script to calculate volume weighted parameters for BVR 2016-2017
### For: Temp (as a check), Oxygen, dissolved CO2, and dissolved CH4
### A Hounshell, 21 Jun 2019

### NOTE (07/15/19): Ultimately decided to designate (based on the depth locations of the GHG samples 
### collected):
#         Epi = 0.1 m + 3 m
#         Meta = 6 m
#         Hypo = 9 m + 11 m
# Will likely average Temp and DO across these same depth ranges for consistency

# Load libraries needed
pacman::p_load(tidyverse,ggplot2)

# Load in data
# BVR Volumes by depth
depth <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data/BVR_Vol.csv")
# BVR Thermocline data (as calculated by Lake Analyzer)
thermo <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data/BVR_Thermo.csv")
thermo$DateTime <- as.POSIXct(strptime(thermo$DateTime, "%Y-%m-%d", tz = "EST"))
# BVR merged YSI and CTD casts
casts <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/BVR_CTDysi_merge.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d %H:%M", tz = "EST"))
# BVR GHG data from 2016-2017
ghg <- read_csv("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_BVR_16_17.csv")

### Start with temperature as a check...
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
names(vw_temp)[1] <- "DateTime"

# Check vw_temp values
ggplot(vw_temp,aes(DateTime,vw_temp))+geom_line()+theme_classic()

## Calculate VW Temp values for the: Epi, Meta, and Hypo using variable thermocline 
## depths
# First round to the nearest whole depth (i.e., 1 m) for the Upper and Lower Meta
thermo$SmetaB_round <- round(thermo$SmetaB_m,0)
thermo$SmetaT_round <- round(thermo$SmetaT_m,0)

# Merge the thermocline data from Lake Analyzer and vw_temp data by date
# First remove time associated with datetime for vw_temp
vw_temp2$Date <- as.POSIXct(strptime(vw_temp2$Date, "%Y-%m-%d", tz = "EST"))
names(vw_temp2)[1] <- "DateTime"
vw_temp3 <- aggregate(vw_temp2, by=list(vw_temp2$DateTime), FUN = "mean")
date_merge <- merge(thermo, vw_temp3, by="DateTime", all.x=TRUE, all.y=TRUE)

# Tell R to match the meta Top to the correct depth and volume weight above that depth
# First, when water column is completely mixed, set metaTop and metaBottom to 10 m
thermo$SmetaT_round[33]=10
thermo$SmetaB_round[33]=10
thermo$SmetaT_round[34]=10
thermo$SmetaB_round[34]=10
thermo$SmetaT_round[57]=10
thermo$SmetaB_round[57]=10
vw_temp4 <- select(vw_temp3,starts_with("BVR"))
vw_epi_temp <- matrix(rep(-99),nrow=57,ncol=10)
vw_epi_temp2 <- matrix(rep(-99),nrow=57,ncol=1)
for (i in 1:length(vw_temp4$BVR_1)){
  for (j in 1:thermo$SmetaT_round[i]){
    vw_epi_temp[i,j] = vw_temp4[i,j]*depth_2$LayerVol_m3[j]
    vw_epi_temp2[i] = sum(vw_epi_temp[i,1:j])/sum(depth_2$LayerVol_m3[1:j])
  }
}

# Combine 'final' matrix with date, VW temp for water column, VW temp for epi, meta, and hypo
vw_tempf <- cbind.data.frame(vw_temp3$Group.1,vw_temp3$vw_temp,vw_epi_temp2)
names(vw_tempf)[1] <- "DateTime"
names(vw_tempf)[2] <- "WaterCol"
names(vw_tempf)[3] <- "Epi"

# Plot current data, just to check...
ggplot(vw_tempf,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  theme_classic()

# Calculate VW Temp for the Metalimnion - will need to be bounded by the MetaTop and MetaBottom
vw_meta_temp <- matrix(rep(-99),nrow=57,ncol=10)
vw_meta_temp2 <- matrix(rep(-99),nrow=57,ncol=1)
for (i in 1:57){
  for (j in thermo$SmetaT_round[i]:thermo$SmetaB_round[i]){
    vw_meta_temp[i,j] = vw_temp4[i,j]*depth_2$LayerVol_m3[j]
    vw_meta_temp2[i] = sum(vw_meta_temp[i,thermo$SmetaT_round[i]:thermo$SmetaB_round[i]])/sum(depth_2$LayerVol_m3[thermo$SmetaT_round[i]:thermo$SmetaB_round[i]])
  }
}

# Add VW_Meta_Temp to final data matrix
vw_tempf$Meta <- vw_meta_temp2

# Plot current data to check...
ggplot(vw_tempf,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  theme_classic()

# Now calculate Hypo VW Temp
vw_hypo_temp <- matrix(rep(-99),nrow=57,ncol=10)
vw_hypo_temp2 <- matrix(rep(-99),nrow=57,ncol=1)
for (i in 1:length(vw_temp4$BVR_1)){
  for (j in thermo$SmetaB_round[i]:10){
    vw_hypo_temp[i,j] = vw_temp4[i,j]*depth_2$LayerVol_m3[j]
    vw_hypo_temp2[i] = sum(vw_hypo_temp[i,j:10])/sum(depth_2$LayerVol_m3[j:10])
  }
}

# Add VW_Hypo_Temp to final data matrix
vw_tempf$Hypo <- vw_hypo_temp2

# Plot current data to check...
vw_temp <- ggplot(vw_tempf,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('Temp')+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/vw_temp.jpg",vw_temp,width=15,height=10)

### Now need to do the same for oxygen - make sure the Epi, Meta, and Hypo designations are accurately
### capturing oxygen dynamics in BVR

# Re-combine all casts then use spread to convert to wide form selecting for oxygen
casts_merge_o2 <- rbind(casts_1,casts_2,casts_3,casts_4,casts_5,casts_6,casts_7,casts_8,
                     casts_9,casts_10)
casts_merge_o2 <- casts_merge_o2 %>% select(Date,DO_mgL,grouping) %>% arrange(Date)
casts_layers_o2 <- casts_merge_o2 %>% spread(grouping,DO_mgL)

# Change columns to increase from smallest to largest depth
casts_layers_o2 <- casts_layers_o2[,c(1:2,4:11,3)]

# Select complete cases
casts_layers_o2 <- na.omit(casts_layers_o2)

# Calculate volume weighted average temperature for the entire water column
# First, average when mulitple casts were conducted on the same day
casts_layers_o2$Date <- as.POSIXct(strptime(casts_layers_o2$Date, "%Y-%m-%d", tz = "EST"))
casts_layers_o2 <- aggregate(casts_layers_o2, by=list(casts_layers_o2$Date), FUN = "mean")

# Use for loop to calculate volume weighted averages for each time point
vw_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:57){
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

# Check vw_temp values
ggplot(vw_o2,aes(DateTime,vw_o2))+geom_line()+theme_classic()

## Calculate VW O2 values for the: Epi, Meta, and Hypo using variable thermocline 
## depths
vw_o2_2 <- select(vw_o2,starts_with("BVR"))
vw_epi_o2 <- matrix(rep(-99),nrow=57,ncol=10)
vw_epi_o2_2 <- matrix(rep(-99),nrow=57,ncol=1)
for (i in 1:57){
  for (j in 1:thermo$SmetaT_round[i]){
    vw_epi_o2[i,j] = vw_o2_2[i,j]*depth_2$LayerVol_m3[j]
    vw_epi_o2_2[i] = sum(vw_epi_o2[i,1:j])/sum(depth_2$LayerVol_m3[1:j])
  }
}

# Combine 'final' matrix with date, VW O2 for water column, VW O2 for epi, meta, and hypo
vw_o2_f <- cbind.data.frame(vw_o2$DateTime,vw_o2$vw_o2,vw_epi_o2_2)
names(vw_o2_f)[1] <- "DateTime"
names(vw_o2_f)[2] <- "WaterCol"
names(vw_o2_f)[3] <- "Epi"

# Plot current data, just to check...
ggplot(vw_o2_f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  theme_classic()

# Calculate VW Temp for the Metalimnion - will need to be bounded by the MetaTop and MetaBottom
vw_meta_o2 <- matrix(rep(-99),nrow=57,ncol=10)
vw_meta_o2_2 <- matrix(rep(-99),nrow=57,ncol=1)
for (i in 1:57){
  for (j in thermo$SmetaT_round[i]:thermo$SmetaB_round[i]){
    vw_meta_o2[i,j] = vw_o2_2[i,j]*depth_2$LayerVol_m3[j]
    vw_meta_o2_2[i] = sum(vw_meta_o2[i,thermo$SmetaT_round[i]:thermo$SmetaB_round[i]])/sum(depth_2$LayerVol_m3[thermo$SmetaT_round[i]:thermo$SmetaB_round[i]])
  }
}

# Add VW_Meta_Temp to final data matrix
vw_o2_f$Meta <- vw_meta_o2_2

# Plot current data to check...
ggplot(vw_o2_f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  theme_classic()

# Now calculate Hypo VW Temp
vw_hypo_o2 <- matrix(rep(-99),nrow=57,ncol=10)
vw_hypo_o2_2 <- matrix(rep(-99),nrow=57,ncol=1)
for (i in 1:length(vw_o2_2$BVR_1)){
  for (j in thermo$SmetaB_round[i]:10){
    vw_hypo_o2[i,j] = vw_o2_2[i,j]*depth_2$LayerVol_m3[j]
    vw_hypo_o2_2[i] = sum(vw_hypo_o2[i,j:10])/sum(depth_2$LayerVol_m3[j:10])
  }
}

# Add VW_Hypo_Temp to final data matrix
vw_o2_f$Hypo <- vw_hypo_o2_2

# Plot current data to check...
vw_o2 <- ggplot(vw_o2_f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('Temp')+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/vw_o2.jpg",vw_o2,width=15,height=10)

### Now calculate dissolved GHG (CO2 and CH4) for water column, epi, meta, hypo using variable depths
### Start with dissolved CO2
# But first, need to get the GHG data in order
# Going to be difficult to accurately capture the Meta: GHG data was only collected at 0.1, 3, 6, 9, 11 m
# Meta is often situated between these depths (aka: don't actually have a 'meta' sample)

# Let's think about this: plot the depth of the Meta Top and Bottom with time
thermo$DateTime <- vw_o2_f$DateTime

ggplot(thermo,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = SmetaT_round, col="SmetaT_round"))+
  geom_line(aes(y = SmetaB_round, col="SmetaB_round"))+
  geom_hline(yintercept = 3)+
  geom_hline(yintercept = 6)+
  geom_hline(yintercept = 9)+
  ylim(0,10)+
  theme_classic()
