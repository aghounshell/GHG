### Script to calculate Epi, Meta, and Hypo volume weighted averaged Temp, DO, pCH4, and pCO2
### For BVR Summer 2016 and Summer 2017
### Defined Epi, Meta, and Hypo as follows:
###     Epi: 0.1 m + 3.0 m (0 - 4m)
###     Meta: 6.0 m (4 - 7m)
###     Hypo: 9.0 m + 11.0 m (7 - 11m)
### A Hounshell, 16Jul19
### Updated to include final BVR sampling in 2017, 01 Nov 2019
### Updated on 25 Nov 2019
###   Include 2018 data
###   Only use 9 m for WaterCol and Hypo calculations (too many Na's for 11 m!)

# RFile: VW_BVR_16to18

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load in data
# BVR Volumes by depth
depth <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/BVR_Vol.csv")
# BVR merged YSI and CTD casts
casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d", tz = "EST"))
# BVR GHG data from 2016-2017
ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_16_18_reps.csv")
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

# Plot averaged temperature for 'casts_10' (aka: > 9 m)
ggplot(casts_layers,aes(x=Date,y=BVR_10))+
  geom_line(aes(x=Date,y=BVR_10))+
  geom_point()+
  theme_classic()

# Separate by year and remove funky values (2017-07-06; 2017-10-08)
swi_16 <- casts_layers %>% filter(Date>=as.Date('2016-01-01')&Date<=as.Date('2016-12-31'))
swi_16$doy <- yday(swi_16$Date)
swi_17 <- casts_layers %>% filter(Date>=as.Date('2017-01-01')&Date<=as.Date('2017-12-31'))
swi_17$doy <- yday(swi_17$Date)
swi_18 <- casts_layers %>% filter(Date>=as.Date('2018-01-01')&Date<=as.Date('2018-12-31'))
swi_18$doy <- yday(swi_18$Date)

# Export out to compare to BVR
write_csv(swi_16, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_16_bvr.csv")
write_csv(swi_17, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_17_bvr.csv")
write_csv(swi_18, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_18_bvr.csv")

# Plot all years on same graph
ggplot()+
  geom_line(data=swi_16,aes(x=doy,y=BVR_10,color="2016"),size=1.1)+
  geom_point(data=swi_16,aes(x=doy,y=BVR_10,color="2016"),size=4)+
  geom_line(data=swi_17,aes(x=doy,y=BVR_10,color="2017"),size=1.1)+
  geom_point(data=swi_17,aes(x=doy,y=BVR_10,color="2017"),size=4)+
  geom_line(data=swi_18,aes(x=doy,y=BVR_10,color="2018"),size=1.1)+
  geom_point(data=swi_18,aes(x=doy,y=BVR_10,color="2018"),size=4)+
  geom_vline(xintercept = 315,linetype="dashed",color="#ffa700",size=1)+ #Turnover BVR 2016
  geom_vline(xintercept = 311,linetype="dashed",color="#ffa07a",size=1)+ #Turnover BVR 2017
  geom_vline(xintercept = 302,linetype="dashed",color="#F5793A",size=1)+ #Turnover BVR 2018
  scale_color_manual(breaks=c("2016","2017","2018"), labels=c("BVR 2016","BVR 2017","BVR 2018"),
                     values=c('#ffa700','#ffa07a','#F5793A'))+
  xlab("Day of Year")+
  labs(color="")+
  ylab("Temperature (deg C)")+
  theme_classic(base_size=15)

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
for(i in 1:length(casts_layers$Date)){
  vw_epi_temp[i] <- sum(casts_layers$BVR_1[i]*depth_2$LayerVol_m3[1],
                    casts_layers$BVR_2[i]*depth_2$LayerVol_m3[2],
                    casts_layers$BVR_3[i]*depth_2$LayerVol_m3[3],
                    casts_layers$BVR_4[i]*depth_2$LayerVol_m3[4])/sum(depth_2$LayerVol_m3[c(1:4)])
}

### Calculate Meta VW Temp: defined as 4-7m
vw_meta_temp <- rep(-99,length(casts_layers$Date))
for(i in 1:length(casts_layers$Date)){
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

vw_tempf_gather <- vw_tempf %>% gather(key=depth,value=vw_temp,-DateTime)
vw_tempf_gather16 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_tempf_gather17 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_tempf_gather18 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))
vw_tempf_gather$depth<-factor(vw_tempf_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))

# Plot to check
temp16 <- ggplot(vw_tempf_gather16,aes(x = DateTime, y = vw_temp, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-11-11"))+ #Turnover
  labs(color="")+
  xlab('2016')+
  ylab(expression('Temperature ('*degree*C*')'))+
  ylim(0,30)+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  theme_classic(base_size = 15)

temp17 <- ggplot(vw_tempf_gather17,aes(x = DateTime, y = vw_temp, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-11-07"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression('Temperature ('*degree*C*')'))+
  ylim(0,30)+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  theme_classic(base_size = 15)

temp18 <- ggplot(vw_tempf_gather18,aes(x = DateTime, y = vw_temp, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-10-29"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression('Temperature ('*degree*C*')'))+
  ylim(0,30)+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(temp16,temp17,temp18,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_tempf, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_Temp_16to18.csv")

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
for(i in 1:length(casts_layers_o2$Date)){
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
for(i in 1:length(casts_layers_o2$Date)){
  vw_epi_o2[i] <- sum(casts_layers_o2$BVR_1[i]*depth_2$LayerVol_m3[1],
                        casts_layers_o2$BVR_2[i]*depth_2$LayerVol_m3[2],
                        casts_layers_o2$BVR_3[i]*depth_2$LayerVol_m3[3],
                        casts_layers_o2$BVR_4[i]*depth_2$LayerVol_m3[4])/sum(depth_2$LayerVol_m3[c(1:4)])
}

### Calculate Meta VW O2: defined as 4-7m
vw_meta_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
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

vw_o2f_gather <- vw_o2f %>% gather(key=depth,value=vw_o2,-DateTime)
vw_o2f_gather16 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_o2f_gather17 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_o2f_gather18 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))
vw_o2f_gather$depth<-factor(vw_o2f_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))

# Plot to check
o16 <- ggplot(vw_o2f_gather16,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-11-11"))+ #Turnover
  labs(color="")+
  xlab('2016')+
  ylab(expression("DO (mg L"^-1*")"))+
  ylim(0,17)+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  theme_classic(base_size = 15)

o17 <- ggplot(vw_o2f_gather17,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-11-07"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression("DO (mg L"^-1*")"))+
  ylim(0,17)+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  theme_classic(base_size = 15)

o18 <- ggplot(vw_o2f_gather18,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-10-29"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression("DO (mg L"^-1*")"))+
  ylim(0,17)+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(o16,o17,o18,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_o2f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_o2_16to18.csv")

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

# Remove dates with all Na values at the beginning
ghg <- ghg[-c(1:20),]

# Separate by depth then recombine
ghg_1_rep1 <- ghg %>% filter(depth==0.1,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_1")
ghg_1_rep1$ch4_umol_L <- na.approx(ghg_1_rep1$ch4_umol_L)
ghg_1_rep1$co2_umol_L <- na.approx(ghg_1_rep1$co2_umol_L)

ghg_1_rep2 <- ghg %>% filter(depth==0.1,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_1")
ghg_1_rep2$ch4_umol_L <- na.approx(ghg_1_rep2$ch4_umol_L)
ghg_1_rep2$co2_umol_L <- na.approx(ghg_1_rep2$co2_umol_L)

ghg_2_rep1 <- ghg %>% filter(depth==3,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_2")
ghg_2_rep1$ch4_umol_L <- na.approx(ghg_2_rep1$ch4_umol_L)
ghg_2_rep1$co2_umol_L <- na.approx(ghg_2_rep1$co2_umol_L)

ghg_2_rep2 <- ghg %>% filter(depth==3,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_2")
ghg_2_rep2$ch4_umol_L <- na.approx(ghg_2_rep2$ch4_umol_L)
ghg_2_rep2$co2_umol_L <- na.approx(ghg_2_rep2$co2_umol_L)

ghg_3_rep1 <- ghg %>% filter(depth==6,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_3")
ghg_3_rep1$ch4_umol_L <- na.approx(ghg_3_rep1$ch4_umol_L)
ghg_3_rep1$co2_umol_L <- na.approx(ghg_3_rep1$co2_umol_L)

ghg_3_rep2 <- ghg %>% filter(depth==6,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_3")
ghg_3_rep2$ch4_umol_L <- na.approx(ghg_3_rep2$ch4_umol_L)
ghg_3_rep2$co2_umol_L <- na.approx(ghg_3_rep2$co2_umol_L)

ghg_4_rep1 <- ghg %>% filter(depth==9,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_4")
ghg_4_rep1$ch4_umol_L <- na.approx(ghg_4_rep1$ch4_umol_L)
ghg_4_rep1$co2_umol_L <- na.approx(ghg_4_rep1$co2_umol_L)

ghg_4_rep2 <- ghg %>% filter(depth==9,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_4")
ghg_4_rep2$ch4_umol_L <- na.approx(ghg_4_rep2$ch4_umol_L)
ghg_4_rep2$co2_umol_L <- na.approx(ghg_4_rep2$co2_umol_L)

ghg_5_rep1 <- ghg %>% filter(depth==11,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_5")
ghg_5_rep1$ch4_umol_L <- na.approx(ghg_5_rep1$ch4_umol_L)
ghg_5_rep1$co2_umol_L <- na.approx(ghg_5_rep1$co2_umol_L)

ghg_5_rep2 <- ghg %>% filter(depth==11,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_5")
ghg_5_rep2$ch4_umol_L <- na.approx(ghg_5_rep2$ch4_umol_L)
ghg_5_rep2$co2_umol_L <- na.approx(ghg_5_rep2$co2_umol_L)

# Re-combine reps and separate by co2 and ch4
ghg_merge_rep1 <- rbind(ghg_1_rep1,ghg_2_rep1,ghg_3_rep1,ghg_4_rep1,ghg_5_rep1)
ghg_merge_rep2 <- rbind(ghg_1_rep2,ghg_2_rep2,ghg_3_rep2,ghg_4_rep2,ghg_5_rep2)

rep1_co2 <- ghg_merge_rep1 %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
rep1_co2_layers <- rep1_co2 %>% spread(grouping,co2_umol_L)
rep1_co2_layers$BVR_5 <- na.approx(rep1_co2_layers$BVR_5,na.rm=FALSE)

rep2_co2 <- ghg_merge_rep2 %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
rep2_co2_layers <- rep2_co2 %>% spread(grouping,co2_umol_L)
rep2_co2_layers$BVR_5 <- na.approx(rep2_co2_layers$BVR_5,na.rm=FALSE)

rep1_ch4 <- ghg_merge_rep1 %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
rep1_ch4_layers <- rep1_ch4 %>% spread(grouping,ch4_umol_L)
rep1_ch4_layers$BVR_5 <- na.approx(rep1_ch4_layers$BVR_5,na.rm=FALSE)

rep2_ch4 <- ghg_merge_rep2 %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
rep2_ch4_layers <- rep2_ch4 %>% spread(grouping,ch4_umol_L)
rep2_ch4_layers$BVR_5 <- na.approx(rep2_ch4_layers$BVR_5,na.rm=FALSE)

# Remove row 31 (only have 0.1m depth)
rep1_co2_layers <- rep1_co2_layers[-c(31),]
rep2_co2_layers <- rep2_co2_layers[-c(31),]
rep1_ch4_layers <- rep1_ch4_layers[-c(31),]
rep2_ch4_layers <- rep2_ch4_layers[-c(31),]

# Calculate VW pCH4 for the entire water column: calculate reps 1 and 2 separately
### DON'T USE DATA FROM 11 M (too many Na's; use 9 m for Hypo)
vw_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  vw_ch4_rep1[i] <- sum(rep1_ch4_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                        rep1_ch4_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                        rep1_ch4_layers$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                        rep1_ch4_layers$BVR_4[i]*sum(depth_2$LayerVol_m3[c(8:10)]))/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_ch4_rep1 <- cbind(rep1_ch4_layers,vw_ch4_rep1)

# Plot to check
ggplot(vw_ch4_rep1,aes(datetime,vw_ch4_rep1))+geom_line()+theme_classic()

# Calculate for Rep 2
vw_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  vw_ch4_rep2[i] <- sum(rep2_ch4_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                        rep2_ch4_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                        rep2_ch4_layers$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                        rep2_ch4_layers$BVR_4[i]*sum(depth_2$LayerVol_m3[c(8:10)]))/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_ch4_rep2 <- cbind(rep2_ch4_layers,vw_ch4_rep2)

# Plot to check
ggplot(vw_ch4_rep2,aes(datetime,vw_ch4_rep2))+geom_line()+theme_classic()

# Calculate Avg and Stdev for Water Col VW
vw_ch4 <- cbind(vw_ch4_rep1$vw_ch4_rep1,vw_ch4_rep2$vw_ch4_rep2)
names(vw_ch4)[1] <- "ch4_vw_rep1"
names(vw_ch4)[2] <- "ch4_vw_rep2"

vw_ch4_avg <- rowMeans(vw_ch4,na.rm=TRUE)
vw_ch4_stdev <- apply(vw_ch4,1,FUN=sd,na.rm=TRUE)

vw_ch4_all <- cbind.data.frame(vw_ch4_rep1$datetime,vw_ch4_avg,vw_ch4_stdev)
names(vw_ch4_all)[1] <- "datetime"
names(vw_ch4_all)[2] <- "vw_avg"
names(vw_ch4_all)[3] <- "vw_std"

# Plot to check: avg and stdev
ggplot(vw_ch4_all,aes(datetime,vw_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=vw_avg-vw_std, ymax=vw_avg+vw_std))+
  theme_classic()

### Calculate VW CH4 for Epi, Meta, and Hypo
### Calculate Epi VW CH4: defined as 0-4 m
epi_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  epi_ch4_rep1[i] <- sum(rep1_ch4_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                         rep1_ch4_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])
}

# Calculate Epi Rep 2
epi_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  epi_ch4_rep2[i] <- sum(rep2_ch4_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                         rep2_ch4_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])
}

epi_ch4 <- cbind(epi_ch4_rep1,epi_ch4_rep2)
epi_ch4_avg <- rowMeans(epi_ch4,na.rm=TRUE)
epi_ch4_stdev <- apply(epi_ch4,1,FUN=sd,na.rm=TRUE)

vw_ch4_all <- cbind.data.frame(vw_ch4_all,epi_ch4_avg,epi_ch4_stdev)
names(vw_ch4_all)[4] <- "epi_avg"
names(vw_ch4_all)[5] <- "epi_std"

# Plot to check
ggplot(vw_ch4_all,aes(datetime,epi_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=epi_avg-epi_std, ymax=epi_avg+epi_std))+
  theme_classic()

### Calculate Meta VW CH4: defined as 4-7m
# Calculate for Rep 1 and Rep 2 then avg and stdev
meta_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  meta_ch4_rep1[i] <- rep1_ch4_layers$BVR_3[i]
}

# Meta CH4 Rep 2
meta_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  meta_ch4_rep2[i] <- rep2_ch4_layers$BVR_3[i]
}

meta_ch4 <- cbind(meta_ch4_rep1,meta_ch4_rep2)
meta_ch4_avg <- rowMeans(meta_ch4,na.rm=TRUE)
meta_ch4_stdev <- apply(meta_ch4,1,FUN=sd,na.rm=TRUE)

vw_ch4_all <- cbind.data.frame(vw_ch4_all,meta_ch4_avg,meta_ch4_stdev)
names(vw_ch4_all)[6] <- "meta_avg"
names(vw_ch4_all)[7] <- "meta_std"

# Plot to check
ggplot(vw_ch4_all,aes(datetime,meta_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=meta_avg-meta_std, ymax=meta_avg+meta_std))+
  theme_classic()

### Calculate Hypo VW CH4: defined as 7-11m
### USE 9M ONLY
hypo_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  hypo_ch4_rep1[i] <- rep1_ch4_layers$BVR_4[i]
}

# Hypo Rep 2
hypo_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  hypo_ch4_rep2[i] <- rep2_ch4_layers$BVR_4[i]
}

hypo_ch4 <- cbind(hypo_ch4_rep1,hypo_ch4_rep2)
hypo_ch4_avg <- rowMeans(hypo_ch4,na.rm=TRUE)
hypo_ch4_stdev <- apply(hypo_ch4,1,FUN=sd,na.rm=TRUE)

vw_ch4_all <- cbind.data.frame(vw_ch4_all,hypo_ch4_avg,hypo_ch4_stdev)
names(vw_ch4_all)[8] <- "hypo_avg"
names(vw_ch4_all)[9] <- "hypo_std"

# Plot to check
ggplot(vw_ch4_all,aes(datetime,hypo_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=hypo_avg-hypo_std, ymax=hypo_avg+hypo_std))+
  theme_classic()

## Separate into years (2016; 2017; 2018): width = 1200; height = 300
vw_ch4_avg <- vw_ch4_all %>% select(datetime,vw_avg,epi_avg,meta_avg,hypo_avg)
names(vw_ch4_avg)[2] <- 'WaterCol'
names(vw_ch4_avg)[3] <- 'Epi'
names(vw_ch4_avg)[4] <- 'Meta'
names(vw_ch4_avg)[5] <- 'Hypo'

vw_ch4_avg_long <- vw_ch4_avg %>% pivot_longer(-datetime,names_to="depth",values_to="ch4_avg")

vw_ch4_std <- vw_ch4_all %>% select(datetime,vw_std,epi_std,meta_std,hypo_std)
names(vw_ch4_std)[2] <- 'WaterCol'
names(vw_ch4_std)[3] <- 'Epi'
names(vw_ch4_std)[4] <- 'Meta'
names(vw_ch4_std)[5] <- 'Hypo'

vw_ch4_std_long <- vw_ch4_std %>% pivot_longer(-datetime,names_to="depth",values_to="ch4_std")

vw_ch4_long <- merge(vw_ch4_avg_long,vw_ch4_std_long,by=c("datetime","depth"))

vw_ch4_16 <- vw_ch4_long %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_ch4_17 <- vw_ch4_long %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_ch4_18 <- vw_ch4_long %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

# Remove rows with NA averages (but not SD!)
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

vw_ch4_16 <- completeFun(vw_ch4_16,"ch4_avg")
vw_ch4_17 <- completeFun(vw_ch4_17,"ch4_avg")
vw_ch4_18 <- completeFun(vw_ch4_18,"ch4_avg")

ch416 <- ggplot(vw_ch4_16,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-11-11"))+ #Turnover
  labs(color="")+
  xlab('2016')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  theme_classic(base_size = 15)

ch417 <- ggplot(vw_ch4_17,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2017-11-07"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  theme_classic(base_size = 15)

ch418 <- ggplot(vw_ch4_18,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2018-10-29"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(ch416,ch417,ch418,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_ch4_all, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_ch4_stats_16to18.csv")

### Calculate VW CO2 for FCR
# Calculate VW pCH4 for the entire water column
# USE 9 M ONLY FOR HYPO CALCULATIONS
vw_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  vw_co2_rep1[i] <- sum(rep1_co2_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                        rep1_co2_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                        rep1_co2_layers$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                        rep1_co2_layers$BVR_4[i]*sum(depth_2$LayerVol_m3[c(8:10)]))/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_co2_rep1 <- cbind(rep1_co2_layers,vw_co2_rep1)

# Plot to check
ggplot(vw_co2_rep1,aes(datetime,vw_co2_rep1))+geom_line()+theme_classic()

# Calculate for Rep 2
vw_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  vw_co2_rep2[i] <- sum(rep2_co2_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                        rep2_co2_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]),
                        rep2_co2_layers$BVR_3[i]*sum(depth_2$LayerVol_m3[5],depth_2$LayerVol_m3[6],depth_2$LayerVol_m3[7]),
                        rep2_co2_layers$BVR_4[i]*sum(depth_2$LayerVol_m3[c(8:10)]))/sum(depth_2$LayerVol_m3[c(1:10)])
}

vw_co2_rep2 <- cbind(rep2_co2_layers,vw_co2_rep2)

# Plot to check
ggplot(vw_co2_rep2,aes(datetime,vw_co2_rep2))+geom_line()+theme_classic()

# Calculate Avg and Stdev for Water Col VW
vw_co2 <- cbind(vw_co2_rep1$vw_co2_rep1,vw_co2_rep2$vw_co2_rep2)
names(vw_co2)[1] <- "co2_vw_rep1"
names(vw_co2)[2] <- "co2_vw_rep2"

vw_co2_avg <- rowMeans(vw_co2,na.rm=TRUE)
vw_co2_stdev <- apply(vw_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_rep1$datetime,vw_co2_avg,vw_co2_stdev)
names(vw_co2_all)[1] <- "datetime"
names(vw_co2_all)[2] <- "vw_avg"
names(vw_co2_all)[3] <- "vw_std"

# Plot to check: avg and stdev
ggplot(vw_co2_all,aes(datetime,vw_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=vw_avg-vw_std, ymax=vw_avg+vw_std))+
  theme_classic()

### Calculate VW CO2 for Epi, Meta, and Hypo
### Calculate Epi VW CO2: defined as 0-4 m
epi_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  epi_co2_rep1[i] <- sum(rep1_co2_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                       rep1_co2_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])
}

# Calculate for Rep 2
epi_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  epi_co2_rep2[i] <- sum(rep2_co2_layers$BVR_1[i]*sum(depth_2$LayerVol_m3[1],depth_2$LayerVol_m3[2]),
                         rep2_co2_layers$BVR_2[i]*sum(depth_2$LayerVol_m3[3],depth_2$LayerVol_m3[4]))/sum(depth_2$LayerVol_m3[c(1:4)])
}

epi_co2 <- cbind(epi_co2_rep1,epi_co2_rep2)
epi_co2_avg <- rowMeans(epi_co2,na.rm=TRUE)
epi_co2_stdev <- apply(epi_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_all,epi_co2_avg,epi_co2_stdev)
names(vw_co2_all)[4] <- "epi_avg"
names(vw_co2_all)[5] <- "epi_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,epi_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=epi_avg-epi_std, ymax=epi_avg+epi_std))+
  theme_classic()

### Calculate Meta VW CO2: defined as 4-7m
# Calculate for Rep 1
meta_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  meta_co2_rep1[i] <- sum(rep1_co2_layers$BVR_3[i]*sum(depth_2$LayerVol_m3[5:7]))/sum(depth_2$LayerVol_m3[c(5:7)])
}

# Calculate for Rep 2
meta_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  meta_co2_rep2[i] <- sum(rep2_co2_layers$BVR_3[i]*sum(depth_2$LayerVol_m3[5:7]))/sum(depth_2$LayerVol_m3[c(5:7)])
}

meta_co2 <- cbind(meta_co2_rep1,meta_co2_rep2)
meta_co2_avg <- rowMeans(meta_co2)
meta_co2_stdev <- apply(meta_co2,1,FUN=sd)

vw_co2_all <- cbind.data.frame(vw_co2_all,meta_co2_avg,meta_co2_stdev)
names(vw_co2_all)[6] <- "meta_avg"
names(vw_co2_all)[7] <- "meta_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,meta_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=meta_avg-meta_std, ymax=meta_avg+meta_std))+
  theme_classic()

### Calculate Hypo VW CO2: defined as 7-11m
### USE ONLY 9M
# Calculate Rep 1
hypo_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  hypo_co2_rep1[i] <- rep1_co2_layers$BVR_4[i]
}

# Calculate Rep 2
hypo_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  hypo_co2_rep2[i] <- rep2_co2_layers$BVR_4[i]
}

hypo_co2 <- cbind(hypo_co2_rep1,hypo_co2_rep2)
hypo_co2_avg <- rowMeans(hypo_co2,na.rm=TRUE)
hypo_co2_stdev <- apply(hypo_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_all,hypo_co2_avg,hypo_co2_stdev)
names(vw_co2_all)[8] <- "hypo_avg"
names(vw_co2_all)[9] <- "hypo_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,hypo_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=hypo_avg-hypo_std, ymax=hypo_avg+hypo_std))+
  theme_classic()

## Separate into years (2016; 2017; 2018): width = 1200; height = 300
vw_co2_avg <- vw_co2_all %>% select(datetime,vw_avg,epi_avg,meta_avg,hypo_avg)
names(vw_co2_avg)[2] <- 'WaterCol'
names(vw_co2_avg)[3] <- 'Epi'
names(vw_co2_avg)[4] <- 'Meta'
names(vw_co2_avg)[5] <- 'Hypo'

vw_co2_avg_long <- vw_co2_avg %>% pivot_longer(-datetime,names_to="depth",values_to="co2_avg")

vw_co2_std <- vw_co2_all %>% select(datetime,vw_std,epi_std,meta_std,hypo_std)
names(vw_co2_std)[2] <- 'WaterCol'
names(vw_co2_std)[3] <- 'Epi'
names(vw_co2_std)[4] <- 'Meta'
names(vw_co2_std)[5] <- 'Hypo'

## Calculate Hypo 'averaged' concentration over each summer
# First select the correct date range for each summer
# April 1 - Turnover
## Re-use for later!!!
vw_co2_avg_16 <- vw_co2_avg %>% filter(datetime>=as.Date('2016-04-01')&
                                                  datetime<=as.Date('2016-11-11'))
vw_co2_avg_17 <- vw_co2_avg %>% filter(datetime>=as.Date('2017-04-01')&
                                                  datetime<=as.Date('2017-11-07'))

vw_ch4_avg_16 <- vw_ch4_avg %>% filter(datetime>=as.Date('2016-04-01')&datetime<=as.Date('2016-11-11'))
vw_ch4_avg_17 <- vw_ch4_avg %>% filter(datetime>=as.Date('2017-04-01')&datetime<=as.Date('2017-11-07'))

vw_hypo_co2_16 <- sum(vw_co2_avg_16$Hypo)/length(vw_co2_avg_16$Hypo)
vw_hypo_co2_17 <- sum(vw_co2_avg_17$Hypo)/length(vw_co2_avg_17$Hypo)
vw_hypo_ch4_16 <- sum(vw_ch4_avg_16$Hypo)/length(vw_ch4_avg_16$Hypo)
vw_hypo_ch4_17 <- sum(vw_ch4_avg_17$Hypo)/length(vw_ch4_avg_17$Hypo)

# Begin plotting again
vw_co2_std_long <- vw_co2_std %>% pivot_longer(-datetime,names_to="depth",values_to="co2_std")

vw_co2_long <- merge(vw_co2_avg_long,vw_co2_std_long,by=c("datetime","depth"))

vw_co2_16 <- vw_co2_long %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_co2_17 <- vw_co2_long %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_co2_18 <- vw_co2_long %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

co216 <- ggplot(vw_co2_16,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-11-11"))+ #Turnover
  labs(color="")+
  xlab('2016')+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  ylim(0,700)+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  theme_classic(base_size = 15)

co217 <- ggplot(vw_co2_17,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-11-07"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  ylim(0,700)+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  theme_classic(base_size=15)

co218 <- ggplot(vw_co2_18,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-10-29"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  ylim(0,700)+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size=15)

ggarrange(co216,co217,co218,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_co2_all, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_co2_stats_16to18.csv")
