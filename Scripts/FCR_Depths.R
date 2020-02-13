### Script to calculate Epi, Meta, and Hypo volume weighted averaged Temp, DO, pCH4, and pCO2
### For FCR Summer 2016 and Summer 2017
### Defined Epi, Meta, and Hypo as follows:
###     Epi: 0.1 m + 1.6 m (0 - 2.6m)
###     Meta: 3.8 m + 5.0 m (2.6 - 6.5m)
###     Hypo: 6.2 m + 8.0 m + 9.0 m (6.5 - 9m)
### A Hounshell, 22Jul19
### Updated: 20Nov19
###   Include Summer 2018: updated CTD/YSI casts and GHG data
###   Need to remove dates with all Na's from GHG data

## RFile: VW_FCR_16to18

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load in data
# FCR Volumes by depth
depth <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Vol.csv")
# FCR merged YSI and CTD casts
casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d", tz = "EST"))
# FCR GHG data from 2016-2017: includes reps
ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_16_18_reps.csv")
ghg$datetime <- as.POSIXct(strptime(ghg$datetime, "%Y-%m-%d", tz="EST"))

### Start with temperature as a check...(code modified from VW_BVR.R)
## Calculate volume weighted averages for the entire water column
# First: average concentrations between depth intervals (depth intervals
# used to calculate the volume in FCR)
# Also average across casts when multiple casts/day
casts_1 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=0 & Depth_m<1.3) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_1")

casts_2 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=1.3 & Depth_m<2.9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_2")

casts_3 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=2.9 & Depth_m<4.4) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_3")

casts_4 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=4.4 & Depth_m<5.9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_4")

casts_5 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=5.9 & Depth_m<6.8) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_5")

casts_6 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=6.8 & Depth_m<8.7) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_6")

casts_7 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=8.7 & Depth_m<9.6) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_7")

# Re-combine depths as well
depth_2 <- rep(-99,7)
depth_2[1] <- sum(depth$`Volume (L)`[1:3])
depth_2[2] <- sum(depth$`Volume (L)`[4:8])
depth_2[3] <- sum(depth$`Volume (L)`[9:13])
depth_2[4] <- sum(depth$`Volume (L)`[14:18])
depth_2[5] <- sum(depth$`Volume (L)`[19:21])
depth_2[6] <- sum(depth$`Volume (L)`[22:27])
depth_2[7] <- sum(depth$`Volume (L)`[28:31])

# Re-combine all casts then use spread to convert to wide form
casts_merge <- rbind(casts_1,casts_2,casts_3,casts_4,casts_5,casts_6,casts_7)
casts_merge_temp <- casts_merge %>% select(Date,Temp_C,grouping) %>% arrange(Date)
casts_layers_temp <- casts_merge_temp %>% spread(grouping,Temp_C)

# Remove rows with Na values
casts_layers_temp <- na.omit(casts_layers_temp)

# Plot averaged temperature for 'casts_7' (aka: 8.7 - 9.6 m depths)
ggplot(casts_layers_temp,aes(x=Date,y=FCR_7))+
  geom_line(aes(x=Date,y=FCR_7))+
  geom_point()+
  theme_classic()

# Separate by year and remove funky values (2017-07-06; 2017-10-08)
swi_16 <- casts_layers_temp %>% filter(Date>=as.Date('2016-01-01')&Date<=as.Date('2016-12-31'))
swi_16$doy <- yday(swi_16$Date)
swi_17 <- casts_layers_temp %>% filter(Date>=as.Date('2017-01-01')&Date<=as.Date('2017-12-31'))
swi_17$doy <- yday(swi_17$Date)
swi_17 <- swi_17[-c(30,57),]
swi_18 <- casts_layers_temp %>% filter(Date>=as.Date('2018-01-01')&Date<=as.Date('2018-12-31'))
swi_18$doy <- yday(swi_18$Date)

# Export out to compare to BVR
write_csv(swi_16, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_16_fcr.csv")
write_csv(swi_17, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_17_fcr.csv")
write_csv(swi_18, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_18_fcr.csv")

# Plot all years on same graph
ggplot()+
  geom_line(data=swi_16,aes(x=doy,y=FCR_7,color="2016"),size=1.1)+
  geom_point(data=swi_16,aes(x=doy,y=FCR_7,color="2016"),size=4)+
  geom_line(data=swi_17,aes(x=doy,y=FCR_7,color="2017"),size=1.1)+
  geom_point(data=swi_17,aes(x=doy,y=FCR_7,color="2017"),size=4)+
  geom_line(data=swi_18,aes(x=doy,y=FCR_7,color="2018"),size=1.1)+
  geom_point(data=swi_18,aes(x=doy,y=FCR_7,color="2018"),size=4)+
  scale_color_manual(breaks=c("2016","2017","2018"), labels=c("FCR 2016","FCR 2017","FCR 2018"),
                     values=c('#91bfff','#2c2cff','#0F2080'))+
  geom_vline(xintercept = 280,linetype="dashed",color="#91bfff",size=1)+ #Turnover FCR 2016
  geom_vline(xintercept = 298,linetype="dashed",color="#2c2cff",size=1)+ #Turnover FCR 2017
  geom_vline(xintercept = 294,linetype="dashed",color="#0F2080",size=1)+ #Turnover FCR 2018
  xlab("Day of Year")+
  labs(color="")+
  ylab("Temperature (deg C)")+
  theme_classic(base_size=15)

# Use for loop to calculate volume weighted averages for each time point
vw_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_temp[i] <- sum(casts_layers_temp$FCR_1[i]*depth_2[1],
                    casts_layers_temp$FCR_2[i]*depth_2[2],
                    casts_layers_temp$FCR_3[i]*depth_2[3],
                    casts_layers_temp$FCR_4[i]*depth_2[4],
                    casts_layers_temp$FCR_5[i]*depth_2[5],
                    casts_layers_temp$FCR_6[i]*depth_2[6],
                    casts_layers_temp$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_temp <- cbind(casts_layers_temp,vw_temp)
names(vw_temp)[1] <- "DateTime"

ggplot(vw_temp,aes(DateTime,vw_temp))+geom_line()+theme_classic()

### Calculate Epi VW Temp: defined as 0-2.6 m
vw_epi_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_epi_temp[i] <- sum(casts_layers_temp$FCR_1[i]*depth_2[1],
                        casts_layers_temp$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

### Calculate Meta VW Temp: defined as 2.6-6.5m
vw_meta_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_meta_temp[i] <- sum(casts_layers_temp$FCR_3[i]*depth_2[3],
                         casts_layers_temp$FCR_4[i]*depth_2[4],
                         casts_layers_temp$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

### Calculate Hypo VW Temp: defined as 6.5-9m
vw_hypo_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_hypo_temp[i] <- sum(casts_layers_temp$FCR_6[i]*depth_2[6],
                         casts_layers_temp$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_tempf <- cbind.data.frame(vw_temp$DateTime,vw_temp$vw_temp,vw_epi_temp,vw_meta_temp,vw_hypo_temp)
names(vw_tempf)[1] <- "DateTime"
names(vw_tempf)[2] <- "WaterCol"
names(vw_tempf)[3] <- "Epi"
names(vw_tempf)[4] <- "Meta"
names(vw_tempf)[5] <- "Hypo"
vw_tempf_gather <- vw_tempf %>% gather(key=depth,value=vw_temp,-DateTime)
vw_tempf_gather$depth<-factor(vw_tempf_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))
vw_tempf_gather16 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_tempf_gather17 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_tempf_gather18 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

# Plot to check: width = 1000; height = 400
temp16 <- ggplot(vw_tempf_gather16,aes(x = DateTime, y = vw_temp, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
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
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
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
  geom_vline(xintercept=as.POSIXct("2018-04-23"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2018-07-30"),linetype="longdash")+ #Oxygen off
  geom_vline(xintercept=as.POSIXct("2018-10-21"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression('Temperature ('*degree*C*')'))+
  ylim(0,30)+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(temp16,temp17,temp18,common.legend=TRUE,legend="right",ncol=3,nrow=1)
#### NOTE: NEED TO DO SOME QA/QC OF TEMP DATA IN 2017 ####

# Export out Temp Data VW averaged by depth
write_csv(vw_tempf, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_Temp_16to18.csv")

### Calculate the same for oxygen
# Re-combine all casts then use spread to convert to wide form
casts_merge_o2 <- casts_merge %>% select(Date,DO_mgL,grouping) %>% arrange(Date)
casts_layers_o2 <- casts_merge_o2 %>% spread(grouping,DO_mgL)

# Remove rows with Na values
casts_layers_o2 <- na.omit(casts_layers_o2)

# Use for loop to calculate volume weighted averages for each time point
vw_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_o2[i] <- sum(casts_layers_o2$FCR_1[i]*depth_2[1],
                    casts_layers_o2$FCR_2[i]*depth_2[2],
                    casts_layers_o2$FCR_3[i]*depth_2[3],
                    casts_layers_o2$FCR_4[i]*depth_2[4],
                    casts_layers_o2$FCR_5[i]*depth_2[5],
                    casts_layers_o2$FCR_6[i]*depth_2[6],
                    casts_layers_o2$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_o2 <- cbind(casts_layers_o2,vw_o2)
names(vw_o2)[1] <- "DateTime"

ggplot(vw_o2,aes(DateTime,vw_o2))+geom_line()+theme_classic()

### Calculate Epi VW Temp: defined as 0-2.6 m
vw_epi_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_epi_o2[i] <- sum(casts_layers_o2$FCR_1[i]*depth_2[1],
                        casts_layers_o2$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

### Calculate Meta VW Temp: defined as 2.6-6.5m
vw_meta_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_meta_o2[i] <- sum(casts_layers_o2$FCR_3[i]*depth_2[3],
                         casts_layers_o2$FCR_4[i]*depth_2[4],
                         casts_layers_o2$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

### Calculate Hypo VW Temp: defined as 6.5-9m
vw_hypo_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_hypo_o2[i] <- sum(casts_layers_o2$FCR_6[i]*depth_2[6],
                         casts_layers_o2$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_o2f <- cbind.data.frame(vw_o2$DateTime,vw_o2$vw_o2,vw_epi_o2,vw_meta_o2,vw_hypo_o2)
names(vw_o2f)[1] <- "DateTime"
names(vw_o2f)[2] <- "WaterCol"
names(vw_o2f)[3] <- "Epi"
names(vw_o2f)[4] <- "Meta"
names(vw_o2f)[5] <- "Hypo"
vw_o2f_gather <- vw_o2f %>% gather(key=depth,value=vw_o2,-DateTime)
vw_o2f_gather$depth<-factor(vw_o2f_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))
vw_o2f_gather16 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_o2f_gather17 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_o2f_gather18 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

# Plot to check
o16 <- ggplot(vw_o2f_gather16,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
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
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
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
  geom_vline(xintercept=as.POSIXct("2018-04-23"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2018-07-30"),linetype="longdash")+ #Oxygen off
  geom_vline(xintercept=as.POSIXct("2018-10-21"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression("DO (mg L"^-1*")"))+
  ylim(0,17)+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(o16,o17,o18,common.legend=TRUE,legend="right",ncol=3,nrow=1)

## May also want to verify date in 2017 when DO drops to zero for the Hypo and Meta - is this real?

# Export out Temp Data VW averaged by depth
write_csv(vw_o2f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_o2_16to18.csv")

### Calculate VW GHG's: calculate for reps individually
### Then calculate avg and stdev AFTER all calculations
ghg_1_rep1 <- ghg %>% filter(depth==0.1,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_1")
ghg_1_rep1$ch4_umol_L <- na.approx(ghg_1_rep1$ch4_umol_L)
ghg_1_rep1$co2_umol_L <- na.approx(ghg_1_rep1$co2_umol_L)

ghg_1_rep2 <- ghg %>% filter(depth==0.1,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_1")
ghg_1_rep2$ch4_umol_L <- na.approx(ghg_1_rep2$ch4_umol_L)
ghg_1_rep2$co2_umol_L <- na.approx(ghg_1_rep2$co2_umol_L)

ghg_2_rep1 <- ghg %>% filter(depth==1.6,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_2")
ghg_2_rep1$ch4_umol_L <- na.approx(ghg_2_rep1$ch4_umol_L)
ghg_2_rep1$co2_umol_L <- na.approx(ghg_2_rep1$co2_umol_L)

ghg_2_rep2 <- ghg %>% filter(depth==1.6,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_2")
ghg_2_rep2 <- ghg_2_rep2[-104,] # NA value for the last date - cannot interpolate
ghg_2_rep2$ch4_umol_L <- na.approx(ghg_2_rep2$ch4_umol_L)
ghg_2_rep2$co2_umol_L <- na.approx(ghg_2_rep2$co2_umol_L)

ghg_3_rep1 <- ghg %>% filter(depth==3.8,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_3")
ghg_3_rep1$ch4_umol_L <- na.approx(ghg_3_rep1$ch4_umol_L)
ghg_3_rep1$co2_umol_L <- na.approx(ghg_3_rep1$co2_umol_L)

ghg_3_rep2 <- ghg %>% filter(depth==3.8,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_3")
ghg_3_rep2 <- ghg_3_rep2[-104,] # NA value for the last date; cannot interpolate
ghg_3_rep2$ch4_umol_L <- na.approx(ghg_3_rep2$ch4_umol_L)
ghg_3_rep2$co2_umol_L <- na.approx(ghg_3_rep2$co2_umol_L)

ghg_4_rep1 <- ghg %>% filter(depth==5.0,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_4")
ghg_4_rep1$ch4_umol_L <- na.approx(ghg_4_rep1$ch4_umol_L)
ghg_4_rep1$co2_umol_L <- na.approx(ghg_4_rep1$co2_umol_L)

ghg_4_rep2 <- ghg %>% filter(depth==5.0,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_4")
ghg_4_rep2 <- ghg_4_rep2[-c(104,105),] # NAs for last two dates
ghg_4_rep2$ch4_umol_L <- na.approx(ghg_4_rep2$ch4_umol_L)
ghg_4_rep2$co2_umol_L <- na.approx(ghg_4_rep2$co2_umol_L)

ghg_5_rep1 <- ghg %>% filter(depth==6.2,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_5")
ghg_5_rep1$ch4_umol_L <- na.approx(ghg_5_rep1$ch4_umol_L)
ghg_5_rep1$co2_umol_L <- na.approx(ghg_5_rep1$co2_umol_L)

ghg_5_rep2 <- ghg %>% filter(depth==6.2,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_5")
ghg_5_rep2$ch4_umol_L <- na.approx(ghg_5_rep2$ch4_umol_L)
ghg_5_rep2$co2_umol_L <- na.approx(ghg_5_rep2$co2_umol_L)

ghg_6_rep1 <- ghg %>% filter(depth==8.0,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_6")
ghg_6_rep1$ch4_umol_L <- na.approx(ghg_6_rep1$ch4_umol_L)
ghg_6_rep1$co2_umol_L <- na.approx(ghg_6_rep1$co2_umol_L)

ghg_6_rep2 <- ghg %>% filter(depth==8.0,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_6")
ghg_6_rep2$ch4_umol_L <- na.approx(ghg_6_rep2$ch4_umol_L)
ghg_6_rep2$co2_umol_L <- na.approx(ghg_6_rep2$co2_umol_L)

ghg_7_rep1 <- ghg %>% filter(depth==9.0,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_7")
ghg_7_rep1$ch4_umol_L <- na.approx(ghg_7_rep1$ch4_umol_L)
ghg_7_rep1$co2_umol_L <- na.approx(ghg_7_rep1$co2_umol_L)

ghg_7_rep2 <- ghg %>% filter(depth==9.0,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_7")
ghg_7_rep2$ch4_umol_L <- na.approx(ghg_7_rep2$ch4_umol_L)
ghg_7_rep2$co2_umol_L <- na.approx(ghg_7_rep2$co2_umol_L)

# Recombine into reps1 and separate by CO2 and CH4
ghg_merge_rep1 <- rbind(ghg_1_rep1,ghg_2_rep1,ghg_3_rep1,ghg_4_rep1,ghg_5_rep1,
                        ghg_6_rep1,ghg_7_rep1)
ghg_merge_rep2 <- rbind(ghg_1_rep2,ghg_2_rep2,ghg_3_rep2,ghg_4_rep2,ghg_5_rep2,
                        ghg_6_rep2,ghg_7_rep2)
rep1_co2 <- ghg_merge_rep1 %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
rep1_co2_layers <- rep1_co2 %>% spread(grouping,co2_umol_L)

rep2_co2 <- ghg_merge_rep2 %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
rep2_co2_layers <- rep2_co2 %>% spread(grouping,co2_umol_L)

rep1_ch4 <- ghg_merge_rep1 %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
rep1_ch4_layers <- rep1_ch4 %>% spread(grouping,ch4_umol_L)

rep2_ch4 <- ghg_merge_rep2 %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
rep2_ch4_layers <- rep2_ch4 %>% spread(grouping,ch4_umol_L)

# Calculate VW pCH4 for the entire water column: reps 1 and 2 separately
vw_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  vw_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_1[i]*depth_2[1],
                        rep1_ch4_layers$FCR_2[i]*depth_2[2],
                        rep1_ch4_layers$FCR_3[i]*depth_2[3],
                        rep1_ch4_layers$FCR_4[i]*depth_2[4],
                        rep1_ch4_layers$FCR_5[i]*depth_2[5],
                        rep1_ch4_layers$FCR_6[i]*depth_2[6],
                        rep1_ch4_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_ch4_rep1 <- cbind(rep1_ch4_layers,vw_ch4_rep1)

# Plot to check
ggplot(vw_ch4_rep1,aes(datetime,vw_ch4_rep1))+geom_line()+theme_classic()

# Calculate VW pCH4 for Rep 2
vw_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  vw_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_1[i]*depth_2[1],
                        rep2_ch4_layers$FCR_2[i]*depth_2[2],
                        rep2_ch4_layers$FCR_3[i]*depth_2[3],
                        rep2_ch4_layers$FCR_4[i]*depth_2[4],
                        rep2_ch4_layers$FCR_5[i]*depth_2[5],
                        rep2_ch4_layers$FCR_6[i]*depth_2[6],
                        rep2_ch4_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
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

### Calculate Epi VW pCH4: defined as 0-2.6 m
### Calculate for Reps 1 and 2 separately then average/stdev
epi_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  epi_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_1[i]*depth_2[1],
                        rep1_ch4_layers$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

# Epi, rep 2, CH4
epi_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  epi_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_1[i]*depth_2[1],
                         rep2_ch4_layers$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
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

### Calculate Meta VW Temp: defined as 2.6-6.5m
### Calculate for Rep 1 and 2 then avg/stdev
meta_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  meta_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_3[i]*depth_2[3],
                         rep1_ch4_layers$FCR_4[i]*depth_2[4],
                         rep1_ch4_layers$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

# Meta, rep 2, CH4
meta_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  meta_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_3[i]*depth_2[3],
                          rep2_ch4_layers$FCR_4[i]*depth_2[4],
                          rep2_ch4_layers$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
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

### Calculate Hypo VW Temp: defined as 6.5-9m
### Calculate Rep 1 and Rep 2; then combine
hypo_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$datetime))
for(i in 1:length(rep1_ch4_layers$datetime)){
  hypo_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_6[i]*depth_2[6],
                          rep1_ch4_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

# Hypo, rep 2, CH4
hypo_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$datetime))
for(i in 1:length(rep2_ch4_layers$datetime)){
  hypo_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_6[i]*depth_2[6],
                          rep2_ch4_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
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

## Separate into years (2016; 2017; 2018): three panel graph width = 1200; height = 300
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
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
  labs(color="")+
  xlab('2016')+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size = 15)

ch417 <- ggplot(vw_ch4_17,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  theme_classic(base_size=15)

ch418 <- ggplot(vw_ch4_18,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-04-23"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2018-07-30"),linetype="longdash")+ #Oxygen off
  geom_vline(xintercept=as.POSIXct("2018-10-21"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(ch416,ch417,ch418,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_ch4_all, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_ch4_stats_16to18.csv")

############################CO2##########################################################
## Calculate VW pCO2 for FCR
# Calculate VW pCH4 for the entire water column
# Remove outlier: Rep 1 ~627 umol/L while Rep 2 ~29 umol/L
rep1_co2_layers$FCR_2[68] <- NA
rep1_co2_layers$FCR_6[104] <- NA

vw_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  vw_co2_rep1[i] <- sum(rep1_co2_layers$FCR_1[i]*depth_2[1],
                        rep1_co2_layers$FCR_2[i]*depth_2[2],
                        rep1_co2_layers$FCR_3[i]*depth_2[3],
                        rep1_co2_layers$FCR_4[i]*depth_2[4],
                        rep1_co2_layers$FCR_5[i]*depth_2[5],
                        rep1_co2_layers$FCR_6[i]*depth_2[6],
                        rep1_co2_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_co2_rep1 <- cbind(rep1_co2_layers,vw_co2_rep1)

# Plot to check
ggplot(vw_co2_rep1,aes(datetime,vw_co2_rep1))+geom_line()+theme_classic()

# Calculate VW pCO2 for Rep 2
vw_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  vw_co2_rep2[i] <- sum(rep2_co2_layers$FCR_1[i]*depth_2[1],
                        rep2_co2_layers$FCR_2[i]*depth_2[2],
                        rep2_co2_layers$FCR_3[i]*depth_2[3],
                        rep2_co2_layers$FCR_4[i]*depth_2[4],
                        rep2_co2_layers$FCR_5[i]*depth_2[5],
                        rep2_co2_layers$FCR_6[i]*depth_2[6],
                        rep2_co2_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_co2_rep2 <- cbind(rep2_co2_layers,vw_co2_rep2)

# Plot to check
ggplot(vw_co2_rep2,aes(datetime,vw_co2_rep2))+geom_line()+theme_classic()

# Calculate Avg and Stdev for Water Col VW
vw_co2 <- cbind(vw_co2_rep1$vw_co2_rep1,vw_co2_rep2$vw_co2_rep2)

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

### Calculate Epi VW pCO2: defined as 0-2.6 m
# Calculate as Rep 1 and Rep 2 then combine
epi_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  epi_co2_rep1[i] <- sum(rep1_co2_layers$FCR_1[i]*depth_2[1],
                         rep1_co2_layers$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

# Epi Rep 2 CO2
epi_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  epi_co2_rep2[i] <- sum(rep2_co2_layers$FCR_1[i]*depth_2[1],
                         rep2_co2_layers$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
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

### Calculate Meta VW pCO2: defined as 2.6-6.5m
### Calculate Rep 1 and 2 seperately
meta_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  meta_co2_rep1[i] <- sum(rep1_co2_layers$FCR_3[i]*depth_2[3],
                          rep1_co2_layers$FCR_4[i]*depth_2[4],
                          rep1_co2_layers$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

# Meta, rep 2, CH4
meta_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  meta_co2_rep2[i] <- sum(rep2_co2_layers$FCR_3[i]*depth_2[3],
                          rep2_co2_layers$FCR_4[i]*depth_2[4],
                          rep2_co2_layers$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

meta_co2 <- cbind(meta_co2_rep1,meta_co2_rep2)
meta_co2_avg <- rowMeans(meta_co2,na.rm=TRUE)
meta_co2_stdev <- apply(meta_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_all,meta_co2_avg,meta_co2_stdev)
names(vw_co2_all)[6] <- "meta_avg"
names(vw_co2_all)[7] <- "meta_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,meta_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=meta_avg-meta_std, ymax=meta_avg+meta_std))+
  theme_classic()

### Calculate Hypo VW Temp: defined as 6.5-9m
### Calculate Rep 1 and Rep 2; then combine
hypo_co2_rep1 <- rep(-99,length(rep1_co2_layers$datetime))
for(i in 1:length(rep1_co2_layers$datetime)){
  hypo_co2_rep1[i] <- sum(rep1_co2_layers$FCR_6[i]*depth_2[6],
                          rep1_co2_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

# Hypo, rep 2, CH4
hypo_co2_rep2 <- rep(-99,length(rep2_co2_layers$datetime))
for(i in 1:length(rep2_co2_layers$datetime)){
  hypo_co2_rep2[i] <- sum(rep2_co2_layers$FCR_6[i]*depth_2[6],
                          rep2_co2_layers$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
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

## Separate into years (2016; 2017): width = 1000; height = 400
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

#### Need to check this ####

## Calculate Hypo 'averaged' concentration over each summer
# First select the correct date range for each summer
# April 1 - Turnover
vw_co2_avg_16 <- vw_co2_avg %>% filter(datetime>=as.Date('2016-04-01')&
                                         datetime<=as.Date('2016-10-14'))
vw_co2_avg_17 <- vw_co2_avg %>% filter(datetime>=as.Date('2017-04-01')&
                                         datetime<=as.Date('2017-12-12'))

vw_ch4_avg_16 <- vw_ch4_avg %>% filter(datetime>=as.Date('2016-04-01')&datetime<=as.Date('2016-10-14'))
vw_ch4_avg_17 <- vw_ch4_avg %>% filter(datetime>=as.Date('2017-04-01')&datetime<=as.Date('2017-12-12'))

vw_hypo_co2_16 <- sum(vw_co2_avg_16$Hypo)/length(vw_co2_avg_16$Hypo)
vw_hypo_co2_17 <- sum(vw_co2_avg_17$Hypo)/length(vw_co2_avg_17$Hypo)
vw_hypo_ch4_16 <- sum(vw_ch4_avg_16$Hypo)/length(vw_ch4_avg_16$Hypo)
vw_hypo_ch4_17 <- sum(vw_ch4_avg_17$Hypo)/length(vw_ch4_avg_17$Hypo)

### Go back to plotting

vw_co2_std_long <- vw_co2_std %>% pivot_longer(-datetime,names_to="depth",values_to="co2_std")

vw_co2_long <- merge(vw_co2_avg_long,vw_co2_std_long,by=c("datetime","depth"))

vw_co2_16 <- vw_co2_long %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_co2_17 <- vw_co2_long %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_co2_18 <- vw_co2_long %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

# Remove rows with NA averages (but not SD!)
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

vw_co2_16 <- completeFun(vw_co2_16,"co2_avg")
vw_co2_17 <- completeFun(vw_co2_17,"co2_avg")
vw_co2_18 <- completeFun(vw_co2_18,"co2_avg")

co216 <- ggplot(vw_co2_16,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
  labs(color="")+
  xlab('2016')+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size = 15)

co217 <- ggplot(vw_co2_17,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size=15)

co218 <- ggplot(vw_co2_18,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-04-23"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2018-07-30"),linetype="longdash")+ #Oxygen off
  geom_vline(xintercept=as.POSIXct("2018-10-21"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(co216,co217,co218,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_co2_all, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_co2_stats_16to18.csv")
