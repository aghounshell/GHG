### Script to calculate CO2 and CH4 flux from FCR and BVR from Summer 2015 to Summer 2017
### A Hounshell, 10 Jun 2019
### Updated to include separate reps for 0.1 m; constrained to Summer 2016 and 2017
### 21 Oct 2019

# Saved RFile as GHG_Flux

# Load libraries
library(LakeMetabolizer)
library(zoo)
library(tidyverse)
library(scales)
library(ggpubr)

# Set working directory: using only output data files
# Wind data: calculated for FCR when met data is not available
# GHG data for BVR and FCR separated
setwd("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output")

# Read in .csv for FCR data, BVR data, and wind data
fcr <- read.csv("GHG_FCR_0.1_reps.csv",header=TRUE)
fcr$datetime <- as.POSIXct(strptime(fcr$datetime, "%Y-%m-%d", tz="EST"))
bvr <- read.csv("GHG_BVR_0.1_reps.csv",header=TRUE)
bvr$datetime <- as.POSIXct(strptime(bvr$datetime, "%Y-%m-%d", tz="EST"))
wind <- read.csv("Calc_FCR_Wind.csv",header=TRUE)
wind$Date <- as.POSIXct(strptime(wind$Date, "%Y-%m-%d", tz="EST"))

# Separate FCR and BVR GHG Data into Reps 1 and 2: calculate fluxes for each rep
# then calculate avg and stdev from both reps
fcr_1 <- fcr %>% filter(rep==1) %>% group_by(datetime) %>% summarize_all(funs(mean)) %>% 
  arrange(datetime) %>% mutate(grouping="Rep1")
fcr_2 <- fcr %>% filter(rep==2) %>% group_by(datetime) %>% summarize_all(funs(mean)) %>% 
  arrange(datetime) %>% mutate(grouping="Rep2")
bvr_1 <- bvr %>% filter(rep==1) %>% group_by(datetime) %>% summarize_all(funs(mean)) %>% 
  arrange(datetime) %>% mutate(grouping="Rep1")
bvr_2 <- bvr %>% filter(rep==2) %>% group_by(datetime) %>% summarize_all(funs(mean)) %>% 
  arrange(datetime) %>% mutate(grouping="Rep2")

# Calculate 'k' for GHG calculations using the Cole method
k600 <- k.cole.base(wind$met_wind) # units in m/day
k600data <- cbind(wind, k600)
names(k600data)[1] <- "datetime"

# Merge k600 data and GHG data for FCR
# Then approximate GHG data in between sample dates (use linear interpolation)
fcr_ghg_1 <- merge(k600data, fcr_1, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg_1$co2_umol_L <- na.approx(fcr_ghg_1$co2_umol_L,na.rm=FALSE)
fcr_ghg_1$ch4_umol_L <- na.approx(fcr_ghg_1$ch4_umol_L,na.rm=FALSE)
fcr_ghg_1_16 <- fcr_ghg_1 %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
fcr_ghg_1_17 <- fcr_ghg_1 %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

fcr_ghg_2 <- merge(k600data, fcr_2, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg_2$co2_umol_L <- na.approx(fcr_ghg_2$co2_umol_L,na.rm=FALSE)
fcr_ghg_2$ch4_umol_L <- na.approx(fcr_ghg_2$ch4_umol_L,na.rm=FALSE)
fcr_ghg_2_16 <- fcr_ghg_2 %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
fcr_ghg_2_17 <- fcr_ghg_2 %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

# Load in GHG atmospheric data: calculated from Flux Tower data at Savannah River Labs
# See code: Atm_ch4 and Atm_co2
atm_ch4_16 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_ch4_2016.csv')
atm_ch4_16$datetime <- as.POSIXct(strptime(atm_ch4_16$datetime, "%Y-%m-%d", tz="EST"))
atm_ch4_17 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_ch4_2017.csv')
atm_ch4_17$datetime <- as.POSIXct(strptime(atm_ch4_17$datetime, "%Y-%m-%d", tz="EST"))
atm_co2_16 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_co2_2016.csv')
atm_co2_16$datetime <- as.POSIXct(strptime(atm_co2_16$datetime, "%Y-%m-%d", tz="EST"))
atm_co2_17 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_co2_2017.csv')
atm_co2_17$datetime <- as.POSIXct(strptime(atm_co2_17$datetime, "%Y-%m-%d", tz="EST"))

# Calculate GHG flux
ch4flux_1_16<-rep(-99,length(fcr_ghg_1_16$datetime))
ch4flux_2_16<-rep(-99,length(fcr_ghg_2_16$datetime)) # assuming Catm = 1.8 ppm or 0.1125 umol

ch4flux_1_17<-rep(-99,length(fcr_ghg_1_17$datetime))
ch4flux_2_17<-rep(-99,length(fcr_ghg_2_17$datetime))

co2flux_1_16<-rep(-99,length(fcr_ghg_1_16$datetime))
co2flux_2_16<-rep(-99,length(fcr_ghg_2_16$datetime)) 

co2flux_1_17<-rep(-99,length(fcr_ghg_1_17$datetime))
co2flux_2_17<-rep(-99,length(fcr_ghg_2_17$datetime))

# Separate k600 data by year
k600_16 <- k600data %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
k600_16$datetime <- as.POSIXct(strptime(k600_16$datetime, "%Y-%m-%d", tz="EST"))
k600_17 <- k600data %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
k600_17$datetime <- as.POSIXct(strptime(k600_17$datetime, "%Y-%m-%d", tz="EST"))

# Rates in umol/m2/d
for(i in 1:length(fcr_ghg_1_16$datetime)){
  ch4flux_1_16[i] <- 1000*k600_16$k600[i]*((fcr_ghg_1_16$ch4_umol_L[i])-atm_ch4_16$ch4_umol_l[i])
  ch4flux_2_16[i] <- 1000*k600_16$k600[i]*((fcr_ghg_2_16$ch4_umol_L[i])-atm_ch4_16$ch4_umol_l[i])
  co2flux_1_16[i] <- 1000*k600_16$k600[i]*((fcr_ghg_1_16$co2_umol_L[i])-atm_co2_16$co2_umol_l[i])
  co2flux_2_16[i] <- 1000*k600_16$k600[i]*((fcr_ghg_2_16$co2_umol_L[i])-atm_co2_16$co2_umol_l[i])
}

# Calculate for 2017
for(i in 1:length(fcr_ghg_1_17$datetime)){
  ch4flux_1_17[i] <- 1000*k600_17$k600[i]*((fcr_ghg_1_17$ch4_umol_L[i])-atm_ch4_17$ch4_umol_l[i])
  ch4flux_2_17[i] <- 1000*k600_17$k600[i]*((fcr_ghg_2_17$ch4_umol_L[i])-atm_ch4_17$ch4_umol_l[i])
  co2flux_1_17[i] <- 1000*k600_17$k600[i]*((fcr_ghg_1_17$co2_umol_L[i])-atm_co2_17$co2_umol_l[i])
  co2flux_2_17[i] <- 1000*k600_17$k600[i]*((fcr_ghg_2_17$co2_umol_L[i])-atm_co2_17$co2_umol_l[i])
}

# Also calculate fluxes for days where there are discrete measurements
fcr_1_16 <- fcr_1 %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
fcr_2_16 <- fcr_2 %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
fcr_1_17 <- fcr_1 %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
fcr_2_17 <- fcr_2 %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

fcr_ghg_dis_16_1 <- merge(k600_16, fcr_1_16, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg_dis_16_1 <- merge(fcr_ghg_dis_16_1,atm_ch4_16,by="datetime",all.x=TRUE,all.y=TRUE)
fcr_ghg_dis_16_1 <- merge(fcr_ghg_dis_16_1,atm_co2_16,by="datetime",all.x=TRUE,all.y=TRUE)

fcr_ghg_dis_16_2 <- merge(k600_16, fcr_2_16, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg_dis_16_2 <- merge(fcr_ghg_dis_16_2,atm_ch4_16,by="datetime",all.x=TRUE,all.y=TRUE)
fcr_ghg_dis_16_2 <- merge(fcr_ghg_dis_16_2,atm_co2_16,by="datetime",all.x=TRUE,all.y=TRUE)

fcr_ghg_dis_17_1 <- merge(k600_17, fcr_1_17, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg_dis_17_1 <- merge(fcr_ghg_dis_17_1,atm_ch4_17,by="datetime",all.x=TRUE,all.y=TRUE)
fcr_ghg_dis_17_1 <- merge(fcr_ghg_dis_17_1,atm_co2_17,by="datetime",all.x=TRUE,all.y=TRUE)

fcr_ghg_dis_17_2 <- merge(k600_17, fcr_2_17, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg_dis_17_2 <- merge(fcr_ghg_dis_17_2,atm_ch4_17,by="datetime",all.x=TRUE,all.y=TRUE)
fcr_ghg_dis_17_2 <- merge(fcr_ghg_dis_17_2,atm_co2_17,by="datetime",all.x=TRUE,all.y=TRUE)

# Calculate GHG flux
ch4flux_dis_16_1<-rep(-99,length(fcr_ghg_dis_16_1$datetime))
ch4flux_dis_16_2<-rep(-99,length(fcr_ghg_dis_16_2$datetime))

co2flux_dis_16_1<-rep(-99,length(fcr_ghg_dis_16_1$datetime))
co2flux_dis_16_2<-rep(-99,length(fcr_ghg_dis_16_2$datetime))

ch4flux_dis_17_1<-rep(-99,length(fcr_ghg_dis_17_1$datetime))
ch4flux_dis_17_2<-rep(-99,length(fcr_ghg_dis_17_2$datetime))

co2flux_dis_17_1<-rep(-99,length(fcr_ghg_dis_17_1$datetime))
co2flux_dis_17_2<-rep(-99,length(fcr_ghg_dis_17_2$datetime))

# Rates in umol/m2/d
for(i in 1:length(fcr_ghg_dis_16_1$datetime)){
  ch4flux_dis_16_1[i] <- 1000*fcr_ghg_dis_16_1$k600[i]*((fcr_ghg_dis_16_1$ch4_umol_L[i])-
                                                          fcr_ghg_dis_16_1$ch4_umol_l[i])
  ch4flux_dis_16_2[i] <- 1000*fcr_ghg_dis_16_2$k600[i]*((fcr_ghg_dis_16_2$ch4_umol_L[i])-
                                                          fcr_ghg_dis_16_2$ch4_umol_l[i])
  co2flux_dis_16_1[i] <- 1000*fcr_ghg_dis_16_1$k600[i]*((fcr_ghg_dis_16_1$co2_umol_L[i])-
                                                          fcr_ghg_dis_16_1$co2_umol_l[i])
  co2flux_dis_16_2[i] <- 1000*fcr_ghg_dis_16_2$k600[i]*((fcr_ghg_dis_16_2$co2_umol_L[i])-
                                                          fcr_ghg_dis_16_2$co2_umol_l[i])
}

# Calculate discrete fluxes for 2017 data
for(i in 1:length(fcr_ghg_dis_17_1$datetime)){
  ch4flux_dis_17_1[i] <- 1000*fcr_ghg_dis_17_1$k600[i]*((fcr_ghg_dis_17_1$ch4_umol_L[i])-
                                                          fcr_ghg_dis_17_1$ch4_umol_l[i])
  ch4flux_dis_17_2[i] <- 1000*fcr_ghg_dis_17_2$k600[i]*((fcr_ghg_dis_17_2$ch4_umol_L[i])-
                                                          fcr_ghg_dis_17_2$ch4_umol_l[i])
  co2flux_dis_17_1[i] <- 1000*fcr_ghg_dis_17_1$k600[i]*((fcr_ghg_dis_17_1$co2_umol_L[i])-
                                                          fcr_ghg_dis_17_1$co2_umol_l[i])
  co2flux_dis_17_2[i] <- 1000*fcr_ghg_dis_17_2$k600[i]*((fcr_ghg_dis_17_2$co2_umol_L[i])-
                                                          fcr_ghg_dis_17_2$co2_umol_l[i])
}

# Calculate avg and std for each time point and combine data frames
fcr_flux_16 <- as.data.frame(fcr_ghg_dis_16_1$datetime)
names(fcr_flux_16)[1] <- "datetime"

ch4flux_16 <- cbind(ch4flux_1_16,ch4flux_2_16)
fcr_flux_16$ch4_avg <- rowMeans(ch4flux_16)
fcr_flux_16$ch4_std <- apply(ch4flux_16,1,FUN=sd)

co2flux_16 <- cbind(co2flux_1_16,co2flux_2_16)
fcr_flux_16$co2_avg <- rowMeans(co2flux_16)
fcr_flux_16$co2_std <- apply(co2flux_16,1,FUN=sd)

ch4flux_dis_16 <- cbind(ch4flux_dis_16_1,ch4flux_dis_16_2)
fcr_flux_16$ch4_dis_avg <- rowMeans(ch4flux_dis_16)
fcr_flux_16$ch4_dis_std <- apply(ch4flux_dis_16,1,FUN=sd)

co2flux_dis_16 <- cbind(co2flux_dis_16_1,co2flux_dis_16_2)
fcr_flux_16$co2_dis_avg <- rowMeans(co2flux_dis_16)
fcr_flux_16$co2_dis_std <- apply(co2flux_dis_16,1,FUN=sd)

fcr_flux_16$doy <- as.numeric(strftime(fcr_flux_16$datetime, format = "%j"))

## Try plotting FCR fluxes?
ggplot(fcr_flux_16)+
  geom_line(mapping=aes(x=datetime,y=ch4_avg/1000,color='CH4'))+
  geom_line(mapping=aes(x=datetime,y=co2_avg/1000,color='CO2'))+
  geom_point(mapping=aes(x=datetime,y=ch4_dis_avg/1000,color='CH4'))+
  geom_point(mapping=aes(x=datetime,y=co2_dis_avg/1000,color='CO2'))+
  theme_classic(base_size=17)

# Calculate for 2017
fcr_flux_17 <- as.data.frame(fcr_ghg_dis_17_1$datetime)
names(fcr_flux_17)[1] <- "datetime"

ch4flux_17 <- cbind(ch4flux_1_17,ch4flux_2_17)
fcr_flux_17$ch4_avg <- rowMeans(ch4flux_17)
fcr_flux_17$ch4_std <- apply(ch4flux_17,1,FUN=sd)

co2flux_17 <- cbind(co2flux_1_17,co2flux_2_17)
fcr_flux_17$co2_avg <- rowMeans(co2flux_17)
fcr_flux_17$co2_std <- apply(co2flux_17,1,FUN=sd)

ch4flux_dis_17 <- cbind(ch4flux_dis_17_1,ch4flux_dis_17_2)
fcr_flux_17$ch4_dis_avg <- rowMeans(ch4flux_dis_17)
fcr_flux_17$ch4_dis_std <- apply(ch4flux_dis_17,1,FUN=sd)

co2flux_dis_17 <- cbind(co2flux_dis_17_1,co2flux_dis_17_2)
fcr_flux_17$co2_dis_avg <- rowMeans(co2flux_dis_17)
fcr_flux_17$co2_dis_std <- apply(co2flux_dis_17,1,FUN=sd)

fcr_flux_17$doy <- as.numeric(strftime(fcr_flux_17$datetime, format = "%j"))

## Try plotting FCR fluxes for 2017
ggplot(fcr_flux_17)+
  geom_line(mapping=aes(x=datetime,y=ch4_avg/1000,color='CH4'))+
  geom_line(mapping=aes(x=datetime,y=co2_avg/1000,color='CO2'))+
  geom_point(mapping=aes(x=datetime,y=ch4_dis_avg/1000,color='CH4'))+
  geom_point(mapping=aes(x=datetime,y=co2_dis_avg/1000,color='CO2'))+
  theme_classic(base_size=17)

### Calculate fluxes for BVR

# Merge k600 data and GHG data for FCR
# Then approximate GHG data in between sample dates (use linear interpolation)
bvr_ghg_1 <- merge(k600data, bvr_1, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg_1$co2_umol_L <- na.approx(bvr_ghg_1$co2_umol_L,na.rm=FALSE)
bvr_ghg_1$ch4_umol_L <- na.approx(bvr_ghg_1$ch4_umol_L,na.rm=FALSE)
bvr_ghg_1_16 <- bvr_ghg_1 %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
bvr_ghg_1_17 <- bvr_ghg_1 %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

bvr_ghg_2 <- merge(k600data, bvr_2, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg_2$co2_umol_L <- na.approx(bvr_ghg_2$co2_umol_L,na.rm=FALSE)
bvr_ghg_2$ch4_umol_L <- na.approx(bvr_ghg_2$ch4_umol_L,na.rm=FALSE)
bvr_ghg_2_16 <- bvr_ghg_2 %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
bvr_ghg_2_17 <- bvr_ghg_2 %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

# Calculate GHG flux
bvr_ch4flux_1_16<-rep(-99,length(bvr_ghg_1_16$datetime))
bvr_ch4flux_2_16<-rep(-99,length(bvr_ghg_2_16$datetime))

bvr_ch4flux_1_17<-rep(-99,length(bvr_ghg_1_17$datetime))
bvr_ch4flux_2_17<-rep(-99,length(bvr_ghg_2_17$datetime))

bvr_co2flux_1_16<-rep(-99,length(bvr_ghg_1_16$datetime))
bvr_co2flux_2_16<-rep(-99,length(bvr_ghg_2_16$datetime)) 

bvr_co2flux_1_17<-rep(-99,length(bvr_ghg_1_17$datetime))
bvr_co2flux_2_17<-rep(-99,length(bvr_ghg_2_17$datetime))

# Rates in umol/m2/d
for(i in 1:length(bvr_ghg_1_16$datetime)){
  bvr_ch4flux_1_16[i] <- 1000*k600_16$k600[i]*((bvr_ghg_1_16$ch4_umol_L[i])-atm_ch4_16$ch4_umol_l[i])
  bvr_ch4flux_2_16[i] <- 1000*k600_16$k600[i]*((bvr_ghg_2_16$ch4_umol_L[i])-atm_ch4_16$ch4_umol_l[i])
  bvr_co2flux_1_16[i] <- 1000*k600_16$k600[i]*((bvr_ghg_1_16$co2_umol_L[i])-atm_co2_16$co2_umol_l[i])
  bvr_co2flux_2_16[i] <- 1000*k600_16$k600[i]*((bvr_ghg_2_16$co2_umol_L[i])-atm_co2_16$co2_umol_l[i])
}

# Calculate for 2017
for(i in 1:length(fcr_ghg_1_17$datetime)){
  bvr_ch4flux_1_17[i] <- 1000*k600_17$k600[i]*((bvr_ghg_1_17$ch4_umol_L[i])-atm_ch4_17$ch4_umol_l[i])
  bvr_ch4flux_2_17[i] <- 1000*k600_17$k600[i]*((bvr_ghg_2_17$ch4_umol_L[i])-atm_ch4_17$ch4_umol_l[i])
  bvr_co2flux_1_17[i] <- 1000*k600_17$k600[i]*((bvr_ghg_1_17$co2_umol_L[i])-atm_co2_17$co2_umol_l[i])
  bvr_co2flux_2_17[i] <- 1000*k600_17$k600[i]*((bvr_ghg_2_17$co2_umol_L[i])-atm_co2_17$co2_umol_l[i])
}

# Calculate fluxes for discrete measurements
bvr_1_16 <- bvr_1 %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
bvr_2_16 <- bvr_2 %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
bvr_1_17 <- bvr_1 %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
bvr_2_17 <- bvr_2 %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

bvr_ghg_dis_16_1 <- merge(k600_16, bvr_1_16, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg_dis_16_1 <- merge(bvr_ghg_dis_16_1,atm_ch4_16,by="datetime",all.x=TRUE,all.y=TRUE)
bvr_ghg_dis_16_1 <- merge(bvr_ghg_dis_16_1,atm_co2_16,by="datetime",all.x=TRUE,all.y=TRUE)

bvr_ghg_dis_16_2 <- merge(k600_16, bvr_2_16, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg_dis_16_2 <- merge(bvr_ghg_dis_16_2,atm_ch4_16,by="datetime",all.x=TRUE,all.y=TRUE)
bvr_ghg_dis_16_2 <- merge(bvr_ghg_dis_16_2,atm_co2_16,by="datetime",all.x=TRUE,all.y=TRUE)

bvr_ghg_dis_17_1 <- merge(k600_17, bvr_1_17, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg_dis_17_1 <- merge(bvr_ghg_dis_17_1,atm_ch4_17,by="datetime",all.x=TRUE,all.y=TRUE)
bvr_ghg_dis_17_1 <- merge(bvr_ghg_dis_17_1,atm_co2_17,by="datetime",all.x=TRUE,all.y=TRUE)

bvr_ghg_dis_17_2 <- merge(k600_17, bvr_2_17, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg_dis_17_2 <- merge(bvr_ghg_dis_17_2,atm_ch4_17,by="datetime",all.x=TRUE,all.y=TRUE)
bvr_ghg_dis_17_2 <- merge(bvr_ghg_dis_17_2,atm_co2_17,by="datetime",all.x=TRUE,all.y=TRUE)

# Calculate GHG flux
bvr_ch4flux_dis_16_1<-rep(-99,length(bvr_ghg_dis_16_1$datetime))
bvr_ch4flux_dis_16_2<-rep(-99,length(bvr_ghg_dis_16_2$datetime))

bvr_co2flux_dis_16_1<-rep(-99,length(bvr_ghg_dis_16_1$datetime))
bvr_co2flux_dis_16_2<-rep(-99,length(bvr_ghg_dis_16_2$datetime))

bvr_ch4flux_dis_17_1<-rep(-99,length(bvr_ghg_dis_17_1$datetime))
bvr_ch4flux_dis_17_2<-rep(-99,length(bvr_ghg_dis_17_2$datetime))

bvr_co2flux_dis_17_1<-rep(-99,length(bvr_ghg_dis_17_1$datetime))
bvr_co2flux_dis_17_2<-rep(-99,length(bvr_ghg_dis_17_2$datetime))

# Rates in umol/m2/d
for(i in 1:length(bvr_ghg_dis_16_1$datetime)){
  bvr_ch4flux_dis_16_1[i] <- 1000*bvr_ghg_dis_16_1$k600[i]*((bvr_ghg_dis_16_1$ch4_umol_L[i])-
                                                          bvr_ghg_dis_16_1$ch4_umol_l[i])
  bvr_ch4flux_dis_16_2[i] <- 1000*bvr_ghg_dis_16_2$k600[i]*((bvr_ghg_dis_16_2$ch4_umol_L[i])-
                                                          bvr_ghg_dis_16_2$ch4_umol_l[i])
  bvr_co2flux_dis_16_1[i] <- 1000*bvr_ghg_dis_16_1$k600[i]*((bvr_ghg_dis_16_1$co2_umol_L[i])-
                                                          bvr_ghg_dis_16_1$co2_umol_l[i])
  bvr_co2flux_dis_16_2[i] <- 1000*bvr_ghg_dis_16_2$k600[i]*((bvr_ghg_dis_16_2$co2_umol_L[i])-
                                                          bvr_ghg_dis_16_2$co2_umol_l[i])
}

# Calculate discrete fluxes for 2017 data
for(i in 1:length(bvr_ghg_dis_17_1$datetime)){
  bvr_ch4flux_dis_17_1[i] <- 1000*bvr_ghg_dis_17_1$k600[i]*((bvr_ghg_dis_17_1$ch4_umol_L[i])-
                                                          bvr_ghg_dis_17_1$ch4_umol_l[i])
  bvr_ch4flux_dis_17_2[i] <- 1000*bvr_ghg_dis_17_2$k600[i]*((bvr_ghg_dis_17_2$ch4_umol_L[i])-
                                                          bvr_ghg_dis_17_2$ch4_umol_l[i])
  bvr_co2flux_dis_17_1[i] <- 1000*bvr_ghg_dis_17_1$k600[i]*((bvr_ghg_dis_17_1$co2_umol_L[i])-
                                                          bvr_ghg_dis_17_1$co2_umol_l[i])
  bvr_co2flux_dis_17_2[i] <- 1000*bvr_ghg_dis_17_2$k600[i]*((bvr_ghg_dis_17_2$co2_umol_L[i])-
                                                          bvr_ghg_dis_17_2$co2_umol_l[i])
}

# Calculate avg and std for each time point and combine data frames
bvr_flux_16 <- as.data.frame(bvr_ghg_dis_16_1$datetime)
names(bvr_flux_16)[1] <- "datetime"

bvr_ch4flux_16 <- cbind(bvr_ch4flux_1_16,bvr_ch4flux_2_16)
bvr_flux_16$ch4_avg <- rowMeans(bvr_ch4flux_16)
bvr_flux_16$ch4_std <- apply(bvr_ch4flux_16,1,FUN=sd)

bvr_co2flux_16 <- cbind(bvr_co2flux_1_16,bvr_co2flux_2_16)
bvr_flux_16$co2_avg <- rowMeans(bvr_co2flux_16)
bvr_flux_16$co2_std <- apply(bvr_co2flux_16,1,FUN=sd)

bvr_ch4flux_dis_16 <- cbind(bvr_ch4flux_dis_16_1,bvr_ch4flux_dis_16_2)
bvr_flux_16$ch4_dis_avg <- rowMeans(bvr_ch4flux_dis_16)
bvr_flux_16$ch4_dis_std <- apply(bvr_ch4flux_dis_16,1,FUN=sd)

bvr_co2flux_dis_16 <- cbind(bvr_co2flux_dis_16_1,bvr_co2flux_dis_16_2)
bvr_flux_16$co2_dis_avg <- rowMeans(bvr_co2flux_dis_16)
bvr_flux_16$co2_dis_std <- apply(bvr_co2flux_dis_16,1,FUN=sd)

bvr_flux_16$doy <- as.numeric(strftime(bvr_flux_16$datetime, format = "%j"))

## Try plotting BVR fluxes for 2016
ggplot(bvr_flux_16)+
  geom_line(mapping=aes(x=datetime,y=ch4_avg/1000,color='CH4'))+
  geom_line(mapping=aes(x=datetime,y=co2_avg/1000,color='CO2'))+
  geom_point(mapping=aes(x=datetime,y=ch4_dis_avg/1000,color='CH4'))+
  geom_point(mapping=aes(x=datetime,y=co2_dis_avg/1000,color='CO2'))+
  theme_classic(base_size=17)

# Calculate for BVR 2017
bvr_flux_17 <- as.data.frame(bvr_ghg_dis_17_1$datetime)
names(bvr_flux_17)[1] <- "datetime"

bvr_ch4flux_17 <- cbind(bvr_ch4flux_1_17,bvr_ch4flux_2_17)
bvr_flux_17$ch4_avg <- rowMeans(bvr_ch4flux_17)
bvr_flux_17$ch4_std <- apply(bvr_ch4flux_17,1,FUN=sd)

bvr_co2flux_17 <- cbind(bvr_co2flux_1_17,bvr_co2flux_2_17)
bvr_flux_17$co2_avg <- rowMeans(bvr_co2flux_17)
bvr_flux_17$co2_std <- apply(bvr_co2flux_17,1,FUN=sd)

bvr_ch4flux_dis_17 <- cbind(bvr_ch4flux_dis_17_1,bvr_ch4flux_dis_17_2)
bvr_flux_17$ch4_dis_avg <- rowMeans(bvr_ch4flux_dis_17)
bvr_flux_17$ch4_dis_std <- apply(bvr_ch4flux_dis_17,1,FUN=sd)

bvr_co2flux_dis_17 <- cbind(bvr_co2flux_dis_17_1,bvr_co2flux_dis_17_2)
bvr_flux_17$co2_dis_avg <- rowMeans(bvr_co2flux_dis_17)
bvr_flux_17$co2_dis_std <- apply(bvr_co2flux_dis_17,1,FUN=sd)

bvr_flux_17$doy <- as.numeric(strftime(bvr_flux_17$datetime, format = "%j"))

## Try plotting FCR fluxes for 2017
ggplot(bvr_flux_17)+
  geom_line(mapping=aes(x=datetime,y=ch4_avg/1000,color='CH4'))+
  geom_line(mapping=aes(x=datetime,y=co2_avg/1000,color='CO2'))+
  geom_point(mapping=aes(x=datetime,y=ch4_dis_avg/1000,color='CH4'))+
  geom_point(mapping=aes(x=datetime,y=co2_dis_avg/1000,color='CO2'))+
  theme_classic(base_size=17)

############PLOTS##############
# Plot both reservoirs on the same plot, but for different years
# Methane
ch4_16 <- ggplot()+
  geom_line(fcr_flux_16,mapping=aes(x=datetime,y=ch4_avg/1000,group=1,color='FCR'),size=1.1)+
  geom_line(bvr_flux_16,mapping=aes(x=datetime,y=ch4_avg/1000,group=1,color='BVR'),size=1.1)+
  geom_errorbar(fcr_flux_16,mapping=aes(x=datetime,y=ch4_dis_avg/1000,
                                        ymin=(ch4_dis_avg-ch4_dis_std)/1000,
                                        ymax=(ch4_dis_avg+ch4_dis_std)/1000,color='FCR'),
                size=1)+
  geom_errorbar(bvr_flux_16,mapping=aes(x=datetime,y=ch4_dis_avg/1000,
                                       ymin=(ch4_dis_avg-ch4_dis_std)/1000,
                                       ymax=(ch4_dis_avg+ch4_dis_std)/1000,color="BVR"),
                size=1)+
  geom_point(fcr_flux_16,mapping=aes(x=datetime,y=ch4_dis_avg/1000,group=1,color='FCR'),
             size=4)+
  geom_point(bvr_flux_16,mapping=aes(x=datetime,y=ch4_dis_avg/1000,group=1,color='BVR'),
             size=4)+
  geom_hline(yintercept=0,color="gray",size=0.6)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-09"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-12"))+
  xlab('2016')+
  ylab(expression(paste("CH"[4]*" flux (mmol m"^-2*" d"^-1*")")))+
  theme_classic(base_size=17)

ch4_17 <- ggplot()+
  geom_line(fcr_flux_17,mapping=aes(x=datetime,y=ch4_avg/1000,group=1,color='FCR'),size=1.1)+
  geom_line(bvr_flux_17,mapping=aes(x=datetime,y=ch4_avg/1000,group=1,color='BVR'),size=1.1)+
  geom_errorbar(fcr_flux_17,mapping=aes(x=datetime,y=ch4_dis_avg/1000,
                                        ymin=(ch4_dis_avg-ch4_dis_std)/1000,
                                        ymax=(ch4_dis_avg+ch4_dis_std)/1000,color='FCR'),
                size=1)+
  geom_errorbar(bvr_flux_17,mapping=aes(x=datetime,y=ch4_dis_avg/1000,
                                        ymin=(ch4_dis_avg-ch4_dis_std)/1000,
                                        ymax=(ch4_dis_avg+ch4_dis_std)/1000,color="BVR"),
                size=1)+
  geom_point(fcr_flux_17,mapping=aes(x=datetime,y=ch4_dis_avg/1000,group=1,color='FCR'),
             size=4)+
  geom_point(bvr_flux_17,mapping=aes(x=datetime,y=ch4_dis_avg/1000,group=1,color='BVR'),
             size=4)+
  geom_hline(yintercept=0,color="gray",size=0.6)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-25"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-12"))+
  ylim(-0.1,0.5)+
  xlab('2017')+
  ylab('')+
  theme_classic(base_size=17)

ggarrange(ch4_16,ch4_17,common.legend=TRUE,legend="right")

# CO2
co2_16 <- ggplot()+
  geom_line(fcr_flux_16,mapping=aes(x=datetime,y=co2_avg/1000,group=1,color='FCR'),size=1.1)+
  geom_line(bvr_flux_16,mapping=aes(x=datetime,y=co2_avg/1000,group=1,color='BVR'),size=1.1)+
  geom_errorbar(fcr_flux_16,mapping=aes(x=datetime,y=co2_dis_avg/1000,
                                        ymin=(co2_dis_avg-co2_dis_std)/1000,
                                        ymax=(co2_dis_avg+co2_dis_std)/1000,color='FCR'),
                size=1)+
  geom_errorbar(bvr_flux_16,mapping=aes(x=datetime,y=co2_dis_avg/1000,
                                        ymin=(co2_dis_avg-co2_dis_std)/1000,
                                        ymax=(co2_dis_avg+co2_dis_std)/1000,color="BVR"),
                size=1)+
  geom_point(fcr_flux_16,mapping=aes(x=datetime,y=co2_dis_avg/1000,group=1,color='FCR'),
             size=4)+
  geom_point(bvr_flux_16,mapping=aes(x=datetime,y=co2_dis_avg/1000,group=1,color='BVR'),
             size=4)+
  geom_hline(yintercept=0,color="gray",size=0.6)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-09"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-12"))+
  xlab('2016')+
  ylab(expression(paste("CO"[2]*" flux (mmol m"^-2*" d"^-1*")")))+
  theme_classic(base_size=17)

co2_17 <- ggplot()+
  geom_line(fcr_flux_17,mapping=aes(x=datetime,y=co2_avg/1000,group=1,color='FCR'),size=1.1)+
  geom_line(bvr_flux_17,mapping=aes(x=datetime,y=co2_avg/1000,group=1,color='BVR'),size=1.1)+
  geom_errorbar(fcr_flux_17,mapping=aes(x=datetime,y=co2_dis_avg/1000,
                                        ymin=(co2_dis_avg-co2_dis_std)/1000,
                                        ymax=(co2_dis_avg+co2_dis_std)/1000,color='FCR'),
                size=1)+
  geom_errorbar(bvr_flux_17,mapping=aes(x=datetime,y=co2_dis_avg/1000,
                                        ymin=(co2_dis_avg-co2_dis_std)/1000,
                                        ymax=(co2_dis_avg+co2_dis_std)/1000,color="BVR"),
                size=1)+
  geom_point(fcr_flux_17,mapping=aes(x=datetime,y=co2_dis_avg/1000,group=1,color='FCR'),
             size=4)+
  geom_point(bvr_flux_17,mapping=aes(x=datetime,y=co2_dis_avg/1000,group=1,color='BVR'),
             size=4)+
  geom_hline(yintercept=0,color="gray",size=0.6)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-25"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-12"))+
  xlab('2017')+
  ylab('')+
  ylim(-11,11)+
  theme_classic(base_size=17)

ggarrange(co2_16,co2_17,common.legend=TRUE,legend="right")
