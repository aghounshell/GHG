### Script to calculate CO2 and CH4 flux from FCR and BVR from Summer 2015 to Summer 2017
### A Hounshell, 10 Jun 2019

# Load libraries
library(LakeMetabolizer)
library(zoo)
library(tidyverse)
library(scales)

# Set working directory: using only output data files
# Wind data: calculated for FCR when met data is not available
# GHG data for BVR and FCR separated
setwd("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output")

# Read in .csv for FCR data, BVR data, and wind data
fcr <- read.csv("GHG_FCR_0.1m.csv",header=TRUE)
bvr <- read.csv("GHG_BVR_0.1m.csv",header=TRUE)
wind <- read.csv("Calc_FCR_Wind.csv",header=TRUE)

# Calculate 'k' for GHG calculations using the Cole method
k600 <- k.cole.base(wind$met_wind) # units in m/day
k600data <- cbind(wind, k600)
names(k600data)[1] <- "datetime"

# Merge k600 data and GHG data for FCR
# Then approximate GHG data in between sample dates (use linear interpolation)
fcr_ghg <- merge(k600data, fcr, by="datetime", all.x=TRUE, all.y=TRUE)
fcr_ghg$co2_umol_L <- na.approx(fcr_ghg$co2_umol_L,na.rm=FALSE)
fcr_ghg$ch4_umol_L <- na.approx(fcr_ghg$ch4_umol_L,na.rm=FALSE)

# Calculate GHG flux
ch4flux<-rep(-99,length(fcr_ghg$datetime)) # assuming Catm = 1.8 ppm or 0.1125 umol
co2flux<-rep(-99,length(fcr_ghg$datetime))

# Need to figure out where to get CH4 and CO2 atmospheric values***
# Aka: what are the subtracted values below??
# Rates in umol/m2/d
for(i in 1:length(fcr_ghg$datetime)){
  ch4flux[i]<-1000*k600[i]*((fcr_ghg$ch4_umol_L[i])-0.0118)
  co2flux[i]<-1000*k600[i]*((fcr_ghg$co2_umol_L[i])-21.53)
}

# Also calculate fluxes for days where there are discrete measurements
fcr_ghg_dis <- merge(k600data, fcr, by="datetime", all.x=TRUE, all.y=TRUE)

# Calculate GHG flux
ch4flux_dis<-rep(-99,length(fcr_ghg_dis$datetime)) # assuming Catm = 1.8 ppm or 0.1125 umol
co2flux_dis<-rep(-99,length(fcr_ghg_dis$datetime))

# Need to figure out where to get CH4 and CO2 atmospheric values***
# Aka: what are the subtracted values below??
# Rates in umol/m2/d
for(i in 1:length(fcr_ghg_dis$datetime)){
  ch4flux_dis[i]<-1000*k600[i]*((fcr_ghg_dis$ch4_umol_L[i])-0.0118)
  co2flux_dis[i]<-1000*k600[i]*((fcr_ghg_dis$co2_umol_L[i])-21.53)
}

# Combine data frames
fcr_flux <- cbind.data.frame(fcr_ghg$datetime,ch4flux,co2flux,ch4flux_dis,co2flux_dis)
names(fcr_flux)[1] <- "datetime"
fcr_flux$datetime <- as.POSIXct(strptime(fcr_flux$datetime,format="%Y-%m-%dT%H:%M:%SZ", tz = "EST"))

write_csv(fcr_flux,path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/FCR_Flux_All.csv")

### Do the same for BVR - calculate fluxes

# Merge k600 data and GHG data for BVR
# Then approximate GHG data in between sample dates (use linear interpolation)
bvr_ghg <- merge(k600data, bvr, by="datetime", all.x=TRUE, all.y=TRUE)
bvr_ghg$co2_umol_L <- na.approx(bvr_ghg$co2_umol_L,na.rm=FALSE)
bvr_ghg$ch4_umol_L <- na.approx(bvr_ghg$ch4_umol_L,na.rm=FALSE)

# Calculate GHG flux
ch4flux_bvr<-rep(-99,length(bvr_ghg$datetime)) # assuming Catm = 1.8 ppm or 0.1125 umol
co2flux_bvr<-rep(-99,length(bvr_ghg$datetime))

# Need to figure out where to get CH4 and CO2 atmospheric values***
# Aka: what are the subtracted values below??
# Rates in umol/m2/d
for(i in 1:length(bvr_ghg$datetime)){
  ch4flux_bvr[i]<-1000*k600[i]*((bvr_ghg$ch4_umol_L[i])-0.0118)
  co2flux_bvr[i]<-1000*k600[i]*((bvr_ghg$co2_umol_L[i])-21.53)
}

# Calculate fluxes for days with discrete measurements
bvr_ghg_dis <- merge(k600data, bvr, by="datetime", all.x=TRUE, all.y=TRUE)

# Calculate GHG flux
ch4flux_dis_bvr<-rep(-99,length(bvr_ghg_dis$datetime)) # assuming Catm = 1.8 ppm or 0.1125 umol
co2flux_dis_bvr<-rep(-99,length(bvr_ghg_dis$datetime))

# Need to figure out where to get CH4 and CO2 atmospheric values***
# Aka: what are the subtracted values below??
# Rates in umol/m2/d
for(i in 1:length(bvr_ghg_dis$datetime)){
  ch4flux_dis_bvr[i]<-1000*k600[i]*((bvr_ghg_dis$ch4_umol_L[i])-0.0118)
  co2flux_dis_bvr[i]<-1000*k600[i]*((bvr_ghg_dis$co2_umol_L[i])-21.53)
}

# Combine data frames
bvr_flux <- cbind.data.frame(bvr_ghg$datetime,ch4flux_bvr,co2flux_bvr,ch4flux_dis_bvr,co2flux_dis_bvr)
names(bvr_flux)[1] <- "datetime"
bvr_flux$datetime <- as.POSIXct(strptime(bvr_flux$datetime,format="%Y-%m-%dT%H:%M:%SZ", tz = "EST"))

write_csv(bvr_flux,path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/BVR_Flux_All.csv")

### Plot results
fcr_ch4_all <- ggplot(fcr_flux,aes(x=datetime,y=ch4flux,group=1))+geom_line()+
  geom_point(x=fcr_flux$datetime,y=fcr_flux$ch4flux_dis)+
  labs(x="Date",y="FCR CH4 flux (umol/m2/d)")+
  scale_x_datetime(breaks=date_breaks("5 month"))+
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_ch4_flux_all.jpg",fcr_ch4_all,width=15,height=10)

fcr_co2_all <- ggplot(fcr_flux,aes(x=datetime,y=co2flux,group=1))+geom_line()+
  geom_point(x=fcr_flux$datetime,y=fcr_flux$co2flux_dis)+
  geom_hline(yintercept=0,linetype="dashed")+
  labs(x="Date",y="FCR CO2 flux (umol/m2/d)")+
  scale_x_datetime(breaks=date_breaks("5 month"))+
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_co2_flux_all.jpg",fcr_co2_all,width=15,height=10)

bvr_ch4_all <- ggplot(bvr_flux,aes(x=datetime,y=ch4flux_bvr,group=1))+geom_line()+
  geom_point(x=bvr_flux$datetime,y=bvr_flux$ch4flux_dis_bvr)+
  labs(x="Date",y="BVR CH4 flux (umol/m2/d)")+
  scale_x_datetime(breaks=date_breaks("5 month"))+
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_ch4_flux_all.jpg",bvr_ch4_all,width=15,height=10)

bvr_co2_all <- ggplot(bvr_flux,aes(x=datetime,y=co2flux_bvr,group=1))+geom_line()+
  geom_point(x=bvr_flux$datetime,y=bvr_flux$co2flux_dis_bvr)+
  labs(x="Date",y="BVR CO2 flux (umol/m2/d)")+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_x_datetime(breaks=date_breaks("5 month"))+
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_co2_flux_all.jpg",bvr_co2_all,width=15,height=10)

## Break up graphs into years
# Start the end of March - December
# For 2016 and 2017
fcr_flux_2016 <- fcr_flux %>% filter(datetime>=as.Date("2016-03-30") & datetime<=as.Date("2016-12-03"))
fcr_flux_2017 <- fcr_flux %>% filter(datetime>=as.Date("2017-03-27") & datetime<=as.Date("2017-12-13"))

bvr_flux_2016 <- bvr_flux %>% filter(datetime>=as.Date("2016-04-06") & datetime<=as.Date("2016-11-12"))
bvr_flux_2017 <- bvr_flux %>% filter(datetime>=as.Date("2017-05-05") & datetime<=as.Date("2017-10-24"))

# Plot fluxes by year for each reservoir

fcr_2016_ch4 <- ggplot(fcr_flux_2016,aes(x=datetime,y=ch4flux,group=1))+
  geom_line(size=1.5)+
  geom_point(x=fcr_flux_2016$datetime,y=fcr_flux_2016$ch4flux_dis,size=4)+
  labs(x="2016",y=bquote('FCR CH'[4]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  ylim(0,700)+
  geom_vline(xintercept=as.numeric(fcr_flux_2016$datetime[194]),linetype="dashed",size=1)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_ch4_flux_2016.jpg",fcr_2016_ch4,width=15,height=10)

fcr_2017_ch4 <- ggplot(fcr_flux_2017,aes(x=datetime,y=ch4flux,group=1))+
  geom_line(size=1.5)+
  geom_point(x=fcr_flux_2017$datetime,y=fcr_flux_2017$ch4flux_dis,size=3)+
  labs(x="2017",y=bquote('FCR CH'[4]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  ylim(0,300)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_ch4_flux_2017.jpg",fcr_2017_ch4,width=15,height=10)

fcr_2016_co2 <- ggplot(fcr_flux_2016,aes(x=datetime,y=co2flux,group=1))+
  geom_line(size=1.5)+
  geom_point(x=fcr_flux_2016$datetime,y=fcr_flux_2016$co2flux_dis,size=3)+
  geom_vline(xintercept=as.numeric(fcr_flux_2016$datetime[194]),linetype="dashed",size=1.5)+
  labs(x="2016",y=bquote('FCR CO'[2]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  geom_hline(yintercept = 0,linetype="dashed",size=1.5)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_co2_flux_2016.jpg",fcr_2016_co2,width=15,height=10)

fcr_2017_co2 <- ggplot(fcr_flux_2017,aes(x=datetime,y=co2flux,group=1))+
  geom_line(size=1.5)+
  geom_point(x=fcr_flux_2017$datetime,y=fcr_flux_2017$co2flux_dis,size=3)+
  labs(x="2017",y=bquote('FCR CO'[2]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  geom_hline(yintercept = 0,linetype="dashed",size=1.5)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_co2_flux_2017.jpg",fcr_2017_co2,width=15,height=10)

bvr_2016_ch4 <- ggplot(bvr_flux_2016,aes(x=datetime,y=ch4flux_bvr,group=1))+
  geom_line(size=1.5)+
  geom_point(x=bvr_flux_2016$datetime,y=bvr_flux_2016$ch4flux_dis_bvr,size=3)+
  labs(x="2016",y=bquote('BVR CH'[4]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_ch4_flux_2016.jpg",bvr_2016_ch4,width=15,height=10)

bvr_2017_ch4 <- ggplot(bvr_flux_2017,aes(x=datetime,y=ch4flux_bvr,group=1))+
  geom_line(size=1.5)+
  geom_point(x=bvr_flux_2017$datetime,y=bvr_flux_2017$ch4flux_dis_bvr,size=3)+
  labs(x="2017",y=bquote('BVR CH'[4]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  ylim(0,400)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_ch4_flux_2017.jpg",bvr_2017_ch4,width=15,height=10)

bvr_2016_co2 <- ggplot(bvr_flux_2016,aes(x=datetime,y=co2flux_bvr,group=1))+
  geom_line(size=1.5)+
  geom_point(x=bvr_flux_2016$datetime,y=bvr_flux_2016$co2flux_dis_bvr,size=3)+
  labs(x="2016",y=bquote('BVR CO'[2]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  geom_hline(yintercept=0,linetype="dashed",size=1.5)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30,color="black"))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_co2_flux_2016.jpg",bvr_2016_co2,width=15,height=10)

bvr_2017_co2 <- ggplot(bvr_flux_2017,aes(x=datetime,y=co2flux_bvr,group=1))+
  geom_line(size=1.5)+
  geom_point(x=bvr_flux_2017$datetime,y=bvr_flux_2017$co2flux_dis_bvr,size=3)+
  labs(x="2017",y=bquote('BVR CO'[2]~ 'Flux ('*mu*'mol'~m^-2~ d^-1*')'))+
  geom_hline(yintercept=0,linetype="dashed",size=1.5)+
  theme_classic(base_size=30)+
  theme(axis.text=element_text(size=30))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_co2_flux_2017.jpg",bvr_2017_co2,width=15,height=10)

# Save as Rfile: GHG_Fluxes