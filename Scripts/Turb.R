## Script to plot surface turbidity for 2016, 2017, and 2018
## To assess impacts of 'browning' on hypo temp (as visualized by SWI)
## A Hounshell, 25 Feb 2020

# Load in libraries
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load in data: start with BVR
# BVR Volumes by depth
depth_b <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/BVR_Vol.csv")
# BVR merged YSI and CTD casts
casts_b <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18.csv")
casts_b$Date <- as.POSIXct(strptime(casts_b$Date, "%Y-%m-%d", tz = "EST"))

# Load in data: FCR
# FCR Volumes by depth
depth_f <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Vol.csv")
# FCR merged YSI and CTD casts
casts_f <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18.csv")
casts_f$Date <- as.POSIXct(strptime(casts_f$Date, "%Y-%m-%d", tz = "EST"))

# Separate out BVR surface values
b_casts_1 <- casts_b %>% select(Date,Depth_m,Turb_NTU) %>% filter(Depth_m>=0 & Depth_m<1) %>% 
  group_by(Date) %>% summarize_all(funs(mean(.,na.rm=TRUE))) %>% arrange(Date) %>% mutate(grouping="BVR_1")

# Separate out FCR surface values
f_casts_1 <- casts_f %>% select(Date,Depth_m,Turb_NTU) %>% filter(Depth_m>=0 & Depth_m<1.3) %>% 
  group_by(Date) %>% summarize_all(funs(mean(.,na.rm=TRUE))) %>% arrange(Date) %>% mutate(grouping="FCR_1")

# Select complete cases
b_casts_1 <- na.omit(b_casts_1)
f_casts_1 <- na.omit(f_casts_1)

# Separate by year
b_turb_16 <- b_casts_1 %>% filter(Date>=as.Date('2016-01-01')&Date<=as.Date('2016-12-31'))
b_turb_16$doy <- yday(b_turb_16$Date)
b_turb_17 <- b_casts_1 %>% filter(Date>=as.Date('2017-01-01')&Date<=as.Date('2017-12-31'))
b_turb_17$doy <- yday(b_turb_17$Date)
b_turb_18 <- b_casts_1 %>% filter(Date>=as.Date('2018-01-01')&Date<=as.Date('2018-12-31'))
b_turb_18$doy <- yday(b_turb_18$Date)

f_turb_16 <- f_casts_1 %>% filter(Date>=as.Date('2016-01-01')&Date<=as.Date('2016-12-31'))
f_turb_16$doy <- yday(f_turb_16$Date)
f_turb_17 <- f_casts_1 %>% filter(Date>=as.Date('2017-01-01')&Date<=as.Date('2017-12-31'))
f_turb_17$doy <- yday(f_turb_17$Date)
f_turb_18 <- f_casts_1 %>% filter(Date>=as.Date('2018-01-01')&Date<=as.Date('2018-12-31'))
f_turb_18$doy <- yday(f_turb_18$Date)

# Plot!
fcr <- ggplot()+
  geom_line(data=f_turb_16,aes(x=doy,y=Turb_NTU,color="2016"),size=1.1)+
  geom_point(data=f_turb_16,aes(x=doy,y=Turb_NTU,color="2016"),size=4)+
  geom_line(data=f_turb_17,aes(x=doy,y=Turb_NTU,color="2017"),size=1.1)+
  geom_point(data=f_turb_17,aes(x=doy,y=Turb_NTU,color="2017"),size=4)+
  geom_line(data=f_turb_18,aes(x=doy,y=Turb_NTU,color="2018"),size=1.1)+
  geom_point(data=f_turb_18,aes(x=doy,y=Turb_NTU,color="2018"),size=4)+
  scale_color_manual(breaks=c("2016","2017","2018"), labels=c("FCR 2016","FCR 2017","FCR 2018"),
                     values=c('#91bfff','#2c2cff','#0F2080'))+
  geom_vline(xintercept = 280,linetype="dashed",color="#91bfff",size=1)+ #Turnover FCR 2016
  geom_vline(xintercept = 298,linetype="dashed",color="#2c2cff",size=1)+ #Turnover FCR 2017
  geom_vline(xintercept = 294,linetype="dashed",color="#0F2080",size=1)+ #Turnover FCR 2018
  xlab("Day of Year")+
  labs(color="")+
  ylab("Turbidity (NTU)")+
  ylim(0,35)+
  xlim(0,365)+
  theme_classic(base_size=15)

bvr <- ggplot()+
  geom_line(data=b_turb_16,aes(x=doy,y=Turb_NTU,color="2016"),size=1.1)+
  geom_point(data=b_turb_16,aes(x=doy,y=Turb_NTU,color="2016"),size=4)+
  geom_line(data=b_turb_17,aes(x=doy,y=Turb_NTU,color="2017"),size=1.1)+
  geom_point(data=b_turb_17,aes(x=doy,y=Turb_NTU,color="2017"),size=4)+
  geom_line(data=b_turb_18,aes(x=doy,y=Turb_NTU,color="2018"),size=1.1)+
  geom_point(data=b_turb_18,aes(x=doy,y=Turb_NTU,color="2018"),size=4)+
  geom_vline(xintercept = 315,linetype="dashed",color="#ffa700",size=1)+ #Turnover BVR 2016
  geom_vline(xintercept = 311,linetype="dashed",color="#ffa07a",size=1)+ #Turnover BVR 2017
  geom_vline(xintercept = 302,linetype="dashed",color="#F5793A",size=1)+ #Turnover BVR 2018
  scale_color_manual(breaks=c("2016","2017","2018"), labels=c("BVR 2016","BVR 2017","BVR 2018"),
                     values=c('#ffa700','#ffa07a','#F5793A'))+
  xlab("Day of Year")+
  labs(color="")+
  ylab("Turbidity (NTU)")+
  ylim(0,35)+
  xlim(0,365)+
  theme_classic(base_size=15)

ggarrange(fcr,bvr,legend="right",ncol=2,nrow=1)
