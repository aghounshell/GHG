### Script to plot Schmidt stability for FCR and BVR during 2016, 2017, and 2018
### Schmidt Stability calculated by Lake Analyzer in Matlab
### A Hounshell, 13 Jan 20

# Load libraires
pacman::p_load(tidyverse,ggplot2,ggpubr,lubridate)

# Load Schmidt stabilities: as calculated by Lake Analyzer
fcr <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/FCR_SchmidtStability.csv")
fcr$DateTime <- as.POSIXct(strptime(fcr$DateTime, "%m/%d/%Y", tz="EST"))

bvr <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/BVR_SchmidtStability.csv")
bvr$DateTime <- as.POSIXct(strptime(bvr$DateTime, "%m/%d/%Y", tz="EST"))

# Separate by year and convert to DOY
fcr_16 <- fcr %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
fcr_16$doy <- yday(fcr_16$DateTime)
fcr_17 <- fcr %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
fcr_17$doy <- yday(fcr_17$DateTime)
fcr_18 <- fcr %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))
fcr_18$doy <- yday(fcr_18$DateTime)

bvr_16 <- bvr %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
bvr_16$doy <- yday(bvr_16$DateTime)
bvr_17 <- bvr %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
bvr_17$doy <- yday(bvr_17$DateTime)
bvr_18 <- bvr %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))
bvr_18$doy <- yday(bvr_18$DateTime)

# Plot each as DOY for each year on same graph: 1. FCR and 2. BVR
fcr <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=doy,y=St,group=1,color="fcr_16"),size=1.1)+
  geom_line(data=fcr_17,mapping=aes(x=doy,y=St,group=1,color="fcr_17"),size=1.1)+
  geom_line(data=fcr_18,mapping=aes(x=doy,y=St,group=1,color="fcr_18"),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=doy,y=St,group=1,color="fcr_16"),size=4)+
  geom_point(data=fcr_17,mapping=aes(x=doy,y=St,group=1,color="fcr_17"),size=4)+
  geom_point(data=fcr_18,mapping=aes(x=doy,y=St,group=1,color="fcr_18"),size=4)+
  geom_vline(xintercept = 280,linetype="dashed",color="#91bfff",size=1)+ #Turnover FCR 2016
  geom_vline(xintercept = 303,linetype="dashed",color="#2c2cff",size=1)+ #Turnover FCR 2017
  geom_vline(xintercept = 294,linetype="dashed",color="#0F2080",size=1)+ #Turnover FCR 2018
  scale_color_manual(breaks=c("fcr_16","fcr_17","fcr_18"), labels=c("FCR 2016","FCR 2017","FCR 2018"),
                     values=c('#91bfff','#2c2cff','#0F2080'))+
  xlim(91,334)+
  ylab(expression(paste("Schmidt Stability (J m"^-2*")")))+
  xlab("Day of Year")+
  labs(color="")+
  theme_classic(base_size=15)

bvr <- ggplot()+
  geom_line(data=bvr_16,mapping=aes(x=doy,y=St,group=1,color="bvr_16"),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=doy,y=St,group=1,color="bvr_17"),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=doy,y=St,group=1,color="bvr_18"),size=1.1)+
  geom_point(data=bvr_16,mapping=aes(x=doy,y=St,group=1,color="bvr_16"),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=doy,y=St,group=1,color="bvr_17"),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=doy,y=St,group=1,color="bvr_18"),size=4)+
  geom_vline(xintercept = 315,linetype="dashed",color="#ffa700",size=1)+ #Turnover BVR 2016
  geom_vline(xintercept = 311,linetype="dashed",color="#ffa07a",size=1)+ #Turnover BVR 2017
  geom_vline(xintercept = 302,linetype="dashed",color="#F5793A",size=1)+ #Turnover BVR 2018
  scale_color_manual(breaks=c("bvr_16","bvr_17","bvr_18"), labels=c("BVR 2016","BVR 2017","BVR 2018"),
                     values=c('#ffa700','#ffa07a','#F5793A'))+
  xlim(91,334)+
  ylab(expression(paste("Schmidt Stability (J m"^-2*")")))+
  xlab("Day of Year")+
  labs(color="")+
  theme_classic(base_size=15)

ggarrange(fcr,bvr,legend="right",ncol=2,nrow=1)
