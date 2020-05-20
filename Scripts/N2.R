### Script plot Bouyancy frequency (calculated by LakeAnalyzer in Matlab)
### for FCR and BVR in 2016, 2017, and 2018
### A Hounshell, 20 May 2020

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load in data for FCR and BVR
fcr <- read_csv('C:/Users/ahoun/OneDrive/Desktop/GHG/Data/FCR_results_N2.csv')
fcr$DateTime <- as.POSIXct(strptime(fcr$DateTime, "%m/%d/%Y", tz = "EST"))

bvr <- read_csv('C:/Users/ahoun/OneDrive/Desktop/GHG/Data/BVR_results_N2.csv')
bvr$DateTime <- as.POSIXct(strptime(bvr$DateTime, "%m/%d/%Y", tz = "EST"))

fcr_16 <- fcr %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
fcr_17 <- fcr %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
fcr_18 <- fcr %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

bvr_16 <- bvr %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
bvr_17 <- bvr %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
bvr_18 <- bvr %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

n2_2016 <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=DateTime,y=N2,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_16,mapping=aes(x=DateTime,y=N2,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=DateTime,y=N2,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_16,mapping=aes(x=DateTime,y=N2,group=1,color='BVR'),size=4)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("Buoyancy Frequency (s"^-2*")")))+
  ylim(0,0.05)+
  theme_classic(base_size=15)+
  theme(legend.position=c(0.3,0.9))

n2_2017 <- ggplot()+
  geom_line(data=fcr_17,mapping=aes(x=DateTime,y=N2,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=DateTime,y=N2,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_17,mapping=aes(x=DateTime,y=N2,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=DateTime,y=N2,group=1,color='BVR'),size=4)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab(expression(paste("Buoyancy Frequency (s"^-2*")")))+
  ylim(0,0.050)+
  theme_classic(base_size=15)+
  theme(legend.position="none")

n2_2018 <- ggplot()+
  geom_line(data=fcr_18,mapping=aes(x=DateTime,y=N2,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=DateTime,y=N2,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_18,mapping=aes(x=DateTime,y=N2,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=DateTime,y=N2,group=1,color='BVR'),size=4)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-30"),color="#0F2080")+ # SSS Off in FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab(expression(paste("Buoyancy Frequency (s"^-2*")")))+
  ylim(0,0.050)+
  theme_classic(base_size=15)+
  theme(legend.position="none")

ggarrange(n2_2016,n2_2017,n2_2018,common.legend=FALSE,ncol=2,nrow=2)
