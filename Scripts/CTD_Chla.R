### Script to plot Chla at 1.0 m for both FCR and BVR for 2016, 2017, 2018 using CTD Data
### A Hounshell, 20 May 2020

# Saved Rfile: Flora_Phyto

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load in CTD data: use YSI + CTD merged for both FCR and BVR
fcr <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18.csv")
fcr$Date <- as.POSIXct(strptime(fcr$Date, "%Y-%m-%d", tz = "EST"))

bvr <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18.csv")
bvr$Date <- as.POSIXct(strptime(bvr$Date, "%Y-%m-%d", tz = "EST"))

# Select data around 1.0 m
fcr_1 <- fcr %>% select(Date,Depth_m,Chla_ugL) %>% filter(Depth_m>=0 & Depth_m<1.6) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_1")

fcr_1 <- fcr_1[complete.cases(fcr_1),]

bvr_1 <- bvr %>% select(Date,Depth_m,Chla_ugL) %>% filter(Depth_m>=0 & Depth_m<1.6) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="BVR_1")

bvr_1 <- bvr_1[complete.cases(bvr_1),]

# Plot
ggplot(fcr_1,aes(x=Date,y=Chla_ugL))+
  geom_line()+
  geom_point()+
  theme_classic(base_size=15)

# Let's look at FloraData that's available...
flora <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/FluoroProbe.csv")
flora$DateTime <- as.POSIXct(strptime(flora$DateTime, "%m/%d/%Y", tz = "EST"))

fcr_flora <- flora %>% filter(Reservoir == 'FCR' & Site == "50") %>% 
  filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2018-12-31'))

bvr_flora <- flora %>% filter(Reservoir == 'BVR' & Site == "50") %>% 
  filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2018-12-31'))

fcr_flora_1 <- fcr_flora %>% select(DateTime,Depth_m,TotalConc_ugL) %>% filter(Depth_m>=0.8 & Depth_m<1.2) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_1")

bvr_flora_1 <- bvr_flora %>% select(DateTime,Depth_m,TotalConc_ugL) %>% filter(Depth_m>=0.8 & Depth_m<1.2) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_1")

# Plot Flora data
ggplot()+
  geom_line(bvr_flora_1,mapping=aes(x=DateTime,y=TotalConc_ugL,color='BVR'))+
  geom_point(bvr_flora_1,mapping=aes(x=DateTime,y=TotalConc_ugL,color='BVR'))+
  geom_line(fcr_flora_1,mapping=aes(x=DateTime,y=TotalConc_ugL,color='FCR'))+
  geom_point(fcr_flora_1,mapping=aes(x=DateTime,y=TotalConc_ugL,color='FCR'))+
  theme_classic(base_size=15)

# Separate by year and make pretty graph
fcr_16 <- fcr_flora_1 %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
fcr_17 <- fcr_flora_1 %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
fcr_18 <- fcr_flora_1 %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

bvr_16 <- bvr_flora_1 %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
bvr_17 <- bvr_flora_1 %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
bvr_18 <- bvr_flora_1 %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

phyto_16 <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_16,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_16,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='BVR'),size=4)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("Total phytoplankton (", mu,"g L"^-1*")")))+
  ylim(0,50)+
  theme_classic(base_size=15)+
  theme(legend.position=c(0.2,0.9))

phyto_17 <- ggplot()+
  geom_line(data=fcr_17,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_17,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='BVR'),size=4)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab(expression(paste("Total phytoplankton (", mu,"g L"^-1*")")))+
  ylim(0,50)+
  theme_classic(base_size=15)+
  theme(legend.position="none")

phyto_18 <- ggplot()+
  geom_line(data=fcr_18,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_18,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=DateTime,y=TotalConc_ugL,group=1,color='BVR'),size=4)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-30"),color="#0F2080")+ # SSS Off in FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab(expression(paste("Total phytoplankton (", mu,"g L"^-1*")")))+
  ylim(0,50)+
  theme_classic(base_size=15)+
  theme(legend.position="none")

ggarrange(phyto_16,phyto_17,phyto_18,common.legend=FALSE,ncol=2,nrow=2)
