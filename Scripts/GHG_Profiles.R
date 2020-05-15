### Script to plot profiles of GHGs for 2016, 2017, and 2018 in FCR and BVR
### In response to LO-L Reveiwer comments
### A Hounshell, 15 May 20

# Rfile saved as: GHG_Profiles

# Load libraries
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load GHG Data: 'final' GHG database from RPM
ghg <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/DATASET_for_EDI_LOL_MS_10May20.csv")
ghg$DateTime <- as.POSIXct(strptime(ghg$DateTime, "%m/%d/%Y", tz="EST"))

# Select for FCR and BVR
fcr <- ghg %>% filter(Reservoir == "FCR")
bvr <- ghg %>% filter(Reservoir == "BVR")

# Separate each depth for FCR
fcr_1 <- fcr %>% filter(Depth_m==0.1) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_1")

fcr_2 <- fcr %>% filter(Depth_m==1.6) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_2")

fcr_3 <- fcr %>% filter(Depth_m==3.8) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_3")

fcr_4 <- fcr %>% filter(Depth_m==5) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_4")

fcr_5 <- fcr %>% filter(Depth_m==6.2) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_5")

fcr_6 <- fcr %>% filter(Depth_m==8) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_6")

fcr_7 <- fcr %>% filter(Depth_m==9) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="FCR_7")

fcr_merge <- rbind(fcr_1,fcr_2,fcr_3,fcr_4,fcr_5,fcr_6,fcr_7)

# Select by year
fcr_16 <- fcr_merge %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
fcr_17 <- fcr_merge %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
fcr_18 <- fcr_merge %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))

# Separate each depth for BVR
bvr_1 <- bvr %>% filter(Depth_m==0.1) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="BVR_1")

bvr_2 <- bvr %>% filter(Depth_m==3.0) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="BVR_2")

bvr_3 <- bvr %>% filter(Depth_m==6.0) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="BVR_3")

bvr_4 <- bvr %>% filter(Depth_m==9.0) %>% select(DateTime,Depth_m,ch4_umolL,co2_umolL) %>% 
  group_by(DateTime) %>% summarize_all(funs(mean,sd),na.rm=TRUE) %>% arrange(DateTime) %>% mutate(grouping="BVR_4")

bvr_merge <- rbind(bvr_1,bvr_2,bvr_3,bvr_4)

# Select by year
bvr_16 <- bvr_merge %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
bvr_17 <- bvr_merge %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
bvr_18 <- bvr_merge %>% filter(DateTime>=as.Date('2018-01-01')&DateTime<=as.Date('2018-12-31'))


## Plot CO2
fcr_16_co2 <- ggplot(fcr_16,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(fcr_16,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping,ymin=co2_umolL_mean-co2_umolL_sd,
                                   ymax=co2_umolL_mean+co2_umolL_sd),size=1)+
  scale_color_manual(breaks=c("FCR_1","FCR_2","FCR_3","FCR_4","FCR_5","FCR_6","FCR_7"), 
                     labels=c("0.1 m","1.6 m","3.8 m","5.0 m","6.2 m","8.0 m","9.0 m"),
                     values=c('#F94144','#F3722C','#F8961E','#F9C74F','#90BE6D','#43AA8B','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("Dissolved CO"[2]*" (", mu,"mol L"^-1*")")))+
  ggtitle("FCR")+
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="black")+ #Turnover FCR
  theme_classic(base_size=15)+
  theme(legend.position=c(0.15,0.75))

fcr_17_co2 <- ggplot(fcr_17,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(fcr_17,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping,ymin=co2_umolL_mean-co2_umolL_sd,
                                   ymax=co2_umolL_mean+co2_umolL_sd),size=1)+
  scale_color_manual(breaks=c("FCR_1","FCR_2","FCR_3","FCR_4","FCR_5","FCR_6","FCR_7"), 
                     labels=c("0.1 m","1.6 m","3.8 m","5.0 m","6.2 m","8.0 m","9.0 m"),
                     values=c('#F94144','#F3722C','#F8961E','#F9C74F','#90BE6D','#43AA8B','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab(expression(paste("Dissolved CO"[2]*" (", mu,"mol L"^-1*")")))+
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="black")+ #Turnover FCR
  theme_classic(base_size=15)+
  theme(legend.position='none')

fcr_18_co2 <- ggplot(fcr_18,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(fcr_18,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping,ymin=co2_umolL_mean-co2_umolL_sd,
                                   ymax=co2_umolL_mean+co2_umolL_sd),size=1)+
  scale_color_manual(breaks=c("FCR_1","FCR_2","FCR_3","FCR_4","FCR_5","FCR_6","FCR_7"), 
                     labels=c("0.1 m","1.6 m","3.8 m","5.0 m","6.2 m","8.0 m","9.0 m"),
                     values=c('#F94144','#F3722C','#F8961E','#F9C74F','#90BE6D','#43AA8B','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab(expression(paste("Dissolved CO"[2]*" (", mu,"mol L"^-1*")")))+
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="black")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-30"),color="black")+ # SSS Off in FCR
  theme_classic(base_size=15)+
  theme(legend.position='none')


bvr_16_co2 <- ggplot(bvr_16,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(bvr_16,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping,ymin=co2_umolL_mean-co2_umolL_sd,
                                   ymax=co2_umolL_mean+co2_umolL_sd),size=1)+
  scale_color_manual(breaks=c("BVR_1","BVR_2","BVR_3","BVR_4"), 
                     labels=c("0.1 m","3.0 m","6.0 m","9.0 m"),
                     values=c('#F94144','#F9C74F','#90BE6D','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab("")+
  ggtitle("BVR")+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="black")+ #Turnover
  theme_classic(base_size=15)+
  theme(legend.position=c(0.15,0.85))

bvr_17_co2 <- ggplot(bvr_17,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(bvr_17,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping,ymin=co2_umolL_mean-co2_umolL_sd,
                                   ymax=co2_umolL_mean+co2_umolL_sd),size=1)+
  scale_color_manual(breaks=c("BVR_1","BVR_2","BVR_3","BVR_4"), 
                     labels=c("0.1 m","3.0 m","6.0 m","9.0 m"),
                     values=c('#F94144','#F9C74F','#90BE6D','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab("")+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="black")+ #Turnover
  theme_classic(base_size=15)+
  theme(legend.position='none')

bvr_18_co2 <- ggplot(bvr_18,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(bvr_18,mapping=aes(x=DateTime,y=co2_umolL_mean,color=grouping,ymin=co2_umolL_mean-co2_umolL_sd,
                                   ymax=co2_umolL_mean+co2_umolL_sd),size=1)+
  scale_color_manual(breaks=c("BVR_1","BVR_2","BVR_3","BVR_4"), 
                     labels=c("0.1 m","3.0 m","6.0 m","9.0 m"),
                     values=c('#F94144','#F9C74F','#90BE6D','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab("")+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="black")+ #Turnover
  theme_classic(base_size=15)+
  theme(legend.position='none')

ggarrange(fcr_16_co2,bvr_16_co2,fcr_17_co2,bvr_17_co2,fcr_18_co2,bvr_18_co2,common.legend=FALSE,ncol=2,nrow=3)
# Saved as 900 x 1000

## Plot CH4
fcr_16_ch4 <- ggplot(fcr_16,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(fcr_16,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping,ymin=ch4_umolL_mean-ch4_umolL_sd,
                                   ymax=ch4_umolL_mean+ch4_umolL_sd),size=1)+
  scale_color_manual(breaks=c("FCR_1","FCR_2","FCR_3","FCR_4","FCR_5","FCR_6","FCR_7"), 
                     labels=c("0.1 m","1.6 m","3.8 m","5.0 m","6.2 m","8.0 m","9.0 m"),
                     values=c('#F94144','#F3722C','#F8961E','#F9C74F','#90BE6D','#43AA8B','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("Dissolved CH"[4]*" (", mu,"mol L"^-1*")")))+
  ggtitle("FCR")+
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="black")+ #Turnover FCR
  theme_classic(base_size=15)+
  theme(legend.position=c(0.15,0.75))

fcr_17_ch4 <- ggplot(fcr_17,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(fcr_17,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping,ymin=ch4_umolL_mean-ch4_umolL_sd,
                                   ymax=ch4_umolL_mean+ch4_umolL_sd),size=1)+
  scale_color_manual(breaks=c("FCR_1","FCR_2","FCR_3","FCR_4","FCR_5","FCR_6","FCR_7"), 
                     labels=c("0.1 m","1.6 m","3.8 m","5.0 m","6.2 m","8.0 m","9.0 m"),
                     values=c('#F94144','#F3722C','#F8961E','#F9C74F','#90BE6D','#43AA8B','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab(expression(paste("Dissolved CH"[4]*" (", mu,"mol L"^-1*")")))+
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="black")+ #Turnover FCR
  theme_classic(base_size=15)+
  theme(legend.position='none')

fcr_18_ch4 <- ggplot(fcr_18,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(fcr_18,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping,ymin=ch4_umolL_mean-ch4_umolL_sd,
                                   ymax=ch4_umolL_mean+ch4_umolL_sd),size=1)+
  scale_color_manual(breaks=c("FCR_1","FCR_2","FCR_3","FCR_4","FCR_5","FCR_6","FCR_7"), 
                     labels=c("0.1 m","1.6 m","3.8 m","5.0 m","6.2 m","8.0 m","9.0 m"),
                     values=c('#F94144','#F3722C','#F8961E','#F9C74F','#90BE6D','#43AA8B','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab(expression(paste("Dissolved CH"[4]*" (", mu,"mol L"^-1*")")))+
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="black")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-30"),color="black")+ # SSS Off in FCR
  theme_classic(base_size=15)+
  theme(legend.position='none')


bvr_16_ch4 <- ggplot(bvr_16,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(bvr_16,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping,ymin=ch4_umolL_mean-ch4_umolL_sd,
                                   ymax=ch4_umolL_mean+ch4_umolL_sd),size=1)+
  scale_color_manual(breaks=c("BVR_1","BVR_2","BVR_3","BVR_4"), 
                     labels=c("0.1 m","3.0 m","6.0 m","9.0 m"),
                     values=c('#F94144','#F9C74F','#90BE6D','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab("")+
  ggtitle("BVR")+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="black")+ #Turnover
  theme_classic(base_size=15)+
  theme(legend.position=c(0.15,0.85))

bvr_17_ch4 <- ggplot(bvr_17,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(bvr_17,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping,ymin=ch4_umolL_mean-ch4_umolL_sd,
                                   ymax=ch4_umolL_mean+ch4_umolL_sd),size=1)+
  scale_color_manual(breaks=c("BVR_1","BVR_2","BVR_3","BVR_4"), 
                     labels=c("0.1 m","3.0 m","6.0 m","9.0 m"),
                     values=c('#F94144','#F9C74F','#90BE6D','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab("")+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="black")+ #Turnover
  theme_classic(base_size=15)+
  theme(legend.position='none')

bvr_18_ch4 <- ggplot(bvr_18,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping))+
  geom_line(size=1.1)+
  geom_point(size=4)+
  geom_errorbar(bvr_18,mapping=aes(x=DateTime,y=ch4_umolL_mean,color=grouping,ymin=ch4_umolL_mean-ch4_umolL_sd,
                                   ymax=ch4_umolL_mean+ch4_umolL_sd),size=1)+
  scale_color_manual(breaks=c("BVR_1","BVR_2","BVR_3","BVR_4"), 
                     labels=c("0.1 m","3.0 m","6.0 m","9.0 m"),
                     values=c('#F94144','#F9C74F','#90BE6D','#577590'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab("")+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="black")+ #Turnover
  theme_classic(base_size=15)+
  theme(legend.position='none')

ggarrange(fcr_16_ch4,bvr_16_ch4,fcr_17_ch4,bvr_17_ch4,fcr_18_ch4,bvr_18_ch4,common.legend=FALSE,ncol=2,nrow=3)
# Saved as 900 x 1000
