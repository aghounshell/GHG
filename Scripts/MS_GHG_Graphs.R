### Script to plot Hypo GHGs for AGU poster
### Calculate Hypo GWP and plot
### A Hounshll, 25 Nov 2019
### Graphs updated with 2018 Acoustic data and 'final' GHG database from RPM

# Save Rfile as: MS_GHG_Graphs

# Load in libraries
pacman::p_load(tidyverse,ggplot2,ggpubr)

# Load data: created from FCR_Depths and BVR_Depths for 2016 to 2018
fcr_ch4 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/FCR_VW_ch4_stats_16to18_Revs.csv')
fcr_ch4$datetime <- as.POSIXct(strptime(fcr_ch4$datetime, "%Y-%m-%d", tz = "EST"))
fcr_ch4_16 <- fcr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(fcr_ch4_16)[2] <- "hypo_ch4_avg"
names(fcr_ch4_16)[3] <- "hypo_ch4_std"
fcr_ch4_17 <- fcr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(fcr_ch4_17)[2] <- "hypo_ch4_avg"
names(fcr_ch4_17)[3] <- "hypo_ch4_std"
fcr_ch4_18 <- fcr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))
names(fcr_ch4_18)[2] <- "hypo_ch4_avg"
names(fcr_ch4_18)[3] <- "hypo_ch4_std"

fcr_co2 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/FCR_VW_co2_stats_16to18_Revs.csv')
fcr_co2$datetime <- as.POSIXct(strptime(fcr_co2$datetime, "%Y-%m-%d", tz = "EST"))
fcr_co2_16 <- fcr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(fcr_co2_16)[2] <- "hypo_co2_avg"
names(fcr_co2_16)[3] <- "hypo_co2_std"
fcr_co2_17 <- fcr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(fcr_co2_17)[2] <- "hypo_co2_avg"
names(fcr_co2_17)[3] <- "hypo_co2_std"
fcr_co2_18 <- fcr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))
names(fcr_co2_18)[2] <- "hypo_co2_avg"
names(fcr_co2_18)[3] <- "hypo_co2_std"

# Combine 2016, 2017, and 2018 data for FCR
fcr_16 <- cbind.data.frame(fcr_co2_16,fcr_ch4_16)
fcr_16 <- fcr_16[,-c(4)]
fcr_17 <- cbind.data.frame(fcr_co2_17,fcr_ch4_17)
fcr_17 <- fcr_17[,-c(4)]
fcr_18 <- cbind.data.frame(fcr_co2_18,fcr_ch4_18)
fcr_18 <- fcr_18[,-c(4)]

## Do the same for BVR
bvr_ch4 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/BVR_VW_ch4_stats_16to18_Revs.csv')
bvr_ch4$datetime <- as.POSIXct(strptime(bvr_ch4$datetime, "%Y-%m-%d", tz = "EST"))
bvr_ch4_16 <- bvr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(bvr_ch4_16)[2] <- "hypo_ch4_avg"
names(bvr_ch4_16)[3] <- "hypo_ch4_std"
bvr_ch4_17 <- bvr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(bvr_ch4_17)[2] <- "hypo_ch4_avg"
names(bvr_ch4_17)[3] <- "hypo_ch4_std"
bvr_ch4_18 <- bvr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))
names(bvr_ch4_18)[2] <- "hypo_ch4_avg"
names(bvr_ch4_18)[3] <- "hypo_ch4_std"

bvr_co2 <- read.csv('C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/BVR_VW_co2_stats_16to18_Revs.csv')
bvr_co2$datetime <- as.POSIXct(strptime(bvr_co2$datetime, "%Y-%m-%d", tz = "EST"))
bvr_co2_16 <- bvr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(bvr_co2_16)[2] <- "hypo_co2_avg"
names(bvr_co2_16)[3] <- "hypo_co2_std"
bvr_co2_17 <- bvr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(bvr_co2_17)[2] <- "hypo_co2_avg"
names(bvr_co2_17)[3] <- "hypo_co2_std"
bvr_co2_18 <- bvr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))
names(bvr_co2_18)[2] <- "hypo_co2_avg"
names(bvr_co2_18)[3] <- "hypo_co2_std"

# Combine 2016 and 2017 data for FCR
bvr_16 <- cbind.data.frame(bvr_co2_16,bvr_ch4_16)
bvr_16 <- bvr_16[,-c(4)]
bvr_17 <- cbind.data.frame(bvr_co2_17,bvr_ch4_17)
bvr_17 <- bvr_17[,-c(4)]
bvr_18 <- cbind.data.frame(bvr_co2_18,bvr_ch4_18)
bvr_18 <- bvr_18[,-c(4)]

# Remove NA values from fcr_18
fcr_18 = na.omit(fcr_18)

# Plot Hypo CO2 and CH4
# Plot BVR and FCR on same graph but separate by date
# pCO2
co2_16 <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_16,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='FCR'),size=1,width=1)+
  geom_errorbar(data=bvr_16,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='BVR'),size=1,width=1)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("VW Hypo [CO"[2]*"] (", mu,"mol L"^-1*")")))+
  ylim(-25,1000)+
  theme_classic(base_size=15)+
  theme(legend.position=c(0.3,0.9))

co2_17 <- ggplot()+
  geom_line(data=fcr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_17,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='FCR'),size=1,width=1)+
  geom_errorbar(data=bvr_17,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='BVR'),size=1,width=1)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab('')+
  ylim(-25,1000)+
  theme_classic(base_size=15)+
  theme(legend.position='none')

co2_18 <- ggplot()+
  geom_line(data=fcr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_18,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='FCR'),size=1,width=1)+
  geom_errorbar(data=bvr_18,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='BVR'),size=1,width=1)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-30"),color="#0F2080")+ # SSS Off in FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  ylab("")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  ylab(expression(paste("VW Hypo [CO"[2]*"] (", mu,"mol L"^-1*")")))+
  xlab('2018')+
  ylim(-25,1000)+
  theme_classic(base_size=15)+
  theme(legend.position='none')

# Saved as: 1000, 300; 800 x 700 for MS
co2 <- ggarrange(co2_16,co2_17,co2_18,common.legend=FALSE,ncol=2,nrow=2)

ggsave("C:/Users/ahoun/OneDrive/Desktop/GHG/Fig_Output/Figure3_HiRes.jpg",co2,width=7,height=6,
       units=c("in"))

# pCH4

ch4_16 <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='FCR'),size=1,width=1)+
  geom_errorbar(data=bvr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='BVR'),size=1,width=1)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylim(0,600)+
  ylab(expression(paste("VW Hypo [CH"[4]*"] (", mu,"mol L"^-1*")")))+
  theme_classic(base_size=15)+
  theme(legend.position=c(0.3,0.9))

ch4_17 <- ggplot()+
  geom_line(data=fcr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='FCR'),size=1,width=1)+
  geom_errorbar(data=bvr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='BVR'),size=1,width=1)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab('')+
  ylim(0,600)+
  theme_classic(base_size=15)+
  theme(legend.position='none')

ch4_18 <- ggplot()+
  geom_line(data=fcr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='FCR'),size=1,width=1)+
  geom_errorbar(data=bvr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='BVR'),size=1,width=1)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-30"),color="#0F2080")+ # SSS Off in FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  ylab(expression(paste("VW Hypo [CH"[4]*"] (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylim(0,600)+
  theme_classic(base_size=15)+
  theme(legend.position='none')

ch4 <- ggarrange(ch4_16,ch4_17,ch4_18,common.legend=FALSE,ncol=2,nrow=2)

ggsave("C:/Users/ahoun/OneDrive/Desktop/GHG/Fig_Output/Figure4_HiRes.jpg",ch4,width=7,height=6,
       units=c("in"))
