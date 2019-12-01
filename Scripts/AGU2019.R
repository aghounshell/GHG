### Script to plot Hypo GHGs for AGU poster
### Calculate Hypo GWP and plot
### A Hounshll, 25 Nov 2019

# Save Rfile as: AGU_Graphs

# Load in libraries
pacman::p_load(tidyverse,ggplot2,ggpubr)

# Load data: created from FCR_Depths and BVR_Depths for 2016 to 2018
fcr_ch4 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_ch4_stats_16to18.csv')
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

fcr_co2 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_co2_stats_16to18.csv')
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
bvr_ch4 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_ch4_stats_16to18.csv')
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

bvr_co2 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_co2_stats_16to18.csv')
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
# Remove rows with NA averages (but not SD!)
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

fcr_18 <- completeFun(fcr_18,"hypo_co2_avg")

# Plot Hypo CO2 and CH4
# Plot BVR and FCR on same graph but separate by date
# pCO2
co2_16 <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_16,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_16,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='FCR'),size=1)+
  geom_errorbar(data=bvr_16,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='BVR'),size=1)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("Anoxic","Oxic"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("VW Hypo pCO"[2]*" (", mu,"mol L"^-1*")")))+
  ylim(0,1000)+
  theme_classic(base_size=15)

co2_17 <- ggplot()+
  geom_line(data=fcr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_17,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='FCR'),size=1)+
  geom_errorbar(data=bvr_17,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='BVR'),size=1)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("Anoxic","Oxic"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab('')+
  ylim(0,1000)+
  theme_classic(base_size=15)

co2_18 <- ggplot()+
  geom_line(data=fcr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=datetime,y=hypo_co2_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_18,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='FCR'),size=1)+
  geom_errorbar(data=bvr_18,mapping=aes(x=datetime,y=hypo_co2_avg,ymin=hypo_co2_avg-hypo_co2_std,
                                        ymax=hypo_co2_avg+hypo_co2_std,group=1,color='BVR'),size=1)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("Anoxic","Oxic"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab('')+
  ylim(0,1000)+
  theme_classic(base_size=15)

ggarrange(co2_16,co2_17,co2_18,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# pCH4

ch4_16 <- ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='FCR'),size=1)+
  geom_errorbar(data=bvr_16,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='BVR'),size=1)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("Anoxic","Oxic"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylim(0,600)+
  ylab(expression(paste("VW Hypo pCH"[4]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size=15)

ch4_17 <- ggplot()+
  geom_line(data=fcr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='FCR'),size=1)+
  geom_errorbar(data=bvr_17,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='BVR'),size=1)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("Anoxic","Oxic"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab('')+
  ylim(0,600)+
  theme_classic(base_size=15)

ch4_18 <- ggplot()+
  geom_line(data=fcr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=1.1)+
  geom_line(data=bvr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=1.1)+
  geom_point(data=fcr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='FCR'),size=4)+
  geom_point(data=bvr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,group=1,color='BVR'),size=4)+
  geom_errorbar(data=fcr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='FCR'),size=1)+
  geom_errorbar(data=bvr_18,mapping=aes(x=datetime,y=hypo_ch4_avg,ymin=hypo_ch4_avg-hypo_ch4_std,
                                        ymax=hypo_ch4_avg+hypo_ch4_std,group=1,color='BVR'),size=1)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR","FCR"), labels=c("Anoxic","Oxic"),
                     values=c('#F5793A','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab('')+
  ylim(0,600)+
  theme_classic(base_size=15)

ggarrange(ch4_16,ch4_17,ch4_18,common.legend=TRUE,legend="right",ncol=3,nrow=1)
