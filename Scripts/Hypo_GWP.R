### Script to calculate GWP of Hypo for FCR and BVR
### Starting from cleaned up reps for FCR and BVR
### A Hounshell, 25 Nov 2019

# Load packages
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo)

# Load in data
# FCR Volumes by depth
fcr_depth <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Vol.csv")
# FCR GHG data from 2016-2017: includes reps
fcr_ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_16_18_reps.csv")
fcr_ghg$datetime <- as.POSIXct(strptime(fcr_ghg$datetime, "%Y-%m-%d", tz="EST"))

# BVR Volumes by depth
bvr_depth <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/BVR_Vol.csv")
# BVR GHG data from 2016-2017
bvr_ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_BVR_16_18_reps.csv")
bvr_ghg$datetime <- as.POSIXct(strptime(bvr_ghg$datetime, "%Y-%m-%d", tz="EST"))

### Separate Hypo depths for FCR, then recombine
fcr_ghg_6_rep1 <- fcr_ghg %>% filter(depth==8.0,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_6")
fcr_ghg_6_rep1$ch4_umol_L <- na.approx(fcr_ghg_6_rep1$ch4_umol_L)
fcr_ghg_6_rep1$co2_umol_L <- na.approx(fcr_ghg_6_rep1$co2_umol_L)

fcr_ghg_6_rep2 <- fcr_ghg %>% filter(depth==8.0,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_6")
fcr_ghg_6_rep2$ch4_umol_L <- na.approx(fcr_ghg_6_rep2$ch4_umol_L)
fcr_ghg_6_rep2$co2_umol_L <- na.approx(fcr_ghg_6_rep2$co2_umol_L)

fcr_ghg_7_rep1 <- fcr_ghg %>% filter(depth==9.0,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_7")
fcr_ghg_7_rep1$ch4_umol_L <- na.approx(fcr_ghg_7_rep1$ch4_umol_L)
fcr_ghg_7_rep1$co2_umol_L <- na.approx(fcr_ghg_7_rep1$co2_umol_L)

fcr_ghg_7_rep2 <- fcr_ghg %>% filter(depth==9.0,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_7")
fcr_ghg_7_rep2$ch4_umol_L <- na.approx(fcr_ghg_7_rep2$ch4_umol_L)
fcr_ghg_7_rep2$co2_umol_L <- na.approx(fcr_ghg_7_rep2$co2_umol_L)

# Recombine into reps1 and separate by CO2 and CH4
fcr_ghg_merge_rep1 <- rbind(fcr_ghg_6_rep1,fcr_ghg_7_rep1)
fcr_ghg_merge_rep2 <- rbind(fcr_ghg_6_rep2,fcr_ghg_7_rep2)

fcr_rep1_co2 <- fcr_ghg_merge_rep1 %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
fcr_rep1_co2$co2_umol_L[205] <- NA
fcr_rep1_co2_layers <- fcr_rep1_co2 %>% spread(grouping,co2_umol_L)

fcr_rep2_co2 <- fcr_ghg_merge_rep2 %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
fcr_rep2_co2_layers <- fcr_rep2_co2 %>% spread(grouping,co2_umol_L)

fcr_rep1_ch4 <- fcr_ghg_merge_rep1 %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
fcr_rep1_ch4_layers <- fcr_rep1_ch4 %>% spread(grouping,ch4_umol_L)

fcr_rep2_ch4 <- fcr_ghg_merge_rep2 %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
fcr_rep2_ch4_layers <- fcr_rep2_ch4 %>% spread(grouping,ch4_umol_L)

### Calculate Hypo VW Temp: defined as 6.5-9m
# Re-combine depths as well
fcr_depth_2 <- rep(-99,7)
fcr_depth_2[1] <- sum(fcr_depth$`Volume (L)`[1:3])
fcr_depth_2[2] <- sum(fcr_depth$`Volume (L)`[4:8])
fcr_depth_2[3] <- sum(fcr_depth$`Volume (L)`[9:13])
fcr_depth_2[4] <- sum(fcr_depth$`Volume (L)`[14:18])
fcr_depth_2[5] <- sum(fcr_depth$`Volume (L)`[19:21])
fcr_depth_2[6] <- sum(fcr_depth$`Volume (L)`[22:27])
fcr_depth_2[7] <- sum(fcr_depth$`Volume (L)`[28:31])

### Calculate Rep 1 and Rep 2; then combine
fcr_hypo_ch4_rep1 <- rep(-99,length(fcr_rep1_ch4_layers$datetime))
for(i in 1:length(fcr_rep1_ch4_layers$datetime)){
  fcr_hypo_ch4_rep1[i] <- sum(fcr_rep1_ch4_layers$FCR_6[i]*fcr_depth_2[6],
                          fcr_rep1_ch4_layers$FCR_7[i]*fcr_depth_2[7])/sum(fcr_depth_2[c(6:7)])
}

# Hypo, rep 2, CH4
fcr_hypo_ch4_rep2 <- rep(-99,length(fcr_rep2_ch4_layers$datetime))
for(i in 1:length(fcr_rep2_ch4_layers$datetime)){
  fcr_hypo_ch4_rep2[i] <- sum(fcr_rep2_ch4_layers$FCR_6[i]*fcr_depth_2[6],
                          fcr_rep2_ch4_layers$FCR_7[i]*fcr_depth_2[7])/sum(fcr_depth_2[c(6:7)])
}

fcr_hypo_ch4 <- cbind.data.frame(fcr_rep1_ch4_layers$datetime,fcr_hypo_ch4_rep1,fcr_hypo_ch4_rep2)
names(fcr_hypo_ch4)[1] <- "datetime"
names(fcr_hypo_ch4)[2] <- "hypo_rep1"
names(fcr_hypo_ch4)[3] <- "hypo_rep2"

### Calculate for CO2
fcr_hypo_co2_rep1 <- rep(-99,length(fcr_rep1_co2_layers$datetime))
for(i in 1:length(fcr_rep1_co2_layers$datetime)){
  fcr_hypo_co2_rep1[i] <- sum(fcr_rep1_co2_layers$FCR_6[i]*fcr_depth_2[6],
                              fcr_rep1_co2_layers$FCR_7[i]*fcr_depth_2[7])/sum(fcr_depth_2[c(6:7)])
}

# Hypo, rep 2, CH4
fcr_hypo_co2_rep2 <- rep(-99,length(fcr_rep2_co2_layers$datetime))
for(i in 1:length(fcr_rep2_co2_layers$datetime)){
  fcr_hypo_co2_rep2[i] <- sum(fcr_rep2_co2_layers$FCR_6[i]*fcr_depth_2[6],
                              fcr_rep2_co2_layers$FCR_7[i]*fcr_depth_2[7])/sum(fcr_depth_2[c(6:7)])
}

fcr_hypo_co2 <- cbind.data.frame(fcr_rep1_co2_layers$datetime,fcr_hypo_co2_rep1,fcr_hypo_co2_rep2)
names(fcr_hypo_co2)[1] <- "datetime"
names(fcr_hypo_co2)[2] <- "hypo_rep1"
names(fcr_hypo_co2)[3] <- "hypo_rep2"

### Calculate for BVR CH4
# Separate depths first
bvr_ghg_4_rep1 <- bvr_ghg %>% filter(depth==9,rep==1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_4")
bvr_ghg_4_rep1$ch4_umol_L <- na.approx(bvr_ghg_4_rep1$ch4_umol_L,na.rm=FALSE)
bvr_ghg_4_rep1$co2_umol_L <- na.approx(bvr_ghg_4_rep1$co2_umol_L,na.rm=FALSE)

bvr_ghg_4_rep2 <- bvr_ghg %>% filter(depth==9,rep==2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="BVR_4")
bvr_ghg_4_rep2$ch4_umol_L <- na.approx(bvr_ghg_4_rep2$ch4_umol_L,na.rm=FALSE)
bvr_ghg_4_rep2$co2_umol_L <- na.approx(bvr_ghg_4_rep2$co2_umol_L,na.rm=FALSE)

bvr_hypo_ch4 <- cbind.data.frame(bvr_ghg_4_rep1$datetime,bvr_ghg_4_rep1$ch4_umol_L,bvr_ghg_4_rep2$ch4_umol_L)
names(bvr_hypo_ch4)[1] <- "datetime"
names(bvr_hypo_ch4)[2] <- "hypo_rep1"
names(bvr_hypo_ch4)[3] <- "hypo_rep2"

bvr_hypo_co2 <- cbind.data.frame(bvr_ghg_4_rep1$datetime,bvr_ghg_4_rep1$co2_umol_L,bvr_ghg_4_rep2$co2_umol_L)
names(bvr_hypo_co2)[1] <- "datetime"
names(bvr_hypo_co2)[2] <- "hypo_rep1"
names(bvr_hypo_co2)[3] <- "hypo_rep2"

### Try to create data frame of dates from 01-01-16 to 12-31-18
dates_full <- data.frame(x = seq(as.Date("2016-01-01"), as.Date("2018-12-31"), by = "day"))
names(dates_full)[1] <-  "datetime"

fcr_ch4_merge <- merge(dates_full,fcr_hypo_ch4,by="datetime",all.x=TRUE,all.y=TRUE)
fcr_ch4_merge$hypo_rep1 <- na.approx(fcr_ch4_merge$hypo_rep1,na.rm=FALSE)
fcr_ch4_merge$hypo_rep2 <- na.approx(fcr_ch4_merge$hypo_rep2,na.rm=FALSE)
fcr_co2_merge <- merge(dates_full,fcr_hypo_co2,by="datetime",all.x=TRUE,all.y=TRUE)
fcr_co2_merge$hypo_rep1 <- na.approx(fcr_co2_merge$hypo_rep1,na.rm=FALSE)
fcr_co2_merge$hypo_rep2 <- na.approx(fcr_co2_merge$hypo_rep2,na.rm=FALSE)

bvr_ch4_merge <- merge(dates_full,bvr_hypo_ch4,by="datetime",all.x=TRUE,all.y=TRUE)
bvr_ch4_merge$hypo_rep1 <- na.approx(bvr_ch4_merge$hypo_rep1,na.rm=FALSE)
bvr_ch4_merge$hypo_rep2 <- na.approx(bvr_ch4_merge$hypo_rep2,na.rm=FALSE)
bvr_co2_merge <- merge(dates_full,bvr_hypo_co2,by="datetime",all.x=TRUE,all.y=TRUE)
bvr_co2_merge$hypo_rep1 <- na.approx(bvr_co2_merge$hypo_rep1,na.rm=FALSE)
bvr_co2_merge$hypo_rep2 <- na.approx(bvr_co2_merge$hypo_rep2,na.rm=FALSE)

## Check the data?
ggplot()+
  geom_line(fcr_co2_merge,mapping=aes(x=datetime,y=hypo_rep2),color="blue")+
  geom_line(fcr_co2_merge,mapping=aes(x=datetime,y=hypo_rep1))+
  theme_classic()

ggplot()+
  geom_line(fcr_ch4_merge,mapping=aes(x=datetime,y=hypo_rep2),color="blue")+
  geom_line(fcr_ch4_merge,mapping=aes(x=datetime,y=hypo_rep1))+
  theme_classic()

ggplot()+
  geom_line(bvr_co2_merge,mapping=aes(x=datetime,y=hypo_rep2),color="blue")+
  geom_line(bvr_co2_merge,mapping=aes(x=datetime,y=hypo_rep1))+
  theme_classic()  

ggplot()+
  geom_line(bvr_ch4_merge,mapping=aes(x=datetime,y=hypo_rep2),color="blue")+
  geom_line(bvr_ch4_merge,mapping=aes(x=datetime,y=hypo_rep1))+
  theme_classic()

## Calculate Hypo GHGs: sum from 04-06 to 10-20 for each year
bvr_ch4_16 <- bvr_ch4_merge %>% filter(datetime>=as.Date('2016-04-06')&datetime<=as.Date('2016-10-20'))
bvr_ch4_17 <- bvr_ch4_merge %>% filter(datetime>=as.Date('2017-04-06')&datetime<=as.Date('2017-10-20'))
bvr_ch4_18 <- bvr_ch4_merge %>% filter(datetime>=as.Date('2018-04-06')&datetime<=as.Date('2018-10-20'))

bvr_co2_16 <- bvr_co2_merge %>% filter(datetime>=as.Date('2016-04-06')&datetime<=as.Date('2016-10-20'))
bvr_co2_17 <- bvr_co2_merge %>% filter(datetime>=as.Date('2017-04-06')&datetime<=as.Date('2017-10-20'))
bvr_co2_18 <- bvr_co2_merge %>% filter(datetime>=as.Date('2018-04-06')&datetime<=as.Date('2018-10-20'))

fcr_ch4_16 <- fcr_ch4_merge %>% filter(datetime>=as.Date('2016-04-06')&datetime<=as.Date('2016-10-20'))
fcr_ch4_17 <- fcr_ch4_merge %>% filter(datetime>=as.Date('2017-04-06')&datetime<=as.Date('2017-10-20'))
fcr_ch4_18 <- fcr_ch4_merge %>% filter(datetime>=as.Date('2018-04-06')&datetime<=as.Date('2018-10-20'))

fcr_co2_16 <- fcr_co2_merge %>% filter(datetime>=as.Date('2016-04-06')&datetime<=as.Date('2016-10-20'))
fcr_co2_17 <- fcr_co2_merge %>% filter(datetime>=as.Date('2017-04-06')&datetime<=as.Date('2017-10-20'))
fcr_co2_18 <- fcr_co2_merge %>% filter(datetime>=as.Date('2018-04-06')&datetime<=as.Date('2018-10-20'))

### Sum and calculate Avg and Stdev
bvr_ch4_16_sum <- matrix(ncol=1,nrow=2)
bvr_ch4_16_sum[1,1] <- sum(bvr_ch4_16$hypo_rep1,na.rm=TRUE)
bvr_ch4_16_sum[2,1] <- sum(bvr_ch4_16$hypo_rep2,na.rm=TRUE)

bvr_ch4_17_sum <- matrix(ncol=1,nrow=2)
bvr_ch4_17_sum[1,1] <- sum(bvr_ch4_17$hypo_rep1,na.rm=TRUE)
bvr_ch4_17_sum[2,1] <- sum(bvr_ch4_17$hypo_rep2,na.rm=TRUE)

bvr_ch4_18_sum <- matrix(ncol=1,nrow=2)
bvr_ch4_18_sum[1,1] <- sum(bvr_ch4_18$hypo_rep1,na.rm=TRUE)
bvr_ch4_18_sum[2,1] <- sum(bvr_ch4_18$hypo_rep2,na.rm=TRUE)

bvr_co2_16_sum <- matrix(ncol=1,nrow=2)
bvr_co2_16_sum[1,1] <- sum(bvr_co2_16$hypo_rep1,na.rm=TRUE)
bvr_co2_16_sum[2,1] <- sum(bvr_co2_16$hypo_rep2,na.rm=TRUE)

bvr_co2_17_sum <- matrix(ncol=1,nrow=2)
bvr_co2_17_sum[1,1] <- sum(bvr_co2_17$hypo_rep1,na.rm=TRUE)
bvr_co2_17_sum[2,1] <- sum(bvr_co2_17$hypo_rep2,na.rm=TRUE)

bvr_co2_18_sum <- matrix(ncol=1,nrow=2)
bvr_co2_18_sum[1,1] <- sum(bvr_co2_18$hypo_rep1,na.rm=TRUE)
bvr_co2_18_sum[2,1] <- sum(bvr_co2_18$hypo_rep2,na.rm=TRUE)


fcr_ch4_16_sum <- matrix(ncol=1,nrow=2)
fcr_ch4_16_sum[1,1] <- sum(fcr_ch4_16$hypo_rep1,na.rm=TRUE)
fcr_ch4_16_sum[2,1] <- sum(fcr_ch4_16$hypo_rep2,na.rm=TRUE)

fcr_ch4_17_sum <- matrix(ncol=1,nrow=2)
fcr_ch4_17_sum[1,1] <- sum(fcr_ch4_17$hypo_rep1,na.rm=TRUE)
fcr_ch4_17_sum[2,1] <- sum(fcr_ch4_17$hypo_rep2,na.rm=TRUE)

fcr_ch4_18_sum <- matrix(ncol=1,nrow=2)
fcr_ch4_18_sum[1,1] <- sum(fcr_ch4_18$hypo_rep1,na.rm=TRUE)
fcr_ch4_18_sum[2,1] <- sum(fcr_ch4_18$hypo_rep2,na.rm=TRUE)

fcr_co2_16_sum <- matrix(ncol=1,nrow=2)
fcr_co2_16_sum[1,1] <- sum(fcr_co2_16$hypo_rep1,na.rm=TRUE)
fcr_co2_16_sum[2,1] <- sum(fcr_co2_16$hypo_rep2,na.rm=TRUE)

fcr_co2_17_sum <- matrix(ncol=1,nrow=2)
fcr_co2_17_sum[1,1] <- sum(fcr_co2_17$hypo_rep1,na.rm=TRUE)
fcr_co2_17_sum[2,1] <- sum(fcr_co2_17$hypo_rep2,na.rm=TRUE)

fcr_co2_18_sum <- matrix(ncol=1,nrow=2)
fcr_co2_18_sum[1,1] <- sum(fcr_co2_18$hypo_rep1,na.rm=TRUE)
fcr_co2_18_sum[2,1] <- sum(fcr_co2_18$hypo_rep2,na.rm=TRUE)

data <- matrix(ncol=4,nrow=6)

row.names(data) <- c("BVR 16", "BVR 17", "BVR 18", "FCR 16", "FCR 17", "FCR 18")
colnames(data) <- c("CH4 Avg", "CH4 Stdev", "CO2 Avg", "CO2 Stdev")

data[1,1] <- mean(bvr_ch4_16_sum)
data[1,2] <- sd(bvr_ch4_16_sum)
data[1,3] <- mean(bvr_co2_16_sum)
data[1,4] <- sd(bvr_co2_16_sum)

data[2,1] <- mean(bvr_ch4_17_sum)
data[2,2] <- sd(bvr_ch4_17_sum)
data[2,3] <- mean(bvr_co2_17_sum)
data[2,4] <- sd(bvr_co2_17_sum)

data[3,1] <- mean(bvr_ch4_18_sum)
data[3,2] <- sd(bvr_ch4_18_sum)
data[3,3] <- mean(bvr_co2_18_sum)
data[3,4] <- sd(bvr_co2_18_sum)

data[4,1] <- mean(fcr_ch4_16_sum)
data[4,2] <- sd(fcr_ch4_16_sum)
data[4,3] <- mean(fcr_co2_16_sum)
data[4,4] <- sd(fcr_co2_16_sum)

data[5,1] <- mean(fcr_ch4_17_sum)
data[5,2] <- sd(fcr_ch4_17_sum)
data[5,3] <- mean(fcr_co2_17_sum)
data[5,4] <- sd(fcr_co2_17_sum)

data[6,1] <- mean(fcr_ch4_18_sum)
data[6,2] <- sd(fcr_ch4_18_sum)
data[6,3] <- mean(fcr_co2_18_sum)
data[6,4] <- sd(fcr_co2_18_sum)

res <- c("BVR","BVR","BVR","BVR","BVR","BVR","FCR","FCR","FCR","FCR","FCR","FCR")
year <- c("2016","2016","2017","2017","2018","2018","2016","2016","2017","2017","2018","2018")
gas <- c("ch4","co2","ch4","co2","ch4","co2","ch4","co2","ch4","co2","ch4","co2")
avg <- c(mean(bvr_ch4_16_sum),mean(bvr_co2_16_sum),mean(bvr_ch4_17_sum),mean(bvr_co2_17_sum),
         mean(bvr_ch4_18_sum),mean(bvr_co2_18_sum),mean(fcr_ch4_16_sum),mean(fcr_co2_16_sum),
         mean(fcr_ch4_17_sum),mean(fcr_co2_17_sum),mean(fcr_ch4_18_sum),mean(fcr_co2_18_sum))
sd <- c(sd(bvr_ch4_16_sum),sd(bvr_co2_16_sum),sd(bvr_ch4_17_sum),sd(bvr_co2_17_sum),
        sd(bvr_ch4_18_sum),sd(bvr_co2_18_sum),sd(fcr_ch4_16_sum),sd(fcr_co2_16_sum),
        sd(fcr_ch4_17_sum),sd(fcr_co2_17_sum),sd(fcr_ch4_18_sum),sd(fcr_co2_18_sum))

data_2 <- cbind.data.frame(res,year,gas,avg,sd)

ggplot(data_2,mapping=aes(x=year,y=avg,fill=gas))+
  geom_bar(stat="identity", color="black",position=position_dodge())

# Units in kg/L
bvr_gwp_16 <- matrix(ncol=1,nrow=2)
bvr_gwp_16[1,1] <- ((bvr_ch4_16_sum[1,1]*34*16.04)+(bvr_co2_16_sum[1,1]*44.01))/1000000
bvr_gwp_16[2,1] <- ((bvr_ch4_16_sum[2,1]*34*16.04)+(bvr_co2_16_sum[2,1]*44.01))/1000000

bvr_gwp_17 <- matrix(ncol=1,nrow=2)
bvr_gwp_17[1,1] <- ((bvr_ch4_17_sum[1,1]*34*16.04)+(bvr_co2_17_sum[1,1])*44.01)/1000000
bvr_gwp_17[2,1] <- ((bvr_ch4_17_sum[2,1]*34*16.04)+(bvr_co2_17_sum[2,1])*44.01)/1000000

bvr_gwp_18 <- matrix(ncol=1,nrow=2)
bvr_gwp_18[1,1] <- ((bvr_ch4_18_sum[1,1]*34*16.04)+(bvr_co2_18_sum[1,1])*44.01)/1000000
bvr_gwp_18[2,1] <- ((bvr_ch4_18_sum[2,1]*34*16.04)+(bvr_co2_18_sum[2,1])*44.01)/1000000

fcr_gwp_16 <- matrix(ncol=1,nrow=2)
fcr_gwp_16[1,1] <- ((fcr_ch4_16_sum[1,1]*34*16.04)+(fcr_co2_16_sum[1,1]*44.01))/1000000
fcr_gwp_16[2,1] <- ((fcr_ch4_16_sum[2,1]*34*16.04)+(fcr_co2_16_sum[2,1]*44.01))/1000000

fcr_gwp_17 <- matrix(ncol=1,nrow=2)
fcr_gwp_17[1,1] <- ((fcr_ch4_17_sum[1,1]*34*16.04)+(fcr_co2_17_sum[1,1]*44.01))/1000000
fcr_gwp_17[2,1] <- ((fcr_ch4_17_sum[2,1]*34*16.04)+(fcr_co2_17_sum[2,1]*44.01))/1000000

fcr_gwp_18 <- matrix(ncol=1,nrow=2)
fcr_gwp_18[1,1] <- ((fcr_ch4_18_sum[1,1]*34*16.04)+(fcr_co2_18_sum[1,1]*44.01))/1000000
fcr_gwp_18[2,1] <- ((fcr_ch4_18_sum[2,1]*34*16.04)+(fcr_co2_18_sum[2,1]*44.01))/1000000

res <- c("BVR","BVR","BVR","FCR","FCR","FCR")
year <- c("2016","2017","2018","2016","2017","2018")
avg <- c(mean(bvr_gwp_16),mean(bvr_gwp_17),mean(bvr_gwp_18),mean(fcr_gwp_16),mean(fcr_gwp_17),
         mean(fcr_gwp_18))
sd <- c(sd(bvr_gwp_16),sd(bvr_gwp_17),sd(bvr_gwp_18),sd(fcr_gwp_16),sd(fcr_gwp_17),
        sd(fcr_gwp_18))

gwp <- cbind.data.frame(res,year,avg,sd)

ggplot(gwp,mapping=aes(x=year,y=avg,fill=res))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=avg-sd,ymax=avg+sd), width=.2,position=position_dodge(.9))+
  scale_fill_manual(breaks=c("BVR","FCR"),
                     values=c('#F5793A','#0F2080'))+
  labs(fill="")+
  xlab("")+
  ylab(expression(paste("Hypo GWP (g L"^-1*")")))+
  theme_classic(base_size=15)
