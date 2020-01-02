### Script to plot Metals data from Hypo of FCR and BVR: 2016, 2017, and 2018
### Using metals EDI data: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.455.4
### Calculated VW Hypo concentrations following: FCR_Depths and BVR_Depths
### A. Hounshell, 02 Jan 2020
## Saved RFile as: VW_Metals

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo)

# Load in data
# FCR Volumes by depth
depth_fcr <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Vol.csv")

# Re-combine depths to reflect 'layers' in FCR
depth_fcr_2 <- rep(-99,7)
depth_fcr_2[1] <- sum(depth_fcr$`Volume (L)`[1:3])
depth_fcr_2[2] <- sum(depth_fcr$`Volume (L)`[4:8])
depth_fcr_2[3] <- sum(depth_fcr$`Volume (L)`[9:13])
depth_fcr_2[4] <- sum(depth_fcr$`Volume (L)`[14:18])
depth_fcr_2[5] <- sum(depth_fcr$`Volume (L)`[19:21])
depth_fcr_2[6] <- sum(depth_fcr$`Volume (L)`[22:27])
depth_fcr_2[7] <- sum(depth_fcr$`Volume (L)`[28:31])

# BVR Volumes by depth
depth_bvr <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/BVR_Vol.csv")

# Re-combine depths to reflect 'layers' in BVR
# Add volume from the bottom-most layers (10-13)
depth_bvr_2 <- depth_bvr[c(1:10),]
depth_bvr_2[10,2] <- sum(depth_bvr[c(10:13),2])

# Metals data (FCR + BVR from EDI)
metals <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/Metals_EDI.csv")
metals$DateTime <- as.POSIXct(strptime(metals$DateTime, "%m/%d/%Y", tz = "EST"))

# Separate metals data by FCR and BVR for years 2016, 2017, and 2018
metals_fcr <- metals %>% filter(Reservoir == "FCR" & Site == "50") %>% 
  filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2018-12-31')) %>% 
  select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)

metals_bvr <- metals %>% filter(Reservoir == "BVR" & Site == "50") %>% 
  filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2018-12-31')) %>% 
  select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)

# Plot to look at data (quick and dirty!)
# FCR TFe
ggplot(metals_fcr,aes(x=DateTime,y=TFe_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# FCR TMn
ggplot(metals_fcr,aes(x=DateTime,y=TMn_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# FCR SFe
ggplot(metals_fcr,aes(x=DateTime,y=SFe_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# FCR SMn
ggplot(metals_fcr,aes(x=DateTime,y=SMn_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# BVR TFe
ggplot(metals_bvr,aes(x=DateTime,y=TFe_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# BVR TMn
ggplot(metals_bvr,aes(x=DateTime,y=TMn_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# BVR SFe
ggplot(metals_bvr,aes(x=DateTime,y=SFe_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

# BVR SMn
ggplot(metals_bvr,aes(x=DateTime,y=SMn_mgL,group=Depth_m,color=Depth_m))+
  geom_line()+
  theme_classic()

########################### FCR ################################
## First, divide data into 'layers' 
fcr_1 <- metals_fcr %>% filter(Depth_m==0.1) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_1")
fcr_1$TFe_mgL <- na.approx(fcr_1$TFe_mgL)
fcr_1$TMn_mgL <- na.approx(fcr_1$TMn_mgL)
fcr_1$SFe_mgL <- na.approx(fcr_1$SFe_mgL)
fcr_1$SMn_mgL <- na.approx(fcr_1$SMn_mgL)

fcr_2 <- metals_fcr %>% filter(Depth_m==1.6) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_2")
fcr_2$TFe_mgL <- na.approx(fcr_2$TFe_mgL)
fcr_2$TMn_mgL <- na.approx(fcr_2$TMn_mgL)
fcr_2$SFe_mgL <- na.approx(fcr_2$SFe_mgL)
fcr_2$SMn_mgL <- na.approx(fcr_2$SMn_mgL)

fcr_3 <- metals_fcr %>% filter(Depth_m==3.8) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_3")
fcr_3$TFe_mgL <- na.approx(fcr_3$TFe_mgL)
fcr_3$TMn_mgL <- na.approx(fcr_3$TMn_mgL)
fcr_3$SFe_mgL <- na.approx(fcr_3$SFe_mgL)
fcr_3$SMn_mgL <- na.approx(fcr_3$SMn_mgL)

fcr_4 <- metals_fcr %>% filter(Depth_m==5.0) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_4")
fcr_4$TFe_mgL <- na.approx(fcr_4$TFe_mgL)
fcr_4$TMn_mgL <- na.approx(fcr_4$TMn_mgL)
fcr_4$SFe_mgL <- na.approx(fcr_4$SFe_mgL)
fcr_4$SMn_mgL <- na.approx(fcr_4$SMn_mgL)

fcr_5 <- metals_fcr %>% filter(Depth_m==6.2) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_5")
fcr_5$TFe_mgL <- na.approx(fcr_5$TFe_mgL)
fcr_5$TMn_mgL <- na.approx(fcr_5$TMn_mgL)
fcr_5$SFe_mgL <- na.approx(fcr_5$SFe_mgL)
fcr_5$SMn_mgL <- na.approx(fcr_5$SMn_mgL)

fcr_6 <- metals_fcr %>% filter(Depth_m==8.0) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_6")
fcr_6$TFe_mgL <- na.approx(fcr_6$TFe_mgL)
fcr_6$TMn_mgL <- na.approx(fcr_6$TMn_mgL)
fcr_6$SFe_mgL <- na.approx(fcr_6$SFe_mgL)
fcr_6$SMn_mgL <- na.approx(fcr_6$SMn_mgL)

fcr_7 <- metals_fcr %>% filter(Depth_m==9.0) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="FCR_7")
fcr_7$TFe_mgL <- na.approx(fcr_7$TFe_mgL)
fcr_7$TMn_mgL <- na.approx(fcr_7$TMn_mgL)
fcr_7$SFe_mgL <- na.approx(fcr_7$SFe_mgL)
fcr_7$SMn_mgL <- na.approx(fcr_7$SMn_mgL)

# Then recombine depths
fcr_merge <- rbind(fcr_1,fcr_2,fcr_3,fcr_4,fcr_5,fcr_6,fcr_7)

fcr_tfe <- fcr_merge %>% select(DateTime,TFe_mgL,grouping) %>% arrange(DateTime)
fcr_tfe_layers <- fcr_tfe %>% spread(grouping,TFe_mgL)
fcr_tfe_layers$FCR_1 <- na.approx(fcr_tfe_layers$FCR_1)
fcr_tfe_layers$FCR_2 <- na.approx(fcr_tfe_layers$FCR_2)
fcr_tfe_layers$FCR_3 <- na.approx(fcr_tfe_layers$FCR_3)
fcr_tfe_layers$FCR_4 <- na.approx(fcr_tfe_layers$FCR_4)
fcr_tfe_layers$FCR_5 <- na.approx(fcr_tfe_layers$FCR_5)
fcr_tfe_layers$FCR_6 <- na.approx(fcr_tfe_layers$FCR_6)
fcr_tfe_layers$FCR_7 <- na.approx(fcr_tfe_layers$FCR_7)

fcr_tmn <- fcr_merge %>% select(DateTime,TMn_mgL,grouping) %>% arrange(DateTime)
fcr_tmn_layers <- fcr_tmn %>% spread(grouping,TMn_mgL)
fcr_tmn_layers$FCR_1 <- na.approx(fcr_tmn_layers$FCR_1)
fcr_tmn_layers$FCR_2 <- na.approx(fcr_tmn_layers$FCR_2)
fcr_tmn_layers$FCR_3 <- na.approx(fcr_tmn_layers$FCR_3)
fcr_tmn_layers$FCR_4 <- na.approx(fcr_tmn_layers$FCR_4)
fcr_tmn_layers$FCR_5 <- na.approx(fcr_tmn_layers$FCR_5)
fcr_tmn_layers$FCR_6 <- na.approx(fcr_tmn_layers$FCR_6)
fcr_tmn_layers$FCR_7 <- na.approx(fcr_tmn_layers$FCR_7)

fcr_sfe <- fcr_merge %>% select(DateTime,SFe_mgL,grouping) %>% arrange(DateTime)
fcr_sfe_layers <- fcr_sfe %>% spread(grouping,SFe_mgL)
fcr_sfe_layers$FCR_1 <- na.approx(fcr_sfe_layers$FCR_1)
fcr_sfe_layers$FCR_2 <- na.approx(fcr_sfe_layers$FCR_2)
fcr_sfe_layers$FCR_3 <- na.approx(fcr_sfe_layers$FCR_3)
fcr_sfe_layers$FCR_4 <- na.approx(fcr_sfe_layers$FCR_4)
fcr_sfe_layers$FCR_5 <- na.approx(fcr_sfe_layers$FCR_5)
fcr_sfe_layers$FCR_6 <- na.approx(fcr_sfe_layers$FCR_6)
fcr_sfe_layers$FCR_7 <- na.approx(fcr_sfe_layers$FCR_7)

fcr_smn <- fcr_merge %>% select(DateTime,SMn_mgL,grouping) %>% arrange(DateTime)
fcr_smn_layers <- fcr_smn %>% spread(grouping,SMn_mgL)
fcr_smn_layers$FCR_1 <- na.approx(fcr_smn_layers$FCR_1)
fcr_smn_layers$FCR_2 <- na.approx(fcr_smn_layers$FCR_2)
fcr_smn_layers$FCR_3 <- na.approx(fcr_smn_layers$FCR_3)
fcr_smn_layers$FCR_4 <- na.approx(fcr_smn_layers$FCR_4)
fcr_smn_layers$FCR_5 <- na.approx(fcr_smn_layers$FCR_5)
fcr_smn_layers$FCR_6 <- na.approx(fcr_smn_layers$FCR_6)
fcr_smn_layers$FCR_7 <- na.approx(fcr_smn_layers$FCR_7)

## Calculate Hypo VW average for TMn, TFe, SMn, and SFe
hypo_fcr_tfe <- rep(-99,length(fcr_tfe_layers$DateTime))
for(i in 1:length(fcr_tfe_layers$DateTime)){
  hypo_fcr_tfe[i] <- sum(fcr_tfe_layers$FCR_6[i]*depth_fcr_2[6],
                          fcr_tfe_layers$FCR_7[i]*depth_fcr_2[7])/sum(depth_fcr_2[c(6:7)])
}

hypo_fcr_tmn <- rep(-99,length(fcr_tmn_layers$DateTime))
for(i in 1:length(fcr_tmn_layers$DateTime)){
  hypo_fcr_tmn[i] <- sum(fcr_tmn_layers$FCR_6[i]*depth_fcr_2[6],
                         fcr_tmn_layers$FCR_7[i]*depth_fcr_2[7])/sum(depth_fcr_2[c(6:7)])
}

hypo_fcr_sfe <- rep(-99,length(fcr_sfe_layers$DateTime))
for(i in 1:length(fcr_sfe_layers$DateTime)){
  hypo_fcr_sfe[i] <- sum(fcr_sfe_layers$FCR_6[i]*depth_fcr_2[6],
                         fcr_sfe_layers$FCR_7[i]*depth_fcr_2[7])/sum(depth_fcr_2[c(6:7)])
}

hypo_fcr_smn <- rep(-99,length(fcr_smn_layers$DateTime))
for(i in 1:length(fcr_smn_layers$DateTime)){
  hypo_fcr_smn[i] <- sum(fcr_smn_layers$FCR_6[i]*depth_fcr_2[6],
                         fcr_smn_layers$FCR_7[i]*depth_fcr_2[7])/sum(depth_fcr_2[c(6:7)])
}

hypo_fcr <- cbind.data.frame(fcr_tfe_layers$DateTime,hypo_fcr_tfe,hypo_fcr_tmn,hypo_fcr_sfe,hypo_fcr_smn)
names(hypo_fcr)[1] <- "DateTime"
names(hypo_fcr)[2] <- "TFe_mgL"
names(hypo_fcr)[3] <- "TMn_mgL"
names(hypo_fcr)[4] <- "SFe_mgL"
names(hypo_fcr)[5] <- "SMn_mgL"

# Plot to check
# TFe
ggplot(hypo_fcr,aes(DateTime,TFe_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

# TMn
ggplot(hypo_fcr,aes(DateTime,TMn_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

# SFe
ggplot(hypo_fcr,aes(DateTime,SFe_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

# SMn
ggplot(hypo_fcr,aes(DateTime,SMn_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

################################ BVR #######################################
# Divide data into layers
bvr_1 <- metals_bvr %>% filter(Depth_m==0.1) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="BVR_1")
bvr_1$TFe_mgL <- na.approx(bvr_1$TFe_mgL)
bvr_1$TMn_mgL <- na.approx(bvr_1$TMn_mgL)
bvr_1$SFe_mgL <- na.approx(bvr_1$SFe_mgL)
bvr_1$SMn_mgL <- na.approx(bvr_1$SMn_mgL)

bvr_2 <- metals_bvr %>% filter(Depth_m==3.0) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="BVR_2")
bvr_2$TFe_mgL <- na.approx(bvr_2$TFe_mgL)
bvr_2$TMn_mgL <- na.approx(bvr_2$TMn_mgL)
bvr_2$SFe_mgL <- na.approx(bvr_2$SFe_mgL)
bvr_2$SMn_mgL <- na.approx(bvr_2$SMn_mgL)

bvr_3 <- metals_bvr %>% filter(Depth_m==6) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="BVR_3")
bvr_3$TFe_mgL <- na.approx(bvr_3$TFe_mgL)
bvr_3$TMn_mgL <- na.approx(bvr_3$TMn_mgL)
bvr_3$SFe_mgL <- na.approx(bvr_3$SFe_mgL)
bvr_3$SMn_mgL <- na.approx(bvr_3$SMn_mgL)

bvr_4 <- metals_bvr %>% filter(Depth_m==9) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="BVR_4")
bvr_4$TFe_mgL <- na.approx(bvr_4$TFe_mgL)
bvr_4$TMn_mgL <- na.approx(bvr_4$TMn_mgL)
bvr_4$SFe_mgL <- na.approx(bvr_4$SFe_mgL)
bvr_4$SMn_mgL <- na.approx(bvr_4$SMn_mgL)

bvr_5 <- metals_bvr %>% filter(Depth_m==11) %>% group_by(DateTime) %>% summarize_all(funs(mean)) %>% 
  arrange(DateTime) %>% mutate(grouping="BVR_5")
bvr_5$TFe_mgL <- na.approx(bvr_5$TFe_mgL)
bvr_5$TMn_mgL <- na.approx(bvr_5$TMn_mgL)
bvr_5$SFe_mgL <- na.approx(bvr_5$SFe_mgL)
bvr_5$SMn_mgL <- na.approx(bvr_5$SMn_mgL)

# Then recombine depths
bvr_merge <- rbind(bvr_1,bvr_2,bvr_3,bvr_4,bvr_5)

bvr_tfe <- bvr_merge %>% select(DateTime,TFe_mgL,grouping) %>% arrange(DateTime)
bvr_tfe_layers <- bvr_tfe %>% spread(grouping,TFe_mgL)
bvr_tfe_layers$FCR_1 <- na.approx(bvr_tfe_layers$BVR_1)
bvr_tfe_layers$FCR_2 <- na.approx(bvr_tfe_layers$BVR_2)
bvr_tfe_layers$FCR_3 <- na.approx(bvr_tfe_layers$BVR_3)
bvr_tfe_layers$FCR_4 <- na.approx(bvr_tfe_layers$BVR_4)
bvr_tfe_layers$FCR_5 <- na.approx(bvr_tfe_layers$BVR_5)

bvr_tmn <- bvr_merge %>% select(DateTime,TMn_mgL,grouping) %>% arrange(DateTime)
bvr_tmn_layers <- bvr_tmn %>% spread(grouping,TMn_mgL)
bvr_tmn_layers$FCR_1 <- na.approx(bvr_tmn_layers$BVR_1)
bvr_tmn_layers$FCR_2 <- na.approx(bvr_tmn_layers$BVR_2)
bvr_tmn_layers$FCR_3 <- na.approx(bvr_tmn_layers$BVR_3)
bvr_tmn_layers$FCR_4 <- na.approx(bvr_tmn_layers$BVR_4)
bvr_tmn_layers$FCR_5 <- na.approx(bvr_tmn_layers$BVR_5)

bvr_sfe <- bvr_merge %>% select(DateTime,SFe_mgL,grouping) %>% arrange(DateTime)
bvr_sfe_layers <- bvr_sfe %>% spread(grouping,SFe_mgL)
bvr_sfe_layers$FCR_1 <- na.approx(bvr_sfe_layers$BVR_1)
bvr_sfe_layers$FCR_2 <- na.approx(bvr_sfe_layers$BVR_2)
bvr_sfe_layers$FCR_3 <- na.approx(bvr_sfe_layers$BVR_3)
bvr_sfe_layers$FCR_4 <- na.approx(bvr_sfe_layers$BVR_4)
bvr_sfe_layers$FCR_5 <- na.approx(bvr_sfe_layers$BVR_5)

bvr_smn <- bvr_merge %>% select(DateTime,SMn_mgL,grouping) %>% arrange(DateTime)
bvr_smn_layers <- bvr_smn %>% spread(grouping,SMn_mgL)
bvr_smn_layers$FCR_1 <- na.approx(bvr_smn_layers$BVR_1)
bvr_smn_layers$FCR_2 <- na.approx(bvr_smn_layers$BVR_2)
bvr_smn_layers$FCR_3 <- na.approx(bvr_smn_layers$BVR_3)
bvr_smn_layers$FCR_4 <- na.approx(bvr_smn_layers$BVR_4)
bvr_smn_layers$FCR_5 <- na.approx(bvr_smn_layers$BVR_5)

## Calculate Hypo VW average for TMn, TFe, SMn, and SFe
### USE 9M ONLY (aka: just the concentration as measured at 9 m)
hypo_bvr <- cbind.data.frame(bvr_tfe_layers$DateTime,bvr_tfe_layers$BVR_4,bvr_tmn_layers$BVR_4,
                             bvr_sfe_layers$BVR_4,bvr_smn_layers$BVR_4)
names(hypo_bvr)[1] <- "DateTime"
names(hypo_bvr)[2] <- "TFe_mgL"
names(hypo_bvr)[3] <- "TMn_mgL"
names(hypo_bvr)[4] <- "SFe_mgL"
names(hypo_bvr)[5] <- "SMn_mgL"

# Plot to check
# TFe
ggplot(hypo_bvr,aes(DateTime,TFe_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

# TMn
ggplot(hypo_bvr,aes(DateTime,TMn_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

# SFe
ggplot(hypo_bvr,aes(DateTime,SFe_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

# SMn
ggplot(hypo_bvr,aes(DateTime,SMn_mgL))+
  geom_line()+
  geom_point()+
  theme_classic()

######################## Plot for MS ###############################
# Plot all Fe data by year on one graph
fe_16 <- ggplot()+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='FCR_TFe'),size=1.1)+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='FCR_SFe'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='BVR_TFe'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='BVR_SFe'),size=1.1)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='FCR_TFe'),size=4)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='FCR_SFe'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='BVR_TFe'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='BVR_SFe'),size=4)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR_SFe","BVR_TFe","FCR_SFe","FCR_TFe"), 
                     labels=c("BVR Soluble Fe","BVR Total Fe","FCR Soluble Fe","FCR Total Fe"),
                     values=c('#ffa700','#F5793A','#91bfff','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("VW Hypo Fe (mg L"^-1*")")))+
  theme_classic(base_size=15)

fe_17 <- ggplot()+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='FCR_TFe'),size=1.1)+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='FCR_SFe'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='BVR_TFe'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='BVR_SFe'),size=1.1)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='FCR_TFe'),size=4)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='FCR_SFe'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='BVR_TFe'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='BVR_SFe'),size=4)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR_SFe","BVR_TFe","FCR_SFe","FCR_TFe"), 
                     labels=c("BVR Soluble Fe","BVR Total Fe","FCR Soluble Fe","FCR Total Fe"),
                     values=c('#ffa700','#F5793A','#91bfff','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab(expression(paste("VW Hypo Fe (mg L"^-1*")")))+
  theme_classic(base_size=15)

fe_18 <- ggplot()+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='FCR_TFe'),size=1.1)+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='FCR_SFe'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='BVR_TFe'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='BVR_SFe'),size=1.1)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='FCR_TFe'),size=4)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='FCR_SFe'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=TFe_mgL,group=1,color='BVR_TFe'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=SFe_mgL,group=1,color='BVR_SFe'),size=4)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-19"),color="#0F2080")+ # SSS Off in FCR
  scale_color_manual(breaks=c("BVR_SFe","BVR_TFe","FCR_SFe","FCR_TFe"), 
                     labels=c("BVR Soluble Fe","BVR Total Fe","FCR Soluble Fe","FCR Total Fe"),
                     values=c('#ffa700','#F5793A','#91bfff','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab(expression(paste("VW Hypo Fe (mg L"^-1*")")))+
  theme_classic(base_size=15)

ggarrange(fe_16,fe_17,fe_18,common.legend=TRUE,legend="right",ncol=2,nrow=2)
# Saved as: W = 1000, H = 700

# Plot all Mn data by year on one graph
mn_16 <- ggplot()+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='FCR_TMn'),size=1.1)+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='FCR_SMn'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='BVR_TMn'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='BVR_SMn'),size=1.1)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='FCR_TMn'),size=4)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='FCR_SMn'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='BVR_TMn'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='BVR_SMn'),size=4)+
  geom_vline(xintercept = as.POSIXct("2016-11-11"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-10-07"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR_SMn","BVR_TMn","FCR_SMn","FCR_TMn"), 
                     labels=c("BVR Soluble Mn","BVR Total Mn","FCR Soluble Mn","FCR Total Mn"),
                     values=c('#ffa700','#F5793A','#91bfff','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  xlab('2016')+
  ylab(expression(paste("VW Hypo Mn (mg L"^-1*")")))+
  theme_classic(base_size=15)

mn_17 <- ggplot()+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='FCR_TMn'),size=1.1)+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='FCR_SMn'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='BVR_TMn'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='BVR_SMn'),size=1.1)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='FCR_TMn'),size=4)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='FCR_SMn'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='BVR_TMn'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='BVR_SMn'),size=4)+
  geom_vline(xintercept = as.POSIXct("2017-11-07"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2017-10-30"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  scale_color_manual(breaks=c("BVR_SMn","BVR_TMn","FCR_SMn","FCR_TMn"), 
                     labels=c("BVR Soluble Mn","BVR Total Mn","FCR Soluble Mn","FCR Total Mn"),
                     values=c('#ffa700','#F5793A','#91bfff','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  xlab('2017')+
  ylab(expression(paste("VW Hypo Mn (mg L"^-1*")")))+
  theme_classic(base_size=15)

mn_18 <- ggplot()+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='FCR_TMn'),size=1.1)+
  geom_line(data=hypo_fcr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='FCR_SMn'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='BVR_TMn'),size=1.1)+
  geom_line(data=hypo_bvr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='BVR_SMn'),size=1.1)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='FCR_TMn'),size=4)+
  geom_point(data=hypo_fcr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='FCR_SMn'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=TMn_mgL,group=1,color='BVR_TMn'),size=4)+
  geom_point(data=hypo_bvr,mapping=aes(x=DateTime,y=SMn_mgL,group=1,color='BVR_SMn'),size=4)+
  geom_vline(xintercept = as.POSIXct("2018-10-29"),linetype="dashed",color="#F5793A")+ #Turnover
  geom_vline(xintercept = as.POSIXct("2018-10-21"),linetype="dashed",color="#0F2080")+ #Turnover FCR
  geom_vline(xintercept = as.POSIXct("2018-07-19"),color="#0F2080")+ # SSS Off in FCR
  scale_color_manual(breaks=c("BVR_SMn","BVR_TMn","FCR_SMn","FCR_TMn"), 
                     labels=c("BVR Soluble Mn","BVR Total Mn","FCR Soluble Mn","FCR Total Mn"),
                     values=c('#ffa700','#F5793A','#91bfff','#0F2080'))+
  labs(color="")+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  xlab('2018')+
  ylab(expression(paste("VW Hypo Mn (mg L"^-1*")")))+
  theme_classic(base_size=15)

ggarrange(mn_16,mn_17,mn_18,common.legend=TRUE,legend="right",ncol=2,nrow=2)
