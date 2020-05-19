### Script to calculate Hypo GHG accumulation rate for FCR
### Following FCR_Depths_Rev
### Using updated GHG database from RPM and updated 2018 acoustic scan volumes

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# GHG data
ghg <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/DATASET_for_EDI_LOL_MS_10May20.csv")
ghg$DateTime <- as.POSIXct(strptime(ghg$DateTime, "%m/%d/%Y", tz="EST"))

# Depths data
depth <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/FCR_Bathy_Acoustic.csv")

### Calculate for reps individually; focus on Hypo depths ONLY (8 m and 9 m for FCR)
ghg <- ghg %>% filter(Reservoir == "FCR")

ghg_6_rep1 <- ghg %>% filter(Depth_m==8.0,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_6")
ghg_6_rep1$ch4_umolL <- na.approx(ghg_6_rep1$ch4_umolL)
ghg_6_rep1$co2_umolL <- na.approx(ghg_6_rep1$co2_umolL)

ghg_6_rep2 <- ghg %>% filter(Depth_m==8.0,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_6")
ghg_6_rep2$ch4_umolL <- na.approx(ghg_6_rep2$ch4_umolL)
ghg_6_rep2$co2_umolL <- na.approx(ghg_6_rep2$co2_umolL)

ghg_7_rep1 <- ghg %>% filter(Depth_m==9.0,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_7")
ghg_7_rep1$ch4_umolL <- na.approx(ghg_7_rep1$ch4_umolL)
ghg_7_rep1$co2_umolL <- na.approx(ghg_7_rep1$co2_umolL)

ghg_7_rep2 <- ghg %>% filter(Depth_m==9.0,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_7")
ghg_7_rep2$ch4_umolL <- na.approx(ghg_7_rep2$ch4_umolL)
ghg_7_rep2$co2_umolL <- na.approx(ghg_7_rep2$co2_umolL)

# Recombine into reps1 and separate by CO2 and CH4
ghg_merge_rep1 <- rbind(ghg_6_rep1,ghg_7_rep1)
ghg_merge_rep2 <- rbind(ghg_6_rep2,ghg_7_rep2)

rep1_co2 <- ghg_merge_rep1 %>% select(DateTime,co2_umolL,grouping) %>% arrange(DateTime)
rep1_co2_layers <- rep1_co2 %>% spread(grouping,co2_umolL)

rep2_co2 <- ghg_merge_rep2 %>% select(DateTime,co2_umolL,grouping) %>% arrange(DateTime)
rep2_co2_layers <- rep2_co2 %>% spread(grouping,co2_umolL)

rep1_ch4 <- ghg_merge_rep1 %>% select(DateTime,ch4_umolL,grouping) %>% arrange(DateTime)
rep1_ch4_layers <- rep1_ch4 %>% spread(grouping,ch4_umolL)

rep2_ch4 <- ghg_merge_rep2 %>% select(DateTime,ch4_umolL,grouping) %>% arrange(DateTime)
rep2_ch4_layers <- rep2_ch4 %>% spread(grouping,ch4_umolL)

### Calculate Hypo Mass: defined as 6.5-9m
### Calculate Rep 1 and Rep 2; then combine
hypo_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$DateTime))
for(i in 1:length(rep1_ch4_layers$DateTime)){
  hypo_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_6[i]*depth$Vol_m3[6]*1000,
                          rep1_ch4_layers$FCR_7[i]*depth$Vol_m3[7]*1000)
}

########################################################################

# Hypo, rep 2, CH4
hypo_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$DateTime))
for(i in 1:length(rep2_ch4_layers$DateTime)){
  hypo_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_6[i]*depth$Vol_m3[6],
                          rep2_ch4_layers$FCR_7[i]*depth$Vol_m3[7])/sum(depth$Vol_m3[c(6:7)])
}

hypo_ch4 <- cbind(hypo_ch4_rep1,hypo_ch4_rep2)
hypo_ch4_avg <- rowMeans(hypo_ch4,na.rm=TRUE)
hypo_ch4_stdev <- apply(hypo_ch4,1,FUN=sd,na.rm=TRUE)

vw_ch4_all <- cbind.data.frame(vw_ch4_all,hypo_ch4_avg,hypo_ch4_stdev)
names(vw_ch4_all)[8] <- "hypo_avg"
names(vw_ch4_all)[9] <- "hypo_std"

# Plot to check
ggplot(vw_ch4_all,aes(datetime,hypo_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=hypo_avg-hypo_std, ymax=hypo_avg+hypo_std))+
  theme_classic()

## Separate into years (2016; 2017; 2018): three panel graph width = 1200; height = 300
vw_ch4_avg <- vw_ch4_all %>% select(datetime,vw_avg,epi_avg,meta_avg,hypo_avg)
names(vw_ch4_avg)[2] <- 'WaterCol'
names(vw_ch4_avg)[3] <- 'Epi'
names(vw_ch4_avg)[4] <- 'Meta'
names(vw_ch4_avg)[5] <- 'Hypo'

vw_ch4_avg_long <- vw_ch4_avg %>% pivot_longer(-datetime,names_to="depth",values_to="ch4_avg")

vw_ch4_std <- vw_ch4_all %>% select(datetime,vw_std,epi_std,meta_std,hypo_std)
names(vw_ch4_std)[2] <- 'WaterCol'
names(vw_ch4_std)[3] <- 'Epi'
names(vw_ch4_std)[4] <- 'Meta'
names(vw_ch4_std)[5] <- 'Hypo'

vw_ch4_std_long <- vw_ch4_std %>% pivot_longer(-datetime,names_to="depth",values_to="ch4_std")

vw_ch4_long <- merge(vw_ch4_avg_long,vw_ch4_std_long,by=c("datetime","depth"))

vw_ch4_16 <- vw_ch4_long %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_ch4_17 <- vw_ch4_long %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_ch4_18 <- vw_ch4_long %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

# Remove rows with NA averages (but not SD!)
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

vw_ch4_16 <- completeFun(vw_ch4_16,"ch4_avg")
vw_ch4_17 <- completeFun(vw_ch4_17,"ch4_avg")
vw_ch4_18 <- completeFun(vw_ch4_18,"ch4_avg")

ch416 <- ggplot(vw_ch4_16,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
  labs(color="")+
  xlab('2016')+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size = 15)

ch417 <- ggplot(vw_ch4_17,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  theme_classic(base_size=15)

ch418 <- ggplot(vw_ch4_18,aes(x = datetime, y = ch4_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ch4_avg-ch4_std,ymax=ch4_avg+ch4_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-04-23"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2018-07-30"),linetype="longdash")+ #Oxygen off
  geom_vline(xintercept=as.POSIXct("2018-10-21"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression(paste("CH"[4]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(ch416,ch417,ch418,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_ch4_all, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/FCR_VW_ch4_stats_16to18_Revs.csv")

############################CO2##########################################################
## Calculate VW pCO2 for FCR
# Water column
vw_co2_rep1 <- rep(-99,length(rep1_co2_layers$DateTime))
for(i in 1:length(rep1_co2_layers$DateTime)){
  vw_co2_rep1[i] <- sum(rep1_co2_layers$FCR_1[i]*depth[1,1],
                        rep1_co2_layers$FCR_2[i]*depth[2,1],
                        rep1_co2_layers$FCR_3[i]*depth[3,1],
                        rep1_co2_layers$FCR_4[i]*depth[4,1],
                        rep1_co2_layers$FCR_5[i]*depth[5,1],
                        rep1_co2_layers$FCR_6[i]*depth[6,1],
                        rep1_co2_layers$FCR_7[i]*depth[7,1])/sum(depth[c(1:7),1])
}

vw_co2_rep1 <- cbind(rep1_co2_layers,vw_co2_rep1)

# Plot to check
ggplot(vw_co2_rep1,aes(DateTime,vw_co2_rep1))+geom_line()+theme_classic()

# Calculate VW pCO2 for Rep 2
vw_co2_rep2 <- rep(-99,length(rep2_co2_layers$DateTime))
for(i in 1:length(rep2_co2_layers$DateTime)){
  vw_co2_rep2[i] <- sum(rep2_co2_layers$FCR_1[i]*depth[1,1],
                        rep2_co2_layers$FCR_2[i]*depth[2,1],
                        rep2_co2_layers$FCR_3[i]*depth[3,1],
                        rep2_co2_layers$FCR_4[i]*depth[4,1],
                        rep2_co2_layers$FCR_5[i]*depth[5,1],
                        rep2_co2_layers$FCR_6[i]*depth[6,1],
                        rep2_co2_layers$FCR_7[i]*depth[7,1])/sum(depth[c(1:7),1])
}

vw_co2_rep2 <- cbind(rep2_co2_layers,vw_co2_rep2)

# Plot to check
ggplot(vw_co2_rep2,aes(DateTime,vw_co2_rep2))+geom_line()+theme_classic()

# Calculate Avg and Stdev for Water Col VW
vw_co2 <- cbind(vw_co2_rep1$vw_co2_rep1,vw_co2_rep2$vw_co2_rep2)

vw_co2_avg <- rowMeans(vw_co2,na.rm=TRUE)
vw_co2_stdev <- apply(vw_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_rep1$DateTime,vw_co2_avg,vw_co2_stdev)
names(vw_co2_all)[1] <- "datetime"
names(vw_co2_all)[2] <- "vw_avg"
names(vw_co2_all)[3] <- "vw_std"

# Plot to check: avg and stdev
ggplot(vw_co2_all,aes(datetime,vw_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=vw_avg-vw_std, ymax=vw_avg+vw_std))+
  theme_classic()

### Calculate Epi VW pCO2: defined as 0-2.6 m
# Calculate as Rep 1 and Rep 2 then combine
epi_co2_rep1 <- rep(-99,length(rep1_co2_layers$DateTime))
for(i in 1:length(rep1_co2_layers$DateTime)){
  epi_co2_rep1[i] <- sum(rep1_co2_layers$FCR_1[i]*depth[1,1],
                         rep1_co2_layers$FCR_2[i]*depth[2,1])/sum(depth[c(1:2),1])
}

# Epi Rep 2 CO2
epi_co2_rep2 <- rep(-99,length(rep2_co2_layers$DateTime))
for(i in 1:length(rep2_co2_layers$DateTime)){
  epi_co2_rep2[i] <- sum(rep2_co2_layers$FCR_1[i]*depth[1,1],
                         rep2_co2_layers$FCR_2[i]*depth[2,1])/sum(depth[c(1:2),1])
}

epi_co2 <- cbind(epi_co2_rep1,epi_co2_rep2)
epi_co2_avg <- rowMeans(epi_co2,na.rm=TRUE)
epi_co2_stdev <- apply(epi_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_all,epi_co2_avg,epi_co2_stdev)
names(vw_co2_all)[4] <- "epi_avg"
names(vw_co2_all)[5] <- "epi_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,epi_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=epi_avg-epi_std, ymax=epi_avg+epi_std))+
  theme_classic()

### Calculate Meta VW pCO2: defined as 2.6-6.5m
### Calculate Rep 1 and 2 seperately
meta_co2_rep1 <- rep(-99,length(rep1_co2_layers$DateTime))
for(i in 1:length(rep1_co2_layers$DateTime)){
  meta_co2_rep1[i] <- sum(rep1_co2_layers$FCR_3[i]*depth[3,1],
                          rep1_co2_layers$FCR_4[i]*depth[4,1],
                          rep1_co2_layers$FCR_5[i]*depth[5,1])/sum(depth[c(3:5),1])
}

# Meta, rep 2, CH4
meta_co2_rep2 <- rep(-99,length(rep2_co2_layers$DateTime))
for(i in 1:length(rep2_co2_layers$DateTime)){
  meta_co2_rep2[i] <- sum(rep2_co2_layers$FCR_3[i]*depth[3,1],
                          rep2_co2_layers$FCR_4[i]*depth[4,1],
                          rep2_co2_layers$FCR_5[i]*depth[5,1])/sum(depth[c(3:5),1])
}

meta_co2 <- cbind(meta_co2_rep1,meta_co2_rep2)
meta_co2_avg <- rowMeans(meta_co2,na.rm=TRUE)
meta_co2_stdev <- apply(meta_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_all,meta_co2_avg,meta_co2_stdev)
names(vw_co2_all)[6] <- "meta_avg"
names(vw_co2_all)[7] <- "meta_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,meta_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=meta_avg-meta_std, ymax=meta_avg+meta_std))+
  theme_classic()

### Calculate Hypo VW Temp: defined as 6.5-9m
### Calculate Rep 1 and Rep 2; then combine
hypo_co2_rep1 <- rep(-99,length(rep1_co2_layers$DateTime))
for(i in 1:length(rep1_co2_layers$DateTime)){
  hypo_co2_rep1[i] <- sum(rep1_co2_layers$FCR_6[i]*depth[6,1],
                          rep1_co2_layers$FCR_7[i]*depth[7,1])/sum(depth[c(6:7),1])
}

# Hypo, rep 2, CH4
hypo_co2_rep2 <- rep(-99,length(rep2_co2_layers$DateTime))
for(i in 1:length(rep2_co2_layers$DateTime)){
  hypo_co2_rep2[i] <- sum(rep2_co2_layers$FCR_6[i]*depth[6,1],
                          rep2_co2_layers$FCR_7[i]*depth[7,1])/sum(depth[c(6:7),1])
}

hypo_co2 <- cbind(hypo_co2_rep1,hypo_co2_rep2)
hypo_co2_avg <- rowMeans(hypo_co2,na.rm=TRUE)
hypo_co2_stdev <- apply(hypo_co2,1,FUN=sd,na.rm=TRUE)

vw_co2_all <- cbind.data.frame(vw_co2_all,hypo_co2_avg,hypo_co2_stdev)
names(vw_co2_all)[8] <- "hypo_avg"
names(vw_co2_all)[9] <- "hypo_std"

# Plot to check
ggplot(vw_co2_all,aes(datetime,hypo_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=hypo_avg-hypo_std, ymax=hypo_avg+hypo_std))+
  theme_classic()

## Separate into years (2016; 2017): width = 1000; height = 400
vw_co2_avg <- vw_co2_all %>% select(datetime,vw_avg,epi_avg,meta_avg,hypo_avg)
names(vw_co2_avg)[2] <- 'WaterCol'
names(vw_co2_avg)[3] <- 'Epi'
names(vw_co2_avg)[4] <- 'Meta'
names(vw_co2_avg)[5] <- 'Hypo'

vw_co2_avg_long <- vw_co2_avg %>% pivot_longer(-datetime,names_to="depth",values_to="co2_avg")

vw_co2_std <- vw_co2_all %>% select(datetime,vw_std,epi_std,meta_std,hypo_std)
names(vw_co2_std)[2] <- 'WaterCol'
names(vw_co2_std)[3] <- 'Epi'
names(vw_co2_std)[4] <- 'Meta'
names(vw_co2_std)[5] <- 'Hypo'

#### Need to check this ####

## Calculate Hypo 'averaged' concentration over each summer
# First select the correct date range for each summer
# April 1 - Turnover
vw_co2_avg_16 <- vw_co2_avg %>% filter(datetime>=as.Date('2016-04-01')&
                                         datetime<=as.Date('2016-10-14'))
vw_co2_avg_17 <- vw_co2_avg %>% filter(datetime>=as.Date('2017-04-01')&
                                         datetime<=as.Date('2017-12-12'))

vw_ch4_avg_16 <- vw_ch4_avg %>% filter(datetime>=as.Date('2016-04-01')&datetime<=as.Date('2016-10-14'))
vw_ch4_avg_17 <- vw_ch4_avg %>% filter(datetime>=as.Date('2017-04-01')&datetime<=as.Date('2017-12-12'))

vw_hypo_co2_16 <- sum(vw_co2_avg_16$Hypo)/length(vw_co2_avg_16$Hypo)
vw_hypo_co2_17 <- sum(vw_co2_avg_17$Hypo)/length(vw_co2_avg_17$Hypo)
vw_hypo_ch4_16 <- sum(vw_ch4_avg_16$Hypo)/length(vw_ch4_avg_16$Hypo)
vw_hypo_ch4_17 <- sum(vw_ch4_avg_17$Hypo)/length(vw_ch4_avg_17$Hypo)

### Go back to plotting

vw_co2_std_long <- vw_co2_std %>% pivot_longer(-datetime,names_to="depth",values_to="co2_std")

vw_co2_long <- merge(vw_co2_avg_long,vw_co2_std_long,by=c("datetime","depth"))

vw_co2_16 <- vw_co2_long %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_co2_17 <- vw_co2_long %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_co2_18 <- vw_co2_long %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

# Remove rows with NA averages (but not SD!)
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

vw_co2_16 <- completeFun(vw_co2_16,"co2_avg")
vw_co2_17 <- completeFun(vw_co2_17,"co2_avg")
vw_co2_18 <- completeFun(vw_co2_18,"co2_avg")

co216 <- ggplot(vw_co2_16,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
  labs(color="")+
  xlab('2016')+
  xlim(as.POSIXct("2016-04-01"),as.POSIXct("2016-11-30"))+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size = 15)

co217 <- ggplot(vw_co2_17,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),
                     values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  xlim(as.POSIXct("2017-04-01"),as.POSIXct("2017-11-30"))+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  theme_classic(base_size=15)

co218 <- ggplot(vw_co2_18,aes(x = datetime, y = co2_avg, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=co2_avg-co2_std,ymax=co2_avg+co2_std))+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2018-04-23"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2018-07-30"),linetype="longdash")+ #Oxygen off
  geom_vline(xintercept=as.POSIXct("2018-10-21"))+ #Turnover
  labs(color="")+
  xlab('2018')+
  ylab(expression(paste("CO"[2]*" (", mu,"mol L"^-1*")")))+
  xlim(as.POSIXct("2018-04-01"),as.POSIXct("2018-11-30"))+
  theme_classic(base_size = 15)

ggarrange(co216,co217,co218,common.legend=TRUE,legend="right",ncol=3,nrow=1)

# Export out Temp Data VW averaged by depth
write_csv(vw_co2_all, path = "C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/FCR_VW_co2_stats_16to18_Revs.csv")
