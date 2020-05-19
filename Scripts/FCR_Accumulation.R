### Script to calculate Hypo GHG Max Hypo Volume for FCR and BVR
### Using updated GHG database from RPM and updated 2018 acoustic scan volumes

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# GHG data
ghg <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/DATASET_for_EDI_LOL_MS_10May20.csv")
ghg$DateTime <- as.POSIXct(strptime(ghg$DateTime, "%m/%d/%Y", tz="EST"))

# Depths data
fcr_depth <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/FCR_Bathy_Acoustic.csv")
bvr_depth <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data/BVR_Bathy_Acoustic.csv")

### Calculate for reps individually; focus on Hypo depths ONLY (8 m and 9 m for FCR)
fcr <- ghg %>% filter(Reservoir == "FCR")

ghg_6_rep1 <- fcr %>% filter(Depth_m==8.0,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_6")
ghg_6_rep1$ch4_umolL <- na.approx(ghg_6_rep1$ch4_umolL)
ghg_6_rep1$co2_umolL <- na.approx(ghg_6_rep1$co2_umolL)

ghg_6_rep2 <- fcr %>% filter(Depth_m==8.0,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_6")
ghg_6_rep2$ch4_umolL <- na.approx(ghg_6_rep2$ch4_umolL)
ghg_6_rep2$co2_umolL <- na.approx(ghg_6_rep2$co2_umolL)

ghg_7_rep1 <- fcr %>% filter(Depth_m==9.0,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="FCR_7")
ghg_7_rep1$ch4_umolL <- na.approx(ghg_7_rep1$ch4_umolL)
ghg_7_rep1$co2_umolL <- na.approx(ghg_7_rep1$co2_umolL)

ghg_7_rep2 <- fcr %>% filter(Depth_m==9.0,Rep==2) %>% group_by(DateTime) %>% 
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

#################################### FCR CH4 Max mass (mmol) #########################################

### Calculate Hypo Mass: defined as 6.5-9m
### Calculate Rep 1 and Rep 2; then combine
### Units in: mmol
hypo_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$DateTime))
for(i in 1:length(rep1_ch4_layers$DateTime)){
  hypo_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_6[i]*fcr_depth$Vol_m3[6]*1000,
                          rep1_ch4_layers$FCR_7[i]*fcr_depth$Vol_m3[7]*1000)/1000
}

# Hypo, rep 2, CH4
hypo_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$DateTime))
for(i in 1:length(rep2_ch4_layers$DateTime)){
  hypo_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_6[i]*fcr_depth$Vol_m3[6]*1000,
                          rep2_ch4_layers$FCR_7[i]*fcr_depth$Vol_m3[7]*1000)/1000
}


vw_ch4_all <- cbind.data.frame(hypo_ch4_rep1,hypo_ch4_rep2)
vw_ch4_avg <- rowMeans(vw_ch4_all,na.rm=TRUE)
vw_ch4_stdv <- apply(vw_ch4_all,1,FUN=sd,na.rm=TRUE)
vw_ch4_all <- cbind.data.frame(rep2_ch4_layers$DateTime,vw_ch4_all,vw_ch4_avg,vw_ch4_stdv)
names(vw_ch4_all)[1] <- "datetime"

vw_ch4_16 <- vw_ch4_all %>% filter(datetime>=as.Date('2016-04-06')&datetime<=as.Date('2016-09-30'))
vw_ch4_17 <- vw_ch4_all %>% filter(datetime>=as.Date('2017-04-06')&datetime<=as.Date('2017-09-30'))
vw_ch4_18 <- vw_ch4_all %>% filter(datetime>=as.Date('2018-04-06')&datetime<=as.Date('2018-09-30'))

# Plot mass (mmol)
ggplot(vw_ch4_all,aes(x=datetime,y=hypo_ch4_rep1))+
  geom_line()+
  theme_classic(base_size=15)

######################################## FCR CO2 Max Mass (mmol) #####################################
### Units in: mmol
hypo_co2_rep1 <- rep(-99,length(rep1_co2_layers$DateTime))
for(i in 1:length(rep1_co2_layers$DateTime)){
  hypo_co2_rep1[i] <- sum(rep1_co2_layers$FCR_6[i]*fcr_depth$Vol_m3[6]*1000,
                          rep1_co2_layers$FCR_7[i]*fcr_depth$Vol_m3[7]*1000)/1000
}

# Hypo, rep 2, CH4
hypo_co2_rep2 <- rep(-99,length(rep2_co2_layers$DateTime))
for(i in 1:length(rep2_co2_layers$DateTime)){
  hypo_co2_rep2[i] <- sum(rep2_co2_layers$FCR_6[i]*fcr_depth$Vol_m3[6]*1000,
                          rep2_co2_layers$FCR_7[i]*fcr_depth$Vol_m3[7]*1000)/1000
}

vw_co2_all <- cbind.data.frame(hypo_co2_rep1,hypo_co2_rep2)
vw_co2_avg <- rowMeans(vw_co2_all,na.rm=TRUE)
vw_co2_stdv <- apply(vw_co2_all,1,FUN=sd,na.rm=TRUE)
vw_co2_all <- cbind.data.frame(rep2_co2_layers$DateTime,vw_co2_all,vw_co2_avg,vw_co2_stdv)
names(vw_co2_all)[1] <- "datetime"

vw_co2_16 <- vw_co2_all %>% filter(datetime>=as.Date('2016-04-06')&datetime<=as.Date('2016-09-30'))
vw_co2_17 <- vw_co2_all %>% filter(datetime>=as.Date('2017-04-06')&datetime<=as.Date('2017-09-30'))
vw_co2_18 <- vw_co2_all %>% filter(datetime>=as.Date('2018-04-06')&datetime<=as.Date('2018-09-30'))

####################################### BVR CH4 Mass (mmol) ############################################
# Select BVR data only
bvr <- ghg %>% filter(Reservoir == "BVR")

ghg_4_rep1 <- bvr %>% filter(Depth_m==9,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_4")
ghg_4_rep1 <- ghg_4_rep1[-c(1),] # NAs for first date
ghg_4_rep1$ch4_umolL <- na.approx(ghg_4_rep1$ch4_umolL)
ghg_4_rep1$co2_umolL <- na.approx(ghg_4_rep1$co2_umolL)

ghg_4_rep2 <- bvr %>% filter(Depth_m==9,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_4")
ghg_4_rep2$ch4_umolL <- na.approx(ghg_4_rep2$ch4_umolL)
ghg_4_rep2$co2_umolL <- na.approx(ghg_4_rep2$co2_umolL)

rep1_co2 <- ghg_4_rep1 %>% select(DateTime,co2_umolL,grouping) %>% arrange(DateTime)
rep1_co2_layers <- rep1_co2 %>% spread(grouping,co2_umolL)

rep2_co2 <- ghg_4_rep2 %>% select(DateTime,co2_umolL,grouping) %>% arrange(DateTime)
rep2_co2_layers <- rep2_co2 %>% spread(grouping,co2_umolL)

rep1_ch4 <- ghg_4_rep1 %>% select(DateTime,ch4_umolL,grouping) %>% arrange(DateTime)
rep1_ch4_layers <- rep1_ch4 %>% spread(grouping,ch4_umolL)

rep2_ch4 <- ghg_4_rep2 %>% select(DateTime,ch4_umolL,grouping) %>% arrange(DateTime)
rep2_ch4_layers <- rep2_ch4 %>% spread(grouping,ch4_umolL)






##################################### BVR CO2 Mass (mmol) ###############################################










############################# FCR CH4 Accumulation Rate: DID NOT USE ######################################


# Calculate dGHG/dt for VW Hypo CH4
i_diff <- vw_ch4_all[c(1:104),]
f_diff <- vw_ch4_all[c(2:105),]
  
(z <- difftime(as.POSIXct(f_diff$datetime), as.POSIXct(i_diff$datetime), units="days"))
z <- as.numeric(z,units="days")

# dGHG/dt (units = mmol/d)
dch4_rep1_mmold <- (f_diff$hypo_ch4_rep1-i_diff$hypo_ch4_rep1)/z
dch4_rep2_mmold <- (f_diff$hypo_ch4_rep2-i_diff$hypo_ch4_rep2)/z

dch4 <- cbind(dch4_rep1_mmold,dch4_rep2_mmold)
dch4_avg <- rowMeans(dch4,na.rm=TRUE)
dch4_stdev <- apply(dch4,1,FUN=sd,na.rm=TRUE)

dch4 <- cbind.data.frame(f_diff$datetime,dch4,dch4_avg,dch4_stdev)
names(dch4)[1] <- "datetime"

diff <- cbind.data.frame(diff,dch4_rep2_mmold)
names(diff)[1] <- "datetime"

# Plot to check
ggplot(dch4,aes(datetime,dch4_avg))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=dch4_avg-dch4_stdev, ymax=dch4_avg+dch4_stdev))+
  theme_classic()

