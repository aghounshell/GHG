### Script to calculate Hypo GHG Max Hypo Volume for FCR and BVR
### Using updated GHG database from RPM and updated 2018 acoustic scan volumes

# Rfile saved as: Max_GWP

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

#################################### FCR CH4 Max concentration (umol/L) #########################################
### Calculate Hypo Mass: defined as 6.5-9m
### Calculate Rep 1 and Rep 2; then combine
### Units in: mmol
hypo_ch4_rep1 <- rep(-99,length(rep1_ch4_layers$DateTime))
for(i in 1:length(rep1_ch4_layers$DateTime)){
  hypo_ch4_rep1[i] <- sum(rep1_ch4_layers$FCR_6[i]*fcr_depth$Vol_m3[6],
                          rep1_ch4_layers$FCR_7[i]*fcr_depth$Vol_m3[7])/sum(fcr_depth$Vol_m3[c(6:7)])
}

# Hypo, rep 2, CH4
hypo_ch4_rep2 <- rep(-99,length(rep2_ch4_layers$DateTime))
for(i in 1:length(rep2_ch4_layers$DateTime)){
  hypo_ch4_rep2[i] <- sum(rep2_ch4_layers$FCR_6[i]*fcr_depth$Vol_m3[6],
                          rep2_ch4_layers$FCR_7[i]*fcr_depth$Vol_m3[7])/sum(fcr_depth$Vol_m3[c(6:7)])
}


vw_ch4_all <- cbind.data.frame(hypo_ch4_rep1,hypo_ch4_rep2)
vw_ch4_avg <- rowMeans(vw_ch4_all,na.rm=TRUE)
vw_ch4_stdv <- apply(vw_ch4_all,1,FUN=sd,na.rm=TRUE)
vw_ch4_all <- cbind.data.frame(rep2_ch4_layers$DateTime,vw_ch4_all,vw_ch4_avg,vw_ch4_stdv)
names(vw_ch4_all)[1] <- "datetime"

vw_ch4_16 <- vw_ch4_all %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_ch4_17 <- vw_ch4_all %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_ch4_17 <- vw_ch4_17[complete.cases(vw_ch4_17),]
vw_ch4_18 <- vw_ch4_all %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))
vw_ch4_18 <- vw_ch4_18[complete.cases(vw_ch4_18),]

# Plot concentration (umol/L)
ggplot(vw_ch4_all,aes(x=datetime,y=hypo_ch4_rep1))+
  geom_line()+
  theme_classic(base_size=15)

######################################## FCR CO2 Max Mass (mmol) #####################################
### Units in: mmol
hypo_co2_rep1 <- rep(-99,length(rep1_co2_layers$DateTime))
for(i in 1:length(rep1_co2_layers$DateTime)){
  hypo_co2_rep1[i] <- sum(rep1_co2_layers$FCR_6[i]*fcr_depth$Vol_m3[6],
                          rep1_co2_layers$FCR_7[i]*fcr_depth$Vol_m3[7])/sum(fcr_depth$Vol_m3[c(6:7)])
}

# Hypo, rep 2, CH4
hypo_co2_rep2 <- rep(-99,length(rep2_co2_layers$DateTime))
for(i in 1:length(rep2_co2_layers$DateTime)){
  hypo_co2_rep2[i] <- sum(rep2_co2_layers$FCR_6[i]*fcr_depth$Vol_m3[6],
                          rep2_co2_layers$FCR_7[i]*fcr_depth$Vol_m3[7])/sum(fcr_depth$Vol_m3[c(6:7)])
}

vw_co2_all <- cbind.data.frame(hypo_co2_rep1,hypo_co2_rep2)
vw_co2_avg <- rowMeans(vw_co2_all,na.rm=TRUE)
vw_co2_stdv <- apply(vw_co2_all,1,FUN=sd,na.rm=TRUE)
vw_co2_all <- cbind.data.frame(rep2_co2_layers$DateTime,vw_co2_all,vw_co2_avg,vw_co2_stdv)
names(vw_co2_all)[1] <- "datetime"

vw_co2_16 <- vw_co2_all %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
vw_co2_17 <- vw_co2_all %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
vw_co2_17 <- vw_co2_17[complete.cases(vw_co2_17),]
vw_co2_18 <- vw_co2_all %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))
vw_co2_18 <- vw_co2_18[complete.cases(vw_co2_18),]

####################################### BVR CH4 Mass (mmol) ############################################
# Select BVR data only
bvr <- ghg %>% filter(Reservoir == "BVR")

# Separate by depth then recombine
ghg_1_rep1 <- bvr %>% filter(Depth_m==0.1,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_1")
ghg_1_rep1$ch4_umolL <- na.approx(ghg_1_rep1$ch4_umolL)
ghg_1_rep1$co2_umolL <- na.approx(ghg_1_rep1$co2_umolL)

ghg_1_rep2 <- bvr %>% filter(Depth_m==0.1,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_1")
ghg_1_rep2$ch4_umolL <- na.approx(ghg_1_rep2$ch4_umolL)
ghg_1_rep2$co2_umolL <- na.approx(ghg_1_rep2$co2_umolL)

ghg_2_rep1 <- bvr %>% filter(Depth_m==3,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_2")
ghg_2_rep1$ch4_umolL <- na.approx(ghg_2_rep1$ch4_umolL)
ghg_2_rep1$co2_umolL <- na.approx(ghg_2_rep1$co2_umolL)

ghg_2_rep2 <- bvr %>% filter(Depth_m==3,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_2")
ghg_2_rep2$ch4_umolL <- na.approx(ghg_2_rep2$ch4_umolL)
ghg_2_rep2$co2_umolL <- na.approx(ghg_2_rep2$co2_umolL)

ghg_3_rep1 <- bvr %>% filter(Depth_m==6,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_3")
ghg_3_rep1$ch4_umolL <- na.approx(ghg_3_rep1$ch4_umolL)
ghg_3_rep1$co2_umolL <- na.approx(ghg_3_rep1$co2_umolL)

ghg_3_rep2 <- bvr %>% filter(Depth_m==6,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_3")
ghg_3_rep2$ch4_umolL <- na.approx(ghg_3_rep2$ch4_umolL)
ghg_3_rep2$co2_umolL <- na.approx(ghg_3_rep2$co2_umolL)

ghg_4_rep1 <- bvr %>% filter(Depth_m==9,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_4")
ghg_4_rep1 <- ghg_4_rep1[-c(1),] # NAs for first date
ghg_4_rep1$ch4_umolL <- na.approx(ghg_4_rep1$ch4_umolL)
ghg_4_rep1$co2_umolL <- na.approx(ghg_4_rep1$co2_umolL)

ghg_4_rep2 <- bvr %>% filter(Depth_m==9,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_4")
ghg_4_rep2$ch4_umolL <- na.approx(ghg_4_rep2$ch4_umolL)
ghg_4_rep2$co2_umolL <- na.approx(ghg_4_rep2$co2_umolL)

ghg_5_rep1 <- bvr %>% filter(Depth_m==11,Rep==1) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_5")
ghg_5_rep1$ch4_umolL <- na.approx(ghg_5_rep1$ch4_umolL)
ghg_5_rep1$co2_umolL <- na.approx(ghg_5_rep1$co2_umolL)

ghg_5_rep2 <- bvr %>% filter(Depth_m==11,Rep==2) %>% group_by(DateTime) %>% 
  summarize_all(funs(mean)) %>% arrange(DateTime) %>% mutate(grouping="BVR_5")
ghg_5_rep2 <- ghg_5_rep2[-c(50),] # NAs for first date
ghg_5_rep2$ch4_umolL <- na.approx(ghg_5_rep2$ch4_umolL)
ghg_5_rep2$co2_umolL <- na.approx(ghg_5_rep2$co2_umolL)

# Re-combine reps and separate by co2 and ch4
ghg_merge_rep1 <- rbind(ghg_1_rep1,ghg_2_rep1,ghg_3_rep1,ghg_4_rep1,ghg_5_rep1)
ghg_merge_rep2 <- rbind(ghg_1_rep2,ghg_2_rep2,ghg_3_rep2,ghg_4_rep2,ghg_5_rep2)

rep1_co2 <- ghg_merge_rep1 %>% select(DateTime,co2_umolL,grouping) %>% arrange(DateTime)
rep1_co2_layers <- rep1_co2 %>% spread(grouping,co2_umolL)
rep1_co2_layers$BVR_5 <- na.approx(rep1_co2_layers$BVR_5,na.rm=FALSE)

rep2_co2 <- ghg_merge_rep2 %>% select(DateTime,co2_umolL,grouping) %>% arrange(DateTime)
rep2_co2_layers <- rep2_co2 %>% spread(grouping,co2_umolL)
rep2_co2_layers$BVR_5 <- na.approx(rep2_co2_layers$BVR_5,na.rm=FALSE)

rep1_ch4 <- ghg_merge_rep1 %>% select(DateTime,ch4_umolL,grouping) %>% arrange(DateTime)
rep1_ch4_layers <- rep1_ch4 %>% spread(grouping,ch4_umolL)
rep1_ch4_layers$BVR_5 <- na.approx(rep1_ch4_layers$BVR_5,na.rm=FALSE)

rep2_ch4 <- ghg_merge_rep2 %>% select(DateTime,ch4_umolL,grouping) %>% arrange(DateTime)
rep2_ch4_layers <- rep2_ch4 %>% spread(grouping,ch4_umolL)
rep2_ch4_layers$BVR_5 <- na.approx(rep2_ch4_layers$BVR_5,na.rm=FALSE)

# Remove row 31 (only have 0.1m depth)
rep1_co2_layers <- rep1_co2_layers[-c(31),]
rep2_co2_layers <- rep2_co2_layers[-c(31),]
rep1_ch4_layers <- rep1_ch4_layers[-c(31),]
rep2_ch4_layers <- rep2_ch4_layers[-c(31),]

# Calculate mass (mmol)
bvr_ch4_rep1 <- rep1_ch4_layers$BVR_4
bvr_ch4_rep2 <- rep2_ch4_layers$BVR_4

bvr_vw_ch4_all <- cbind.data.frame(bvr_ch4_rep1,bvr_ch4_rep2)
bvr_vw_ch4_avg <- rowMeans(bvr_vw_ch4_all,na.rm=TRUE)
bvr_vw_ch4_stdv <- apply(bvr_vw_ch4_all,1,FUN=sd,na.rm=TRUE)
bvr_vw_ch4_all <- cbind.data.frame(rep2_ch4_layers$DateTime,bvr_vw_ch4_all,bvr_vw_ch4_avg,bvr_vw_ch4_stdv)
names(bvr_vw_ch4_all)[1] <- "datetime"

bvr_vw_ch4_16 <- bvr_vw_ch4_all %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
bvr_vw_ch4_17 <- bvr_vw_ch4_all %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
bvr_vw_ch4_18 <- bvr_vw_ch4_all %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

##################################### BVR CO2 Mass (mmol) ###############################################
# Calculate mass (mmol)
bvr_co2_rep1 <- rep1_co2_layers$BVR_4
bvr_co2_rep2 <- rep2_co2_layers$BVR_4

bvr_vw_co2_all <- cbind.data.frame(bvr_co2_rep1,bvr_co2_rep2)
bvr_vw_co2_avg <- rowMeans(bvr_vw_co2_all,na.rm=TRUE)
bvr_vw_co2_stdv <- apply(bvr_vw_co2_all,1,FUN=sd,na.rm=TRUE)
bvr_vw_co2_all <- cbind.data.frame(rep2_co2_layers$DateTime,bvr_vw_co2_all,bvr_vw_co2_avg,bvr_vw_co2_stdv)
names(bvr_vw_co2_all)[1] <- "datetime"

bvr_vw_co2_16 <- bvr_vw_co2_all %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
bvr_vw_co2_17 <- bvr_vw_co2_all %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
bvr_vw_co2_18 <- bvr_vw_co2_all %>% filter(datetime>=as.Date('2018-01-01')&datetime<=as.Date('2018-12-31'))

###################################### Combine all Masses (mmol) #########################################
data <- matrix(ncol=2,nrow=12)

row.names(data) <- c("bvr_ch4_16","bvr_co2_16","bvr_ch4_17","bvr_co2_17","bvr_ch4_18","bvr_co2_18",
                     "fcr_ch4_16","fcr_co2_16","fcr_ch4_17","fcr_co2_17","fcr_ch4_18","fcr_co2_18")
colnames(data) <- c("Avg", "Stdev")

data[1,1] <- max(bvr_vw_ch4_16$bvr_vw_ch4_avg)
data[1,2] <- bvr_vw_ch4_16$bvr_vw_ch4_stdv[which.max(bvr_vw_ch4_16$bvr_vw_ch4_avg)]
data[2,1] <- max(bvr_vw_co2_16$bvr_vw_co2_avg)
data[2,2] <- bvr_vw_co2_16$bvr_vw_co2_stdv[which.max(bvr_vw_co2_16$bvr_vw_co2_avg)]

data[3,1] <- max(bvr_vw_ch4_17$bvr_vw_ch4_avg)
data[3,2] <- bvr_vw_ch4_17$bvr_vw_ch4_stdv[which.max(bvr_vw_ch4_17$bvr_vw_ch4_avg)]
data[4,1] <- max(bvr_vw_co2_17$bvr_vw_co2_avg)
data[4,2] <- bvr_vw_co2_17$bvr_vw_co2_stdv[which.max(bvr_vw_co2_17$bvr_vw_co2_avg)]

data[5,1] <- max(bvr_vw_ch4_18$bvr_vw_ch4_avg)
data[5,2] <- bvr_vw_ch4_18$bvr_vw_ch4_stdv[which.max(bvr_vw_ch4_18$bvr_vw_ch4_avg)]
data[6,1] <- max(bvr_vw_co2_18$bvr_vw_co2_avg)
data[6,2] <- bvr_vw_co2_18$bvr_vw_co2_stdv[which.max(bvr_vw_co2_18$bvr_vw_co2_avg)]

data[7,1] <- max(vw_ch4_16$vw_ch4_avg)
data[7,2] <- vw_ch4_16$vw_ch4_stdv[which.max(vw_ch4_16$vw_ch4_avg)]
data[8,1] <- max(vw_co2_16$vw_co2_avg)
data[8,2] <- vw_co2_16$vw_co2_stdv[which.max(vw_co2_16$vw_co2_avg)]

data[9,1] <- max(vw_ch4_17$vw_ch4_avg)
data[9,2] <- vw_ch4_17$vw_ch4_stdv[which.max(vw_ch4_17$vw_ch4_avg)]
data[10,1] <- max(vw_co2_17$vw_co2_avg)
data[10,2] <- vw_co2_17$vw_co2_stdv[which.max(vw_co2_17$vw_co2_avg)]

data[11,1] <- max(vw_ch4_18$vw_ch4_avg)
data[11,2] <- vw_ch4_18$vw_ch4_stdv[which.max(vw_ch4_18$vw_ch4_avg)]
data[12,1] <- max(vw_co2_18$vw_co2_avg)
data[12,2] <- vw_co2_18$vw_co2_stdv[which.max(vw_co2_18$vw_co2_avg)]

data <- as.data.frame(data)

data <- data %>% add_column(year=c('2016','2016','2017','2017','2018','2018','2016','2016','2017','2017',
                                   '2018','2018'))

data <- data %>% add_column(res_gas=c('BVR_ch4','BVR_co2','BVR_ch4','BVR_co2','BVR_ch4','BVR_co2',
                                          'FCR_ch4','FCR_co2','FCR_ch4','FCR_co2','FCR_ch4','FCR_co2'))

################################# Calculate Hypo GWP from maximum GHG concentrations #########################
gwp <- matrix(ncol=4,nrow=6)

row.names(gwp) <- c("bvr_16","bvr_17","bvr_18","fcr_16","fcr_17","fcr_18")

colnames(gwp) <- c("ch4_rep1","ch4_rep2","co2_rep1","co2_rep2")

gwp[1,1] <- bvr_vw_ch4_16$bvr_ch4_rep1[which.max(bvr_vw_ch4_16$bvr_vw_ch4_avg)]
gwp[1,2] <- bvr_vw_ch4_16$bvr_ch4_rep2[which.max(bvr_vw_ch4_16$bvr_vw_ch4_avg)]
gwp[1,3] <- bvr_vw_co2_16$bvr_co2_rep1[which.max(bvr_vw_co2_16$bvr_vw_co2_avg)]
gwp[1,4] <- bvr_vw_co2_16$bvr_co2_rep2[which.max(bvr_vw_co2_16$bvr_vw_co2_avg)]

gwp[2,1] <- bvr_vw_ch4_17$bvr_ch4_rep1[which.max(bvr_vw_ch4_17$bvr_vw_ch4_avg)]
gwp[2,2] <- bvr_vw_ch4_17$bvr_ch4_rep2[which.max(bvr_vw_ch4_17$bvr_vw_ch4_avg)]
gwp[2,3] <- bvr_vw_co2_17$bvr_co2_rep1[which.max(bvr_vw_co2_17$bvr_vw_co2_avg)]
gwp[2,4] <- bvr_vw_co2_17$bvr_co2_rep2[which.max(bvr_vw_co2_17$bvr_vw_co2_avg)]

gwp[3,1] <- bvr_vw_ch4_18$bvr_ch4_rep1[which.max(bvr_vw_ch4_18$bvr_vw_ch4_avg)]
gwp[3,2] <- bvr_vw_ch4_18$bvr_ch4_rep2[which.max(bvr_vw_ch4_18$bvr_vw_ch4_avg)]
gwp[3,3] <- bvr_vw_co2_18$bvr_co2_rep1[which.max(bvr_vw_co2_18$bvr_vw_co2_avg)]
gwp[3,4] <- bvr_vw_co2_18$bvr_co2_rep2[which.max(bvr_vw_co2_18$bvr_vw_co2_avg)]

gwp[4,1] <- vw_ch4_16$hypo_ch4_rep1[which.max(vw_ch4_16$vw_ch4_avg)]
gwp[4,2] <- vw_ch4_16$hypo_ch4_rep2[which.max(vw_ch4_16$vw_ch4_avg)]
gwp[4,3] <- vw_co2_16$hypo_co2_rep1[which.max(vw_co2_16$vw_co2_avg)]
gwp[4,4] <- vw_co2_16$hypo_co2_rep2[which.max(vw_co2_16$vw_co2_avg)]

gwp[5,1] <- vw_ch4_17$hypo_ch4_rep1[which.max(vw_ch4_17$vw_ch4_avg)]
gwp[5,2] <- vw_ch4_17$hypo_ch4_rep2[which.max(vw_ch4_17$vw_ch4_avg)]
gwp[5,3] <- vw_co2_17$hypo_co2_rep1[which.max(vw_co2_17$vw_co2_avg)]
gwp[5,4] <- vw_co2_17$hypo_co2_rep2[which.max(vw_co2_17$vw_co2_avg)]

gwp[6,1] <- vw_ch4_18$hypo_ch4_rep1[which.max(vw_ch4_18$vw_ch4_avg)]
gwp[6,2] <- vw_ch4_18$hypo_ch4_rep2[which.max(vw_ch4_18$vw_ch4_avg)]
gwp[6,3] <- vw_co2_18$hypo_co2_rep1[which.max(vw_co2_18$vw_co2_avg)]
gwp[6,4] <- vw_co2_18$hypo_co2_rep2[which.max(vw_co2_18$vw_co2_avg)]

gwp <- as.data.frame(gwp)

gwp_rep1 <- rep(-99,length(gwp$ch4_rep1))
for(i in 1:length(gwp$ch4_rep1)){
  gwp_rep1[i] <- ((gwp$ch4_rep1[i]*34*16.04)+(gwp$co2_rep1[i]*44.01))/1000000
}

gwp <- cbind.data.frame(gwp,gwp_rep1)

gwp_rep2 <- rep(-99,length(gwp$ch4_rep2))
for(i in 1:length(gwp$ch4_rep2)){
  gwp_rep2[i] <- ((gwp$ch4_rep2[i]*34*16.04)+(gwp$co2_rep2[i]*44.01))/1000000
}

gwp <- cbind.data.frame(gwp,gwp_rep2)

gwp_stats <- cbind(gwp_rep1,gwp_rep2)

gwp_avg <- rowMeans(gwp_stats,na.rm=TRUE)
gwp_stdev <- apply(gwp_stats,1,FUN=sd,na.rm=TRUE)

gwp <- cbind.data.frame(gwp,gwp_avg,gwp_stdev)

gwp <- gwp %>% add_column(year=c('2016','2017','2018','2016','2017','2018'))

gwp <- gwp %>% add_column(res=c('BVR','BVR','BVR','FCR','FCR','FCR'))

# Plot
ghg_graph <- ggplot(data,mapping=aes(x=year,y=Avg,fill=res_gas))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=Avg-Stdev,ymax=Avg+Stdev),width=0.4,position=position_dodge(0.9))+
  scale_fill_manual(breaks=c("BVR_ch4","BVR_co2","FCR_ch4","FCR_co2"),labels=c(expression("BVR CH"[4]),
                                                                               expression("BVR CO"[2]),
                                                                               expression("FCR CH"[4]),
                                                                               expression("FCR CO"[2])),
                    values=c('#F5793A','#ffa700','#0F2080','#91bfff'))+
  labs(fill="")+
  xlab("")+
  ylab(expression(paste("Maximum Hypo GHG (", mu,"mol L"^-1*")")))+
  theme_classic(base_size=15)

gwp_graph <- ggplot(gwp,mapping=aes(x=year,y=gwp_avg,fill=res))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=gwp_avg-gwp_stdev,ymax=gwp_avg+gwp_stdev), width=.2,position=position_dodge(.9))+
  scale_fill_manual(breaks=c("BVR","FCR"),labels=c("BVR","FCR"),
                    values=c('#F5793A','#0F2080'))+
  labs(fill="")+
  xlab("")+
  ylab(expression(paste("Maximum Hypo GWP (g L"^-1*")")))+
  theme_classic(base_size=15)

bargraphs <- ggarrange(ghg_graph,gwp_graph,legend="right",ncol=2,nrow=1)

ggsave("C:/Users/ahoun/OneDrive/Desktop/GHG/Fig_Output/Figure5_HiRes.jpg",bargraphs,width=11,height=4,
       units=c("in"))

# Saved: 1000 x 400

################ Calculate C mineralization for FCR and BVR using updated 2018 sediment SAs ###########################
# Following Hypo_GWP: but use maximum concetrations of CO2 and CH4 (similar to 'updated' GWP calculations)
# Currently in umol/L for both CO2 and CH4

# BVR
mineral_bvr_rep1 <- (((gwp$ch4_rep1[1:3]/1000)*32571.38)/35930.57)+(((gwp$co2_rep1[1:3]/1000)*32571.38)/35930.57)
mineral_bvr_rep2 <- (((gwp$ch4_rep2[1:3]/1000)*32571.38)/35930.57)+(((gwp$co2_rep2[1:3]/1000)*32571.38)/35930.57)

# FCR
mineral_fcr_rep1 <- (((gwp$ch4_rep1[4:6]/1000)*15993.24)/10966.41)+(((gwp$co2_rep1[4:6]/1000)*15993.24)/10966.41)
mineral_fcr_rep2 <- (((gwp$ch4_rep2[4:6]/1000)*15993.24)/10966.41)+(((gwp$co2_rep2[4:6]/1000)*15993.24)/10966.41)

mineral_bvr <- cbind.data.frame(mineral_bvr_rep1,mineral_bvr_rep2)
mineral_fcr <- cbind.data.frame(mineral_fcr_rep1,mineral_fcr_rep2)

mineral_fcr_avg <- rowMeans(mineral_fcr,na.rm=TRUE)
mineral_fcr_stdev <- apply(mineral_fcr,1,FUN=sd,na.rm=TRUE)

mineral_bvr_avg <- rowMeans(mineral_bvr,na.rm=TRUE)
mineral_bvr_stdev <- apply(mineral_bvr,1,FUN=sd,na.rm=TRUE)

mineral <- matrix(ncol=4,nrow=6)
row.names(mineral) <- c("bvr_16","bvr_17","bvr_18","fcr_16","fcr_17","fcr_18")
colnames(mineral) <- c("rep1","rep2","avg","stdev")

mineral <- as.data.frame(mineral)
mineral$rep1[1:3] <- mineral_bvr_rep1
mineral$rep1[4:6] <- mineral_fcr_rep1

mineral$rep2[1:3] <- mineral_bvr_rep2
mineral$rep2[4:6] <- mineral_fcr_rep2

mineral$avg[1:3] <- mineral_bvr_avg
mineral$avg[4:6] <- mineral_fcr_avg

mineral$stdev[1:3] <- mineral_bvr_stdev
mineral$stdev[4:6] <- mineral_fcr_stdev

mineral <- mineral %>% add_column(year=c('2016','2017','2018','2016','2017','2018'))

mineral <- mineral %>% add_column(res=c('BVR','BVR','BVR','FCR','FCR','FCR'))

# Plot: saved as 500 x 400
ggplot(mineral,mapping=aes(x=year,y=avg,fill=res))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=avg-stdev,ymax=avg+stdev), width=.2,position=position_dodge(.9))+
  scale_fill_manual(breaks=c("BVR","FCR"),labels=c("BVR","FCR"),
                    values=c('#F5793A','#0F2080'))+
  labs(fill="")+
  xlab("")+
  ylab(expression(paste("Hypo mineralized C (mol m"^-2*")")))+
  theme_classic(base_size=15)

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