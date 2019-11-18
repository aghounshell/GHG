## Script to format CTD and YSI data (as needed) to determine thermocline depth
## in Lake Analyzer for both BVR and FCR
## A Hounshell, 12 Jun 2019
## Updated, 18 Nov 2019
##  To include 2018 data
##  No longer using Lake Analyzer to calculate thermocline depth - defining stable Epi, Meta, and Hypo

# Load in libraries
# install.packages('pacman')
pacman::p_load(tidyverse,zoo,rLakeAnalyzer)

# Set working directory
setwd("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data")

# Load in YSI and CTD files: for all reservoirs, 2013-2018
ysi <- read_csv('YSI_Profiles.csv')
ysi$DateTime <- as.POSIXct(strptime(ysi$DateTime, "%m/%d/%Y %H:%M", tz = "EST"))
ctd <- read_csv('CTD_Casts.csv')
ctd$Date <- as.POSIXct(strptime(ctd$Date,"%m/%d/%Y %H:%M", tz="EST"))

# Select BVR and FCR for both CTD and YSI casts
ysi_bvr <- ysi %>% filter(Reservoir=="BVR"&Site==50)
ysi_fcr <- ysi %>% filter(Reservoir=="FCR"&Site==50)
ctd_bvr <- ctd %>% filter(Reservoir=="BVR"&Site==50)
ctd_fcr <- ctd %>% filter(Reservoir=="FCR"&Site==50)

# Filter dates from 2016 to 2018
ysi_bvr_date <- ysi_bvr %>% filter(ysi_bvr$DateTime>=as.Date("2016-01-01")&
                                     ysi_bvr$DateTime<=as.Date("2018-12-31"))
ysi_fcr_date <- ysi_fcr %>% filter(ysi_fcr$DateTime>=as.Date("2016-01-01")&
                                     ysi_fcr$DateTime<=as.Date("2018-12-31"))
ctd_bvr_date <- ctd_bvr %>% filter(ctd_bvr$Date>=as.Date("2016-01-01")&
                                     ctd_bvr$Date<=as.Date("2018-12-31"))
ctd_fcr_date <- ctd_fcr %>% filter(ctd_fcr$Date>=as.Date("2016-01-01")&
                                     ctd_fcr$Date<=as.Date("2018-12-31"))

# Select unique dates from both CTD and YSI casts
ysi_bvr_date_list <- as.data.frame(unique(as.Date(ysi_bvr_date$DateTime)))
names(ysi_bvr_date_list)[1] <- "dates"
ysi_bvr_date_list$ysi_bvr <- rep(-99,length(ysi_bvr_date_list$dates))

ysi_fcr_date_list <- as.data.frame(unique(as.Date(ysi_fcr_date$DateTime)))
names(ysi_fcr_date_list)[1] <- "dates"
ysi_fcr_date_list$ysi_fcr <- rep(-99,length(ysi_fcr_date_list$dates))

ctd_bvr_date_list <- as.data.frame(unique(as.Date(ctd_bvr_date$Date)))
names(ctd_bvr_date_list)[1] <- "dates"
ctd_bvr_date_list$ctd_bvr <- rep(-99,length(ctd_bvr_date_list$dates))

ctd_fcr_date_list <- as.data.frame(unique(as.Date(ctd_fcr_date$Date)))
names(ctd_fcr_date_list)[1] <- "dates"
ctd_fcr_date_list$ctd_fcr <- rep(-99,length(ctd_fcr_date_list$dates))

# Combine Unique dates list by date
bvr_dates <- merge(ysi_bvr_date_list, ctd_bvr_date_list, by="dates", all.x=TRUE, all.y=TRUE)
fcr_dates <- merge(ysi_fcr_date_list, ctd_fcr_date_list, by="dates", all.x=TRUE, all.y=TRUE)

## Need to merge the CTD and YSI data sets by date, then 'fill in' missing CTD
## data with YSI data
# For FCR
names(ysi_fcr_date)[3] <- "Date"
fcr_merge <- merge(ctd_fcr_date, ysi_fcr_date, by="Date", all.x=TRUE, all.y=TRUE)

# Select certain columns
fcr_merge <- fcr_merge %>% select(Date,Reservoir.x,Site.x,Depth_m.x,Temp_C.x,DO_mgL.x,Cond_uScm.x,
                                  Chla_ugL,Turb_NTU,pH.x,ORP_mV.x,PAR_umolm2s.x,Reservoir.y,
                                  Site.y,Depth_m.y,Temp_C.y,DO_mgL.y,Cond_uScm.y,PAR_umolm2s.y,
                                  ORP_mV.y,pH.y)

# Find where there are Na values in the CTD data: need to do it for each column
ctd_fcr_na <- is.na(fcr_merge$Reservoir.x)
fcr_merge$Reservoir.x[ctd_fcr_na] <- fcr_merge$Reservoir.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$Site.x)
fcr_merge$Site.x[ctd_fcr_na] <- fcr_merge$Site.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$Depth_m.x)
fcr_merge$Depth_m.x[ctd_fcr_na] <- fcr_merge$Depth_m.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$Temp_C.x)
fcr_merge$Temp_C.x[ctd_fcr_na] <- fcr_merge$Temp_C.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$DO_mgL.x)
fcr_merge$DO_mgL.x[ctd_fcr_na] <- fcr_merge$DO_mgL.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$Cond_uScm.x)
fcr_merge$Cond_uScm.x[ctd_fcr_na] <- fcr_merge$Cond_uScm.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$pH.x)
fcr_merge$pH.x[ctd_fcr_na] <- fcr_merge$pH.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$ORP_mV.x)
fcr_merge$ORP_mV.x[ctd_fcr_na] <- fcr_merge$ORP_mV.y[ctd_fcr_na]

ctd_fcr_na <- is.na(fcr_merge$PAR_umolm2s.x)
fcr_merge$PAR_umolm2s.x[ctd_fcr_na] <- fcr_merge$PAR_umolm2s.y[ctd_fcr_na]

fcr_all <- fcr_merge %>% select(Date,Reservoir.x,Site.x,Depth_m.x,Temp_C.x,DO_mgL.x,Cond_uScm.x,
                                Chla_ugL,Turb_NTU,pH.x,ORP_mV.x,PAR_umolm2s.x)
names(fcr_all)[2] <- "Reservoir"
names(fcr_all)[3] <- "Site"
names(fcr_all)[4] <- "Depth_m"
names(fcr_all)[5] <- "Temp_C"
names(fcr_all)[6] <- "DO_mgL"
names(fcr_all)[7] <- "Cond_uScm"
names(fcr_all)[10] <- "pH"
names(fcr_all)[11] <- "ORP_mV"
names(fcr_all)[12] <- "PAR_umolm2s"

# Check the number of unique days captured by the merged data set
fcr_date_list <- as.data.frame(unique(as.Date(fcr_all$Date)))

# Export out fcr_all as .csv
# Includes merged data from CTD and YSI casts on unique dates
# For dates with both a CTD and YSI cast, CTD casts were selected
write_csv(fcr_all, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18.csv")

### Merge data CTD and YSI datasets for BVR
names(ysi_bvr_date)[3] <- "Date"
bvr_merge <- merge(ctd_bvr_date, ysi_bvr_date, by="Date", all.x=TRUE, all.y=TRUE)

# Select certain columns
bvr_merge <- bvr_merge %>% select(Date,Reservoir.x,Site.x,Depth_m.x,Temp_C.x,DO_mgL.x,Cond_uScm.x,
                                  Chla_ugL,Turb_NTU,pH.x,ORP_mV.x,PAR_umolm2s.x,Reservoir.y,
                                  Site.y,Depth_m.y,Temp_C.y,DO_mgL.y,Cond_uScm.y,PAR_umolm2s.y,
                                  ORP_mV.y,pH.y)

# Find where there are Na values in the CTD data: need to do it for each column
ctd_bvr_na <- is.na(bvr_merge$Reservoir.x)
bvr_merge$Reservoir.x[ctd_bvr_na] <- bvr_merge$Reservoir.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$Site.x)
bvr_merge$Site.x[ctd_bvr_na] <- bvr_merge$Site.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$Depth_m.x)
bvr_merge$Depth_m.x[ctd_bvr_na] <- bvr_merge$Depth_m.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$Temp_C.x)
bvr_merge$Temp_C.x[ctd_bvr_na] <- bvr_merge$Temp_C.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$DO_mgL.x)
bvr_merge$DO_mgL.x[ctd_bvr_na] <- bvr_merge$DO_mgL.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$Cond_uScm.x)
bvr_merge$Cond_uScm.x[ctd_bvr_na] <- bvr_merge$Cond_uScm.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$pH.x)
bvr_merge$pH.x[ctd_bvr_na] <- bvr_merge$pH.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$ORP_mV.x)
bvr_merge$ORP_mV.x[ctd_bvr_na] <- bvr_merge$ORP_mV.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$PAR_umolm2s.x)
bvr_merge$PAR_umolm2s.x[ctd_bvr_na] <- bvr_merge$PAR_umolm2s.y[ctd_bvr_na]

bvr_all <- bvr_merge %>% select(Date,Reservoir.x,Site.x,Depth_m.x,Temp_C.x,DO_mgL.x,Cond_uScm.x,
                                Chla_ugL,Turb_NTU,pH.x,ORP_mV.x,PAR_umolm2s.x)
names(bvr_all)[2] <- "Reservoir"
names(bvr_all)[3] <- "Site"
names(bvr_all)[4] <- "Depth_m"
names(bvr_all)[5] <- "Temp_C"
names(bvr_all)[6] <- "DO_mgL"
names(bvr_all)[7] <- "Cond_uScm"
names(bvr_all)[10] <- "pH"
names(bvr_all)[11] <- "ORP_mV"
names(bvr_all)[12] <- "PAR_umolm2s"

# Check the number of unique days captured by the merged data set
bvr_date_list <- as.data.frame(unique(as.Date(bvr_all$Date)))

# Export out fcr_all as .csv
# Includes merged data from CTD and YSI casts on unique dates
# For dates with both a CTD and YSI cast, CTD casts were selected
write_csv(bvr_all, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18.csv")

### Format merged CTD-YSI data for use in Lake Analyzer: specifically Temp data
# FCR: select FCR temp data
fcr_temp <- fcr_all %>% select(Date,Depth_m,Temp_C)

df.final<-data.frame()

layer1<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
layer3<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
layer4<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
layer5<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
layer6<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
layer7<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
layer8<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
layer9<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
layer10<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
layer11<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
layer12<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
layer13<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
layer14<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
layer15<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
layer16<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
layer17<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
layer18<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
layer19<-fcr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))

# Define depth of layers
layer1 <- layer1 %>% mutate(depth_f="0.1")
layer2 <- layer2 %>% mutate(depth_f="0.5")
layer3 <- layer3 %>% mutate(depth_f="1")
layer4 <- layer4 %>% mutate(depth_f="1.5")
layer5 <- layer5 %>% mutate(depth_f="2")
layer6 <- layer6 %>% mutate(depth_f="2.5")
layer7 <- layer7 %>% mutate(depth_f="3")
layer8 <- layer8 %>% mutate(depth_f="3.5")
layer9 <- layer9 %>% mutate(depth_f="4")
layer10 <- layer10 %>% mutate(depth_f="4.5")
layer11 <- layer11 %>% mutate(depth_f="5")
layer12 <- layer12 %>% mutate(depth_f="5.5")
layer13 <- layer13 %>% mutate(depth_f="6")
layer14 <- layer14 %>% mutate(depth_f="6.5")
layer15 <- layer15 %>% mutate(depth_f="7")
layer16 <- layer16 %>% mutate(depth_f="7.5")
layer17 <- layer17 %>% mutate(depth_f="8")
layer18 <- layer18 %>% mutate(depth_f="8.5")
layer19 <- layer19 %>% mutate(depth_f="9")

# Combine all layers
df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19)

fcr_layers <- df.final %>% select(Date,depth_f,Temp_C)
fcr_layers <- arrange(fcr_layers, Date)

# Plot data
ggplot(fcr_layers,aes(Date,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  theme_classic()

# Plot 1.0 m layer and 8.0 m layer to verify turn-over for FCR
# Select 1.0 m and 8.0 for FCR
fcr_layers_to <- fcr_layers %>% filter(depth_f %in% c(1.0,8.0))

# Plot data
ggplot(fcr_layers_to,aes(Date,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  geom_point()+
  theme_classic()


fcr_thermo <- fcr_layers %>% spread(depth_f,Temp_C)
colnames(fcr_thermo)[-1] = paste0('wtr_',colnames(fcr_thermo)[-1])
names(fcr_thermo)[1] <- "dateTime"

fcr_thermo2 <- fcr_layers %>% spread(depth_f,Temp_C)
colnames(fcr_thermo2)[-1] = paste0('temp',colnames(fcr_thermo2)[-1])
names(fcr_thermo2)[1] <- "DateTime"

# Remove rows with NA values for all depths
fcr_thermo3 <- fcr_thermo2[rowSums(is.na(fcr_thermo2))<5,]

# Remove rows with bad data (as verified w/ code in Matlab)
fcr_thermo4 <- fcr_thermo3[-c(123:126,128,133,136,151,154,156,158,162,165,171,173,175,178,
                              182,186,188,192,195,199,201,203,204,209,211,213,216),]

# Gather data back to long form
fcr_thermo5 <- fcr_thermo4 %>% gather(depth_f,Temp_C,2:20)

# Plot to check
ggplot(fcr_thermo5,aes(DateTime,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  geom_point()+
  theme_classic()

# Now check turn-over
fcr_layers_to <- fcr_thermo5 %>% filter(depth_f %in% c('temp1','temp8'))

# Plot data
ggplot(fcr_layers_to,aes(DateTime,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  geom_point()+
  theme_classic()

# Clean-up final FCR Thermo data and export
fcr_thermo5$depth_f <- as.character(fcr_thermo5$depth_f)
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp0.1"] <- "0.1"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp0.5"] <- "0.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp1"] <- "1.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp1.5"] <- "1.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp2"] <- "2.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp2.5"] <- "2.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp3"] <- "3.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp3.5"] <- "3.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp4"] <- "4.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp4.5"] <- "4.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp5"] <- "5.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp5.5"] <- "5.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp6"] <- "6.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp6.5"] <- "6.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp7"] <- "7.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp7.5"] <- "7.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp8"] <- "8.0"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp8.5"] <- "8.5"
fcr_thermo5$depth_f[fcr_thermo5$depth_f == "temp9"] <- "9.0"

fcr_thermo5 <- fcr_thermo5 %>% sort(DateTime)

# Export 'cleaned' data
write_csv(fcr_thermo5, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18_clean.csv")

###### NO LONGER USED
# Export out .wtr for use in Lake Analyzer in Matlab
# write.table(fcr_thermo, "C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/FCR.wtr", sep="\t",row.names=FALSE)
# write.table(fcr_thermo2,"C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/FCR2.wtr", sep="\t",row.names=FALSE)
# write.table(fcr_thermo3,"C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/FCR4.wtr", sep="\t",row.names=FALSE)
# write.table(fcr_thermo4,"C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/FCR5.wtr", sep="\t",row.names=FALSE)
######

## Do the same for BVR
bvr_temp <- bvr_all %>% select(Date,Depth_m,Temp_C)

df.final<-data.frame()

layer1<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
layer3<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
layer4<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
layer5<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
layer6<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
layer7<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
layer8<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
layer9<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
layer10<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
layer11<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
layer12<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
layer13<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
layer14<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
layer15<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
layer16<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
layer17<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
layer18<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
layer19<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
layer20<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.5)))
layer21<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10)))
layer22<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.5)))
layer23<-bvr_temp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))

# Define depth of layers
layer1 <- layer1 %>% mutate(depth_f="0.1")
layer2 <- layer2 %>% mutate(depth_f="0.5")
layer3 <- layer3 %>% mutate(depth_f="1.0")
layer4 <- layer4 %>% mutate(depth_f="1.5")
layer5 <- layer5 %>% mutate(depth_f="2.0")
layer6 <- layer6 %>% mutate(depth_f="2.5")
layer7 <- layer7 %>% mutate(depth_f="3.0")
layer8 <- layer8 %>% mutate(depth_f="3.5")
layer9 <- layer9 %>% mutate(depth_f="4.0")
layer10 <- layer10 %>% mutate(depth_f="4.5")
layer11 <- layer11 %>% mutate(depth_f="5.0")
layer12 <- layer12 %>% mutate(depth_f="5.5")
layer13 <- layer13 %>% mutate(depth_f="6.0")
layer14 <- layer14 %>% mutate(depth_f="6.5")
layer15 <- layer15 %>% mutate(depth_f="7.0")
layer16 <- layer16 %>% mutate(depth_f="7.5")
layer17 <- layer17 %>% mutate(depth_f="8.0")
layer18 <- layer18 %>% mutate(depth_f="8.5")
layer19 <- layer19 %>% mutate(depth_f="9.0")
layer20 <- layer20 %>% mutate(depth_f="9.5")
layer21 <- layer21 %>% mutate(depth_f="10.0")
layer22 <- layer22 %>% mutate(depth_f="10.5")
layer23 <- layer23 %>% mutate(depth_f="11.0")

# Combine all layers
df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,layer14,layer15,layer16,layer17,layer18,layer19,layer20,layer21,layer22,layer23)

bvr_layers <- df.final %>% select(Date,depth_f,Temp_C)
bvr_layers <- arrange(bvr_layers, Date)

# Plot layers for BVR
ggplot(bvr_layers,aes(Date,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  theme_classic()

bvr_thermo <- bvr_layers %>% spread(depth_f,Temp_C)

# Change the order of the columns to be from smallest to largest depths
bvr_thermo2 <- bvr_thermo[,c(1:5,9:24,6:8)]
colnames(bvr_thermo2)[-1] = paste0('temp',colnames(bvr_thermo2)[-1])
names(bvr_thermo2)[1] <- "dateTime"

# Remove rows with NA values for all depths
bvr_thermo3 <- bvr_thermo2[rowSums(is.na(bvr_thermo2))<5,]

# Remove funky looking casts (as determined in Matlab)
bvr_thermo4 <- bvr_thermo3[-c(6,55,96),]

# Gather data back to long form
bvr_thermo5 <- bvr_thermo4 %>% gather(depth_f,Temp_C,2:24)

# Plot to check
ggplot(bvr_thermo5,aes(dateTime,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  geom_point()+
  theme_classic()

# Now check turn-over
bvr_layers_to <- bvr_thermo5 %>% filter(depth_f %in% c('temp1.0','temp8.0'))

# Plot data
ggplot(bvr_layers_to,aes(dateTime,Temp_C,group=depth_f,color=as.factor(depth_f)))+
  geom_line()+
  geom_point()+
  theme_classic()

# Clean-up final FCR Thermo data and export
bvr_thermo5$depth_f <- as.character(bvr_thermo5$depth_f)
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp0.1"] <- "0.1"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp0.5"] <- "0.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp1.0"] <- "1.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp1.5"] <- "1.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp2.0"] <- "2.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp2.5"] <- "2.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp3.0"] <- "3.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp3.5"] <- "3.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp4.0"] <- "4.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp4.5"] <- "4.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp5.0"] <- "5.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp5.5"] <- "5.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp6.0"] <- "6.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp6.5"] <- "6.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp7.0"] <- "7.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp7.5"] <- "7.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp8.0"] <- "8.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp8.5"] <- "8.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp9.0"] <- "9.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp9.5"] <- "9.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp10.0"] <- "10.0"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp10.5"] <- "10.5"
bvr_thermo5$depth_f[bvr_thermo5$depth_f == "temp11.0"] <- "11.0"

bvr_thermo5 <- bvr_thermo5 %>% sort(dateTime)

# Export 'cleaned' data
write_csv(bvr_thermo5, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18_clean.csv")

###### NO LONGER USED
# Export out .wtr for use in Lake Analyzer in Matlab
# write.table(bvr_thermo2, "C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/BVR.wtr", sep="\t",row.names=FALSE)
# write.table(bvr_thermo3, "C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/BVR3.wtr", sep="\t",row.names=FALSE)
# write.table(bvr_thermo4, "C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/BVR4.wtr", sep="\t",row.names=FALSE)
######

## Saved RFile as CTDMergeYSI.R