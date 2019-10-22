### FCR and BVR VW Hypo for 2016 and 2017
### Plotting graphs for GLEON Poster 2019
### A Hounshell, 22 Oct2019
# Save Rfile as GLEON_pData

## Load in FCR and BVR data created from FCR_Depths and BVR_Depths code
fcr_ch4 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_ch4_stats.csv')
fcr_ch4$datetime <- as.POSIXct(strptime(fcr_ch4$datetime, "%Y-%m-%d", tz = "EST"))
fcr_ch4_16 <- fcr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(fcr_ch4_16)[2] <- "ch4_avg"
names(fcr_ch4_16)[3] <- "ch4_std"
fcr_ch4_17 <- fcr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(fcr_ch4_17)[2] <- "ch4_avg"
names(fcr_ch4_17)[3] <- "ch4_std"

fcr_co2 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_co2_stats.csv')
fcr_co2$datetime <- as.POSIXct(strptime(fcr_co2$datetime, "%Y-%m-%d", tz = "EST"))
fcr_co2_16 <- fcr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(fcr_co2_16)[2] <- "co2_avg"
names(fcr_co2_16)[3] <- "co2_std"
fcr_co2_17 <- fcr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(fcr_co2_17)[2] <- "co2_avg"
names(fcr_co2_17)[3] <- "co2_std"

# Combine 2016 and 2017 data for FCR
fcr_16 <- cbind.data.frame(fcr_co2_16,fcr_ch4_16)
fcr_16 <- fcr_16[,-c(4)]
fcr_17 <- cbind.data.frame(fcr_co2_17,fcr_ch4_17)
fcr_17 <- fcr_17[,-c(4)]
fcr_16$datetime <- strftime(fcr_16$datetime, format = "%j")
fcr_17$datetime <- strftime(fcr_17$datetime, format = "%j")

## Do the same for BVR
bvr_ch4 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_ch4_stats.csv')
bvr_ch4$datetime <- as.POSIXct(strptime(bvr_ch4$datetime, "%Y-%m-%d", tz = "EST"))
bvr_ch4_16 <- bvr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(bvr_ch4_16)[2] <- "ch4_avg"
names(bvr_ch4_16)[3] <- "ch4_std"
bvr_ch4_17 <- bvr_ch4 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(bvr_ch4_17)[2] <- "ch4_avg"
names(bvr_ch4_17)[3] <- "ch4_std"

bvr_co2 <- read.csv('C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_co2_stats.csv')
bvr_co2$datetime <- as.POSIXct(strptime(bvr_co2$datetime, "%Y-%m-%d", tz = "EST"))
bvr_co2_16 <- bvr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
names(bvr_co2_16)[2] <- "co2_avg"
names(bvr_co2_16)[3] <- "co2_std"
bvr_co2_17 <- bvr_co2 %>% select(datetime,hypo_avg,hypo_std) %>% 
  filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))
names(bvr_co2_17)[2] <- "co2_avg"
names(bvr_co2_17)[3] <- "co2_std"

# Combine 2016 and 2017 data for FCR
bvr_16 <- cbind.data.frame(bvr_co2_16,bvr_ch4_16)
bvr_16 <- bvr_16[,-c(4)]
bvr_17 <- cbind.data.frame(bvr_co2_17,bvr_ch4_17)
bvr_17 <- bvr_17[,-c(4)]
bvr_16$datetime <- strftime(bvr_16$datetime, format = "%j")
bvr_17$datetime <- strftime(bvr_17$datetime, format = "%j")

# Plot Hypo CO2 on same graph for both BVR and FCR and both years
ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=datetime,y=co2_avg,group=1,color='FCR 2016'))+
  geom_line(data=fcr_17,mapping=aes(x=datetime,y=co2_avg,group=1,color='FCR 2017'))+
  geom_line(data=bvr_16,mapping=aes(x=datetime,y=co2_avg,group=1,color='BVR 2016'))+
  geom_line(data=bvr_17,mapping=aes(x=datetime,y=co2_avg,group=1,color='BVR 2017'))+
  theme_classic(base_size=17)

ggplot()+
  geom_line(data=fcr_16,mapping=aes(x=datetime,y=ch4_avg,group=1,color='FCR 2016'))+
  geom_line(data=fcr_17,mapping=aes(x=datetime,y=ch4_avg,group=1,color='FCR 2017'))+
  geom_line(data=bvr_16,mapping=aes(x=datetime,y=ch4_avg,group=1,color='BVR 2016'))+
  geom_line(data=bvr_17,mapping=aes(x=datetime,y=ch4_avg,group=1,color='BVR 2017'))+
  theme_classic(base_size=17)
