### Script to create heatmaps for FCR
### Separate into Summer 2016 and Summer 2017
### For Temp and DO
### Overlay: Calculated MetaS, MetaB, Thermocline; Sampling dates; Defined Epi, Meta, Hypo 
### used for calculations
### A Hounshell, 15 July 2019; Following: FCR_HEATMAPS_2018
### Udpated, 18 Nov 2019
###   To include 2018 data
###   Include code to save as csv files as well

# Rfile: FCR_Heatmaps

# Load libraries
pacman::p_load(akima,dplyr,ggplot2,tidyverse,reshape2,gridExtra,grid,colorRamps,RColorBrewer,lubridate)

# Load in merged CTD and YSI casts for the 2016-2017 period
# Will need to divide into Summer 2016 and Summer 2017
casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_CTDysi_merge16to18.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d", tz = "EST"))
casts16 <- casts %>% filter(casts$Date>=as.Date("2016-01-01")&casts$Date<=as.Date("2016-12-31"))
casts17 <- casts %>% filter(casts$Date>=as.Date("2017-01-01")&casts$Date<=as.Date("2017-12-31"))
casts18 <- casts %>% filter(casts$Date>=as.Date("2018-01-01")&casts$Date<=as.Date("2018-12-31"))

### NO LONGER USING
# Load in thermocline and meta data as calculated using LakeAnalyzer in Matlab
# thermo <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Thermo.csv")
# thermo$DateTime <- as.POSIXct(strptime(thermo$DateTime, "%m/%d/%Y",tz="EST"))
# thermo$DateTime <- as.Date(thermo$DateTime,"%Y-%m-%d", tz="EST")
# thermo_16 <- thermo %>% filter(thermo$DateTime>=as.Date("2016-01-01")&thermo$DateTime<=as.Date("2016-12-31"))
# thermo_17 <- thermo %>% filter(thermo$DateTime>=as.Date("2017-01-01")&thermo$DateTime<=as.Date("2017-12-31"))

# Filter out depths in the CTD cast that are closest to these specified values.
df.final<-data.frame()
ctd1<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.4)))
ctd3<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.7)))
ctd4<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd5<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.3)))
ctd6<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd7<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.9)))
ctd8<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.3)))
ctd9<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.6)))
ctd10<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.9)))
ctd11<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.2)))
ctd12<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd13<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd14<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.1)))
ctd15<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.4)))
ctd16<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.7)))
ctd17<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd18<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.3)))
ctd19<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.6)))
ctd20<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.9)))
ctd21<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd22<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd23<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.8)))
ctd24<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.1)))
ctd25<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.4)))
ctd26<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.7)))
ctd27<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd28<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.3)))
ctd29<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.7)))
ctd30<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd31<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))
ctd32<-casts16 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.6)))

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31,ctd32)

# Re-arrange the data frame by date
ctd_16 <- arrange(df.final, Date)

# Round each extracted depth to the nearest 10th. 
ctd_16$Depth_m <- round(as.numeric(ctd_16$Depth_m), digits = 0.5)
ctd_16 <- ctd_16 %>% group_by(Date,Depth_m) %>% summarise_each(funs(mean))
ctd_16$DOY <- yday(ctd_16$Date)


# Select and make each CTD variable a separate dataframe
temp_16 <- select(ctd_16, Date, DOY, Depth_m, Temp_C)
temp_16 <- na.omit(temp_16)
do_16 <- select(ctd_16, Date, DOY, Depth_m, DO_mgL)
do_16 <- na.omit(do_16)

# Complete data interpolation for the heatmaps
# interative processes here

#temperature
interp_temp <- interp(x=temp_16$DOY, y = temp_16$Depth_m, z = temp_16$Temp_C,
                      xo = seq(min(temp_16$DOY), max(temp_16$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_temp <- interp2xyz(interp_temp, data.frame=T)

#dissolved oxygen
interp_do <- interp(x=do_16$DOY, y = do_16$Depth_m, z = do_16$DO_mgL,
                    xo = seq(min(do_16$DOY), max(do_16$DOY), by = .1), 
                    yo = seq(0.1, 10.2, by = 0.01),
                    extrap = F, linear = T, duplicate = "strip")
interp_do <- interp2xyz(interp_do, data.frame=T)

# Plot
interp_temp$date <- as.Date(interp_temp$x,origin="2016-01-01")
interp_do$date <- as.Date(interp_do$x,origin="2016-01-01")

# Temp
temp_interp_meta <- ggplot(interp_temp, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_line(data=thermo_16,aes(x=DateTime,y=SthermD,z=NULL),color="black")+
  geom_line(data=thermo_16,aes(x=DateTime,y=SmetaT,z=NULL),color="black",linetype="dashed")+
  geom_line(data=thermo_16,aes(x=DateTime,y=SmetaB,z=NULL),color="black",linetype="dashed")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 1.6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3.8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 5, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6.2, linetype="dashed", colour="white")+
  geom_hline(yintercept = 8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  labs(x = "2016", y = "Depth (m)", fill=expression(''*~degree*C*''))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/temp_interp_meta_2016.jpg",temp_interp_meta,width=15,height=10)

## Now do the same for 2017
# Filter out depths in the CTD cast that are closest to these specified values.
df.final<-data.frame()
ctd1<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.4)))
ctd3<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.7)))
ctd4<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd5<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.3)))
ctd6<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd7<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.9)))
ctd8<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.3)))
ctd9<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.6)))
ctd10<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.9)))
ctd11<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.2)))
ctd12<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd13<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd14<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.1)))
ctd15<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.4)))
ctd16<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.7)))
ctd17<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd18<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.3)))
ctd19<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.6)))
ctd20<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.9)))
ctd21<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd22<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd23<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.8)))
ctd24<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.1)))
ctd25<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.4)))
ctd26<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.7)))
ctd27<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd28<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.3)))
ctd29<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.7)))
ctd30<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd31<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))
ctd32<-casts17 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.6)))

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31,ctd32)

# Re-arrange the data frame by date
ctd_17 <- arrange(df.final, Date)

# Round each extracted depth to the nearest 10th. 
ctd_17$Depth_m <- round(as.numeric(ctd_17$Depth_m), digits = 0.5)
ctd_17 <- ctd_17 %>% group_by(Date,Depth_m) %>% summarise_each(funs(mean))
ctd_17$DOY <- yday(ctd_17$Date)


# Select and make each CTD variable a separate dataframe
temp_17 <- select(ctd_17, Date, DOY, Depth_m, Temp_C)
temp_17 <- na.omit(temp_17)
do_17 <- select(ctd_17, Date, DOY, Depth_m, DO_mgL)
do_17 <- na.omit(do_17)

# Complete data interpolation for the heatmaps
# interative processes here

#temperature
interp_temp_17 <- interp(x=temp_17$DOY, y = temp_17$Depth_m, z = temp_17$Temp_C,
                      xo = seq(min(temp_17$DOY), max(temp_17$DOY), by = .1), 
                      yo = seq(0.1, 9.6, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_temp_17 <- interp2xyz(interp_temp_17, data.frame=T)

#dissolved oxygen
interp_do_17 <- interp(x=do_17$DOY, y = do_17$Depth_m, z = do_17$DO_mgL,
                    xo = seq(min(do_17$DOY), max(do_17$DOY), by = .1), 
                    yo = seq(0.1, 10.2, by = 0.01),
                    extrap = F, linear = T, duplicate = "strip")
interp_do_17 <- interp2xyz(interp_do_17, data.frame=T)

# Plot
interp_temp_17$date <- as.Date(interp_temp_17$x,origin="2017-01-01")
interp_do_17$date <- as.Date(interp_do_17$x,origin="2017-01-01")

## Now do the same for 2018
# Filter out depths in the CTD cast that are closest to these specified values.
df.final<-data.frame()
ctd1<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.4)))
ctd3<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.7)))
ctd4<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd5<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.3)))
ctd6<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd7<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.9)))
ctd8<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.3)))
ctd9<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.6)))
ctd10<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.9)))
ctd11<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.2)))
ctd12<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd13<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd14<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.1)))
ctd15<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.4)))
ctd16<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.7)))
ctd17<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd18<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.3)))
ctd19<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.6)))
ctd20<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.9)))
ctd21<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd22<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd23<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.8)))
ctd24<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.1)))
ctd25<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.4)))
ctd26<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.7)))
ctd27<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd28<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.3)))
ctd29<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.7)))
ctd30<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd31<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))
ctd32<-casts18 %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.6)))

# Bind each of the data layers together.
df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31,ctd32)

# Re-arrange the data frame by date
ctd_18 <- arrange(df.final, Date)

# Round each extracted depth to the nearest 10th. 
ctd_18$Depth_m <- round(as.numeric(ctd_18$Depth_m), digits = 0.5)
ctd_18 <- ctd_18 %>% group_by(Date,Depth_m) %>% summarise_each(funs(mean))
ctd_18$DOY <- yday(ctd_18$Date)


# Select and make each CTD variable a separate dataframe
temp_18 <- select(ctd_18, Date, DOY, Depth_m, Temp_C)
temp_18 <- na.omit(temp_18)
do_18 <- select(ctd_18, Date, DOY, Depth_m, DO_mgL)
do_18 <- na.omit(do_18)

# Complete data interpolation for the heatmaps
# interative processes here

#temperature
interp_temp_18 <- interp(x=temp_18$DOY, y = temp_18$Depth_m, z = temp_18$Temp_C,
                         xo = seq(min(temp_18$DOY), max(temp_18$DOY), by = .1), 
                         yo = seq(0.1, 9.6, by = 0.01),
                         extrap = F, linear = T, duplicate = "strip")
interp_temp_18 <- interp2xyz(interp_temp_18, data.frame=T)

#dissolved oxygen
interp_do_18 <- interp(x=do_18$DOY, y = do_18$Depth_m, z = do_18$DO_mgL,
                       xo = seq(min(do_18$DOY), max(do_18$DOY), by = .1), 
                       yo = seq(0.1, 10.2, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
interp_do_18 <- interp2xyz(interp_do_18, data.frame=T)

# Plot
interp_temp_18$date <- as.Date(interp_temp_18$x,origin="2018-01-01")
interp_do_18$date <- as.Date(interp_do_18$x,origin="2018-01-01")

## Export out data: to combine with BVR data to generate one plot with both reservoirs
write_csv(interp_do, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_DOinterp_16.csv")
write_csv(interp_do_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_DOinterp_17.csv")
write_csv(interp_do_18, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_DOinterp_18.csv")

write_csv(interp_temp, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_TempInterp_16.csv")
write_csv(interp_temp_17, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_TempInterp_17.csv")
write_csv(interp_temp_18, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_TempInterp_18.csv")

# Temp w/o variable metalimnion
temp_interp_16 <- ggplot(interp_temp, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 2.6, color="black")+
  geom_hline(yintercept = 6.5, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 1.6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3.8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 5, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6.2, linetype="dashed", colour="white")+
  geom_hline(yintercept = 8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x = as.Date("2016-04-18"),y = 0),shape=25, color="black", fill="black",size=2)+ #Oxygen on
  geom_point(aes(x = as.Date("2016-10-09"),y = 0),shape=15, size=2)+ #Turnover
  geom_point(aes(x = as.Date("2016-05-30"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2016-06-27"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2016-07-25"),y = 0),shape=6, size=2)+ #EM
  labs(x = "2016", y = "Depth (m)", fill=expression(''*~degree*C*''))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic(base_size=15)

# Temp w/o variable metalimnion
# temp_interp_17
temp_interp_17 <- ggplot(interp_temp_17, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 2.6, color="black", size=1)+
  geom_hline(yintercept = 6.5, color="black", size=1)+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 1.6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3.8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 5, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6.2, linetype="dashed", colour="white")+
  geom_hline(yintercept = 8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x = as.Date("2017-04-18"),y = 0),shape=25,color="black",fill="black",size=2)+ #Oxygen on
  geom_point(aes(x = as.Date("2017-05-29"),y = 0),shape=6,size=2)+ #EM
  geom_point(aes(x = as.Date("2017-07-07"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2017-10-25"),y = 0),shape=15, size=2)+ #Turnover
  labs(x = "2017", y = "Depth (m)", fill=expression(''*~degree*C*''))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic(base_size=15)

ggarrange(temp_interp_16,temp_interp_17,common.legend=TRUE,legend="right")

# DO w/o variable metalimnion
do_interp_16 <- ggplot(interp_do, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 2.6, color="black")+
  geom_hline(yintercept = 6.5, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 1.6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3.8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 5, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6.2, linetype="dashed", colour="white")+
  geom_hline(yintercept = 8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x = as.Date("2016-04-18"),y = 0),shape=25, color="black", fill="black",size=2)+ #Oxygen on
  geom_point(aes(x = as.Date("2016-10-09"),y = 0),shape=15, size=2)+ #Turnover
  geom_point(aes(x = as.Date("2016-05-30"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2016-06-27"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2016-07-25"),y = 0),shape=6, size=2)+ #EM
  labs(x = "2016", y = "Depth (m)", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  theme_classic(base_size=15)

# DO w/o variable metalimnion
do_interp_17 <- ggplot(interp_do_17, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 2.6, color="black")+
  geom_hline(yintercept = 6.5, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 1.6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3.8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 5, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6.2, linetype="dashed", colour="white")+
  geom_hline(yintercept = 8, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x = as.Date("2017-04-18"),y = 0),shape=25,color="black",fill="black",size=2)+ #Oxygen on
  geom_point(aes(x = as.Date("2017-05-29"),y = 0),shape=6,size=2)+ #EM
  geom_point(aes(x = as.Date("2017-07-07"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2017-10-25"),y = 0),shape=15, size=2)+ #Turnover
  labs(x = "2017", y = "Depth (m)", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  theme_classic(base_size=15)

ggarrange(do_interp_16,do_interp_17,common.legend=TRUE,legend="right")
