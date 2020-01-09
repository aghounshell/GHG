### Script to bin temperature by depths at 0.3m intervals for Lake Analyzer
### UPDATE: Did not work - would not recommend using 0.3 m intervals -- 
### not enough data
### A Hounshell, 17 Jun 2019
### Updated to include 2018 data and reduce to 0.5 m increments
### A Hounshell, 09 Jan 2020

# Load packages
pacman::p_load(tidyverse,zoo,rLakeAnalyzer)

# Load FCR data: merged from CTD and YSI casts for summer 2016 to 2017
# Set wd
setwd("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output")
fcr <- read_csv('FCR_CTDysi_merge16to18.csv')

# Separate data by depth
df.final<-data.frame()

ctd1<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd1$Depth_m <- 0.1
ctd2<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
ctd2$Depth_m <- 0.5
ctd3<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
ctd3$Depth_m <- 1.0
ctd4<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
ctd4$Depth_m <- 1.5
ctd5<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.0)))
ctd5$Depth_m <- 2.0
ctd6<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
ctd6$Depth_m <- 2.5
ctd7<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.0)))
ctd7$Depth_m <- 3.0
ctd8<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd8$Depth_m <- 3.5
ctd9<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.0)))
ctd9$Depth_m <- 4.0
ctd10<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
ctd10$Depth_m <- 4.5
ctd11<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.0)))
ctd11$Depth_m <- 5.0
ctd12<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
ctd12$Depth_m <- 5.5
ctd13<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.0)))
ctd13$Depth_m <- 6.0
ctd14<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd14$Depth_m <- 6.5
ctd15<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.0)))
ctd15$Depth_m <- 7.0
ctd16<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
ctd16$Depth_m <- 7.5
ctd17<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.0)))
ctd17$Depth_m <- 8.0
ctd18<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
ctd18$Depth_m <- 8.5
ctd19<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.0)))
ctd19$Depth_m <- 9.0
ctd20<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.5)))
ctd20$Depth_m <- 9.5
ctd21<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.0)))
ctd21$Depth_m <- 10.0

df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,
                 ctd19,ctd20,ctd21)

fcr_layer <- arrange(df.final, Date)
fcr_layer$Depth_m <- round(as.numeric(fcr_layer$Depth_m), digits = 1)

# Re-format from long to wide
temp_FCR_LA <- fcr_layer %>% select(Date,Depth_m,Temp_C) %>% filter(!Depth_m==-0.1) 
temp_FCR_LA <- temp_FCR_LA %>% group_by(Date,Depth_m) %>% summarise_each(funs(mean))
temp_FCR_LA <- temp_FCR_LA %>% spread(Depth_m,Temp_C)

colnames(temp_FCR_LA)[-1] = paste0('temp',colnames(temp_FCR_LA)[-1])
names(temp_FCR_LA)[1] <- "dateTime"

temp_FCR_LA <- temp_FCR_LA[complete.cases(temp_FCR_LA),]

write.table(temp_FCR_LA, "C:/Users/ahoun/OneDrive/Documents/Lake Analyzer/Source/FCR_Data/FCR.wtr", sep="\t",row.names=FALSE)
