### Script to bin temperature by depths at 0.3m intervals for Lake Analyzer
### UPDATE: Did not work - would not recommend using 0.3 m intervals -- 
### not enough data
### A Hounshell, 17 Jun 2019

# Load packages
pacman::p_load(tidyverse,zoo,rLakeAnalyzer)

# Load FCR data: merged from CTD and YSI casts for summer 2016 to 2017
# Set wd
setwd("C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output")
fcr <- read_csv('FCR_CTDysi_merge.csv')

# Separate data by depth
df.final<-data.frame()

ctd1<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
ctd2<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.4)))
ctd3<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.7)))
ctd4<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
ctd5<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.3)))
ctd6<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd7<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.9)))
ctd8<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.3)))
ctd9<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.6)))
ctd10<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.9)))
ctd11<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.2)))
ctd12<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
ctd13<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
ctd14<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.1)))
ctd15<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.4)))
ctd16<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.7)))
ctd17<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
ctd18<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.3)))
ctd19<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.6)))
ctd20<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.9)))
ctd21<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
ctd22<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
ctd23<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.8)))
ctd24<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.1)))
ctd25<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.4)))
ctd26<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.7)))
ctd27<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
ctd28<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.3)))
ctd29<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.7)))
ctd30<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
ctd31<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))
ctd32<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.6)))
ctd33<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.9)))
ctd34<-fcr %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.2)))

df.final = rbind(ctd1,ctd2,ctd3,ctd4,ctd5,ctd6,ctd7,ctd8,ctd9,ctd10,ctd11,ctd12,ctd13,ctd14,ctd15,ctd16,ctd17,ctd18,ctd19,
                 ctd20,ctd21,ctd22,ctd23,ctd24,ctd25,ctd26,ctd27,ctd28,ctd29,ctd30,ctd31,ctd32,ctd33,ctd34)

fcr_layer <- arrange(df.final, Date)
fcr_layer$Depth_m <- round(as.numeric(fcr_layer$Depth_m), digits = 1)

# Re-format from long to wide
temp_FCR_LA <- fcr_layer %>% select(Date,Depth_m,Temp_C) %>% filter(!Depth_m==-0.1) 
temp_FCR_LA <- temp_FCR_LA %>% group_by(Date,Depth_m) %>% summarise_each(funs(mean))
temp_FCR_LA <- temp_FCR_LA %>% spread(Depth_m,Temp_C)

colnames(temp_FCR_LA)[-1] = paste0('temp',colnames(temp_FCR_LA)[-1])
names(temp_FCR_LA)[1] <- "dateTime"

write.table(temp_FCR_LA, "C:/Users/ahounshell/OneDrive/VT/GHG/GHG/Data_Output/FCR3.wtr", sep="\t",row.names=FALSE)

colnames(temp_FCR_LA)[-1] = paste0('wtr_',colnames(temp_FCR_LA)[-1])
names(temp_FCR_LA)[1] <- "datetime"

FCRthermo <- ts.thermo.depth(temp_FCR_LA)
