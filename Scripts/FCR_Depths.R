### Script to calculate Epi, Meta, and Hypo volume weighted averaged Temp, DO, pCH4, and pCO2
### For FCR Summer 2016 and Summer 2017
### Defined Epi, Meta, and Hypo as follows:
###     Epi: 0.1 m + 1.6 m (0 - 2.6m)
###     Meta: 3.8 m + 5.0 m (2.6 - 6.5m)
###     Hypo: 6.2 m + 8.0 m + 9.0 m (6.5 - 9m)
### A Hounshell, 22Jul19
## RFile: VW_FCR

# Load libraries needed
pacman::p_load(tidyverse,ggplot2,ggpubr)

# Load in data
# FCR Volumes by depth
depth <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Vol.csv")
# FCR merged YSI and CTD casts
casts <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_CTDysi_merge.csv")
casts$Date <- as.POSIXct(strptime(casts$Date, "%Y-%m-%d", tz = "EST"))
# FCR GHG data from 2016-2017
### NEED TO FORMAT DATA DIFFERENTLY TO KEEP REPS - FOR BOTH GHG CONCENTRATIONS AND FLUX CALCS
ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_16_17.csv")
ghg$datetime <- as.POSIXct(strptime(ghg$datetime, "%Y-%m-%d", tz="EST"))

### Start with temperature as a check...(code modified from VW_BVR.R)
## Calculate volume weighted averages for the entire water column
# First: average concentrations between depth intervals (depth intervals
# used to calculate the volume in FCR)
# Also average across casts when multiple casts/day
casts_1 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=0 & Depth_m<1.3) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_1")

casts_2 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=1.3 & Depth_m<2.9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_2")

casts_3 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=2.9 & Depth_m<4.4) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_3")

casts_4 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=4.4 & Depth_m<5.9) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_4")

casts_5 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=5.9 & Depth_m<6.8) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_5")

casts_6 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=6.8 & Depth_m<8.7) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_6")

casts_7 <- casts %>% select(Date,Depth_m,Temp_C,DO_mgL) %>% filter(Depth_m>=8.7 & Depth_m<9.6) %>% group_by(Date) %>% 
  summarize_all(funs(mean)) %>% arrange(Date) %>% mutate(grouping="FCR_7")

# Re-combine depths as well
depth_2 <- rep(-99,7)
depth_2[1] <- sum(depth$`Volume (L)`[1:3])
depth_2[2] <- sum(depth$`Volume (L)`[4:8])
depth_2[3] <- sum(depth$`Volume (L)`[9:13])
depth_2[4] <- sum(depth$`Volume (L)`[14:18])
depth_2[5] <- sum(depth$`Volume (L)`[19:21])
depth_2[6] <- sum(depth$`Volume (L)`[22:27])
depth_2[7] <- sum(depth$`Volume (L)`[28:31])

# Re-combine all casts then use spread to convert to wide form
casts_merge <- rbind(casts_1,casts_2,casts_3,casts_4,casts_5,casts_6,casts_7)
casts_merge_temp <- casts_merge %>% select(Date,Temp_C,grouping) %>% arrange(Date)
casts_layers_temp <- casts_merge_temp %>% spread(grouping,Temp_C)

# Remove rows with Na values
casts_layers_temp <- na.omit(casts_layers_temp)

# Use for loop to calculate volume weighted averages for each time point
vw_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_temp[i] <- sum(casts_layers_temp$FCR_1[i]*depth_2[1],
                    casts_layers_temp$FCR_2[i]*depth_2[2],
                    casts_layers_temp$FCR_3[i]*depth_2[3],
                    casts_layers_temp$FCR_4[i]*depth_2[4],
                    casts_layers_temp$FCR_5[i]*depth_2[5],
                    casts_layers_temp$FCR_6[i]*depth_2[6],
                    casts_layers_temp$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_temp <- cbind(casts_layers_temp,vw_temp)
names(vw_temp)[1] <- "DateTime"

ggplot(vw_temp,aes(DateTime,vw_temp))+geom_line()+theme_classic()

### Calculate Epi VW Temp: defined as 0-2.6 m
vw_epi_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_epi_temp[i] <- sum(casts_layers_temp$FCR_1[i]*depth_2[1],
                        casts_layers_temp$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

### Calculate Meta VW Temp: defined as 2.6-6.5m
vw_meta_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_meta_temp[i] <- sum(casts_layers_temp$FCR_3[i]*depth_2[3],
                         casts_layers_temp$FCR_4[i]*depth_2[4],
                         casts_layers_temp$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

### Calculate Hypo VW Temp: defined as 6.5-9m
vw_hypo_temp <- rep(-99,length(casts_layers_temp$Date))
for(i in 1:length(casts_layers_temp$Date)){
  vw_hypo_temp[i] <- sum(casts_layers_temp$FCR_6[i]*depth_2[6],
                         casts_layers_temp$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_tempf <- cbind.data.frame(vw_temp$DateTime,vw_temp$vw_temp,vw_epi_temp,vw_meta_temp,vw_hypo_temp)
names(vw_tempf)[1] <- "DateTime"
names(vw_tempf)[2] <- "WaterCol"
names(vw_tempf)[3] <- "Epi"
names(vw_tempf)[4] <- "Meta"
names(vw_tempf)[5] <- "Hypo"
vw_tempf_gather <- vw_tempf %>% gather(key=depth,value=vw_temp,-DateTime)
vw_tempf_gather16 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_tempf_gather17 <- vw_tempf_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_tempf_gather$depth<-factor(vw_tempf_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))

# Plot to check: width = 1000; height = 400
temp16 <- ggplot(vw_tempf_gather16,aes(x = DateTime, y = vw_temp, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
  labs(color="")+
  xlab('2016')+
  ylab(expression('Temperature ('*degree*C*')'))+
  theme_classic(base_size = 15)

temp17 <- ggplot(vw_tempf_gather17,aes(x = DateTime, y = vw_temp, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression('Temperature ('*degree*C*')'))+
  theme_classic(base_size = 15)

ggarrange(temp16,temp17,common.legend=TRUE,legend="right")

# Export out Temp Data VW averaged by depth
write_csv(vw_tempf, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_Temp.csv")

### Calculate the same for oxygen
# Re-combine all casts then use spread to convert to wide form
casts_merge_o2 <- casts_merge %>% select(Date,DO_mgL,grouping) %>% arrange(Date)
casts_layers_o2 <- casts_merge_o2 %>% spread(grouping,DO_mgL)

# Remove rows with Na values
casts_layers_o2 <- na.omit(casts_layers_o2)

# Use for loop to calculate volume weighted averages for each time point
vw_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_o2[i] <- sum(casts_layers_o2$FCR_1[i]*depth_2[1],
                    casts_layers_o2$FCR_2[i]*depth_2[2],
                    casts_layers_o2$FCR_3[i]*depth_2[3],
                    casts_layers_o2$FCR_4[i]*depth_2[4],
                    casts_layers_o2$FCR_5[i]*depth_2[5],
                    casts_layers_o2$FCR_6[i]*depth_2[6],
                    casts_layers_o2$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_o2 <- cbind(casts_layers_o2,vw_o2)
names(vw_o2)[1] <- "DateTime"

ggplot(vw_o2,aes(DateTime,vw_o2))+geom_line()+theme_classic()

### Calculate Epi VW Temp: defined as 0-2.6 m
vw_epi_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_epi_o2[i] <- sum(casts_layers_o2$FCR_1[i]*depth_2[1],
                        casts_layers_o2$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

### Calculate Meta VW Temp: defined as 2.6-6.5m
vw_meta_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_meta_o2[i] <- sum(casts_layers_o2$FCR_3[i]*depth_2[3],
                         casts_layers_o2$FCR_4[i]*depth_2[4],
                         casts_layers_o2$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

### Calculate Hypo VW Temp: defined as 6.5-9m
vw_hypo_o2 <- rep(-99,length(casts_layers_o2$Date))
for(i in 1:length(casts_layers_o2$Date)){
  vw_hypo_o2[i] <- sum(casts_layers_o2$FCR_6[i]*depth_2[6],
                         casts_layers_o2$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_o2f <- cbind.data.frame(vw_o2$DateTime,vw_o2$vw_o2,vw_epi_o2,vw_meta_o2,vw_hypo_o2)
names(vw_o2f)[1] <- "DateTime"
names(vw_o2f)[2] <- "WaterCol"
names(vw_o2f)[3] <- "Epi"
names(vw_o2f)[4] <- "Meta"
names(vw_o2f)[5] <- "Hypo"
vw_o2f_gather <- vw_o2f %>% gather(key=depth,value=vw_o2,-DateTime)
vw_o2f_gather16 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_o2f_gather17 <- vw_o2f_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_o2f_gather$depth<-factor(vw_o2f_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))

# Plot to check
o16 <- ggplot(vw_o2f_gather16,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept = as.POSIXct("2016-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept = as.POSIXct("2016-10-09"))+ #Turnover
  geom_vline(xintercept = as.POSIXct("2016-05-30"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-06-27"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept = as.POSIXct("2016-07-25"),linetype="dotted",color="grey")+ #EM
  labs(color="")+
  xlab('2016')+
  ylab(expression("DO (mg L"^-1*")"))+
  theme_classic(base_size = 15)

o17 <- ggplot(vw_o2f_gather17,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  geom_vline(xintercept=as.POSIXct("2017-04-18"),linetype="longdash")+ #Oxygen on
  geom_vline(xintercept=as.POSIXct("2017-05-29"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-07-07"),linetype="dotted",color="grey")+ #EM
  geom_vline(xintercept=as.POSIXct("2017-10-25"))+ #Turnover
  labs(color="")+
  xlab('2017')+
  ylab(expression("DO (mg L"^-1*")"))+
  theme_classic(base_size = 15)

ggarrange(o16,o17,common.legend=TRUE,legend="right")

# Export out Temp Data VW averaged by depth
write_csv(vw_o2f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_o2.csv")

### Calculate VW GHG's
### NEED TO CALCULATE SEPARATELY FOR EACH REP; THEN AVERAGE AND SD TO REPRESENT ERROR
# Separate by depth then recombine
ghg_1 <- ghg %>% filter(depth==0.1) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_1")

ghg_2 <- ghg %>% filter(depth==1.6) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_2")

ghg_3 <- ghg %>% filter(depth==3.8) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_3")

ghg_4 <- ghg %>% filter(depth==5.0) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_4")

ghg_5 <- ghg %>% filter(depth==6.2) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_5")

ghg_6 <- ghg %>% filter(depth==8.0) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_6")

ghg_7 <- ghg %>% filter(depth==9.0) %>% group_by(datetime) %>% 
  summarize_all(funs(mean)) %>% arrange(datetime) %>% mutate(grouping="FCR_7")

# Re-combine all depths then use spread to convert to wide form
ghg_merge <- rbind(ghg_1,ghg_2,ghg_3,ghg_4,ghg_5,ghg_6,ghg_7)
ghg_merge_ch4 <- ghg_merge %>% select(datetime,ch4_umol_L,grouping) %>% arrange(datetime)
ghg_layers_ch4 <- ghg_merge_ch4 %>% spread(grouping,ch4_umol_L)


# Calculate VW pCH4 for the entire water column
vw_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:67){
  vw_ch4[i] <- sum(ghg_layers_ch4$FCR_1[i]*depth_2[1],
                   ghg_layers_ch4$FCR_2[i]*depth_2[2],
                   ghg_layers_ch4$FCR_3[i]*depth_2[3],
                   ghg_layers_ch4$FCR_4[i]*depth_2[4],
                   ghg_layers_ch4$FCR_5[i]*depth_2[5],
                   ghg_layers_ch4$FCR_6[i]*depth_2[6],
                   ghg_layers_ch4$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_ch4[39] <- sum(ghg_layers_ch4$FCR_1[i]*depth_2[1],
                  ghg_layers_ch4$FCR_2[i]*depth_2[2],
                  ghg_layers_ch4$FCR_3[i]*depth_2[3],
                  ghg_layers_ch4$FCR_4[i]*depth_2[4],
                  ghg_layers_ch4$FCR_5[i]*depth_2[5],
                  ghg_layers_ch4$FCR_6[i]*sum(depth_2[6:7]))/sum(depth_2[c(1:7)])

vw_ch4[66] <- sum(ghg_layers_ch4$FCR_1[i]*depth_2[1],
                  ghg_layers_ch4$FCR_2[i]*depth_2[2],
                  ghg_layers_ch4$FCR_3[i]*depth_2[3],
                  ghg_layers_ch4$FCR_4[i]*sum(depth_2[4:5]),
                  ghg_layers_ch4$FCR_6[i]*depth_2[6],
                  ghg_layers_ch4$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])

vw_ch4 <- cbind(ghg_layers_ch4,vw_ch4)

# Plot to check
ggplot(vw_ch4,aes(datetime,vw_ch4))+geom_line()+theme_classic()

### Calculate Epi VW pCH4: defined as 0-2.6 m
vw_epi_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:length(ghg_layers_ch4$datetime)){
  vw_epi_ch4[i] <- sum(ghg_layers_ch4$FCR_1[i]*depth_2[1],
                      ghg_layers_ch4$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

### Calculate Meta VW Temp: defined as 2.6-6.5m
vw_meta_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:length(ghg_layers_ch4$datetime)){
  vw_meta_ch4[i] <- sum(ghg_layers_ch4$FCR_3[i]*depth_2[3],
                       ghg_layers_ch4$FCR_4[i]*depth_2[4],
                       ghg_layers_ch4$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

vw_meta_ch4[66] <- sum(ghg_layers_ch4$FCR_3[i]*depth_2[3],
                       ghg_layers_ch4$FCR_4[i]*sum(depth_2[4:5]))/sum(depth_2[c(3:5)])

### Calculate Hypo VW Temp: defined as 6.5-9m
vw_hypo_ch4 <- rep(-99,length(ghg_layers_ch4$datetime))
for(i in 1:length(ghg_layers_ch4$datetime)){
  vw_hypo_ch4[i] <- sum(ghg_layers_ch4$FCR_6[i]*depth_2[6],
                       ghg_layers_ch4$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

vw_hypo_ch4[39] <- sum(ghg_layers_ch4$FCR_6[i]*sum(depth_2[6:7]))/sum(depth_2[c(6:7)])

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_ch4f <- cbind.data.frame(vw_ch4$datetime,vw_ch4$vw_ch4,vw_epi_ch4,vw_meta_ch4,vw_hypo_ch4)
names(vw_ch4f)[1] <- "DateTime"
names(vw_ch4f)[2] <- "WaterCol"
names(vw_ch4f)[3] <- "Epi"
names(vw_ch4f)[4] <- "Meta"
names(vw_ch4f)[5] <- "Hypo"
vw_ch4f_gather <- vw_ch4f %>% gather(key=depth,value=vw_ch4,-DateTime)
vw_ch4f_gather16 <- vw_ch4f_gather %>% filter(DateTime>=as.Date('2016-01-01')&DateTime<=as.Date('2016-12-31'))
vw_ch4f_gather17 <- vw_ch4f_gather %>% filter(DateTime>=as.Date('2017-01-01')&DateTime<=as.Date('2017-12-31'))
vw_ch4f_gather$depth<-factor(vw_ch4f_gather$depth, levels=c("Epi", "Meta", "Hypo", "WaterCol"))

# Plot to check
ggplot(vw_ch4f_gather16,aes(x = DateTime, y = vw_ch4, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  labs(color="")+
  xlab('2016')+
  ylab(expression("CH"[4]*" ('mu*'mol L"^-1*")"))+
  theme_classic(base_size = 15)

ch417 <- ggplot(vw_o2f_gather17,aes(x = DateTime, y = vw_o2, color = depth))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_color_manual(breaks=c("Epi","Meta","Hypo","WaterCol"),values=c('#F5793A','#A95AA1','#85C0F9','#0F2080'))+
  labs(color="")+
  xlab('2017')+
  ylab(expression("DO (mg L"^-1*")"))+
  theme_classic(base_size = 15)

ggarrange(o16,o17,common.legend=TRUE,legend="right")

# Export out Temp Data VW averaged by depth
write_csv(vw_ch4f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_ch4")

## Calculate VW pCO2 for FCR
ghg_merge_co2 <- ghg_merge %>% select(datetime,co2_umol_L,grouping) %>% arrange(datetime)
ghg_layers_co2 <- ghg_merge_co2 %>% spread(grouping,co2_umol_L)

# Calculate VW pCH4 for the entire water column
vw_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_co2[i] <- sum(ghg_layers_co2$FCR_1[i]*depth_2[1],
                   ghg_layers_co2$FCR_2[i]*depth_2[2],
                   ghg_layers_co2$FCR_3[i]*depth_2[3],
                   ghg_layers_co2$FCR_4[i]*depth_2[4],
                   ghg_layers_co2$FCR_5[i]*depth_2[5],
                   ghg_layers_co2$FCR_6[i]*depth_2[6],
                   ghg_layers_co2$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])
}

vw_co2[39] <- sum(ghg_layers_co2$FCR_1[i]*depth_2[1],
                  ghg_layers_co2$FCR_2[i]*depth_2[2],
                  ghg_layers_co2$FCR_3[i]*depth_2[3],
                  ghg_layers_co2$FCR_4[i]*depth_2[4],
                  ghg_layers_co2$FCR_5[i]*depth_2[5],
                  ghg_layers_co2$FCR_6[i]*sum(depth_2[6:7]))/sum(depth_2[c(1:7)])

vw_co2[66] <- sum(ghg_layers_co2$FCR_1[i]*depth_2[1],
                  ghg_layers_co2$FCR_2[i]*depth_2[2],
                  ghg_layers_co2$FCR_3[i]*depth_2[3],
                  ghg_layers_co2$FCR_4[i]*sum(depth_2[4:5]),
                  ghg_layers_co2$FCR_6[i]*depth_2[6],
                  ghg_layers_co2$FCR_7[i]*depth_2[7])/sum(depth_2[c(1:7)])

vw_co2 <- cbind(ghg_layers_co2,vw_co2)

# Plot to check
ggplot(vw_co2,aes(datetime,vw_co2))+geom_line()+theme_classic()

### Calculate Epi VW pCO2: defined as 0-2.6 m
vw_epi_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_epi_co2[i] <- sum(ghg_layers_co2$FCR_1[i]*depth_2[1],
                       ghg_layers_co2$FCR_2[i]*depth_2[2])/sum(depth_2[c(1:2)])
}

### Calculate Meta VW pCO2: defined as 2.6-6.5m
vw_meta_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_meta_co2[i] <- sum(ghg_layers_co2$FCR_3[i]*depth_2[3],
                        ghg_layers_co2$FCR_4[i]*depth_2[4],
                        ghg_layers_co2$FCR_5[i]*depth_2[5])/sum(depth_2[c(3:5)])
}

vw_meta_co2[66] <- sum(ghg_layers_co2$FCR_3[i]*depth_2[3],
                       ghg_layers_co2$FCR_4[i]*sum(depth_2[4:5]))/sum(depth_2[c(3:5)])

### Calculate Hypo VW Temp: defined as 6.5-9m
vw_hypo_co2 <- rep(-99,length(ghg_layers_co2$datetime))
for(i in 1:length(ghg_layers_co2$datetime)){
  vw_hypo_co2[i] <- sum(ghg_layers_co2$FCR_6[i]*depth_2[6],
                        ghg_layers_co2$FCR_7[i]*depth_2[7])/sum(depth_2[c(6:7)])
}

vw_hypo_co2[39] <- sum(ghg_layers_co2$FCR_6[i]*sum(depth_2[6:7]))/sum(depth_2[c(6:7)])

### Concatenate into a single matrix and plot Temp by depth/whole water column
vw_co2f <- cbind.data.frame(vw_co2$datetime,vw_co2$vw_co2,vw_epi_co2,vw_meta_co2,vw_hypo_co2)
names(vw_co2f)[1] <- "DateTime"
names(vw_co2f)[2] <- "WaterCol"
names(vw_co2f)[3] <- "Epi"
names(vw_co2f)[4] <- "Meta"
names(vw_co2f)[5] <- "Hypo"

# Plot to check
ggplot(vw_co2f,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  xlab('Date')+
  ylab('CO2')+
  theme_classic()

# Export out Temp Data VW averaged by depth
write_csv(vw_co2f, path = "C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_co2")
