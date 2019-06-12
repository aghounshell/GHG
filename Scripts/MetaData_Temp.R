### Script to determine when fall turnover occured for BVR and FCR in Fall 2016
### and fall 2017
### A.G. Hounshell, 27 May 2019

### Fall turn-over determined as:
### Temp at 1 m = Temp at 8 m (McClure et al., 2018)

# Load libraries
library(tidyverse)
library(zoo)

#setWD
setwd("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data")

# Set up graph theme
theme_classic = function(base_size = 12, base_family = "") {
  
  theme(
    # Specify axis options
    axis.line = element_blank(),  
    axis.text.x = element_text(size = base_size*0.8, color = "black", lineheight = 0.9),  
    axis.text.y = element_text(size = base_size*0.8, color = "black", lineheight = 0.9),  
    axis.ticks = element_line(color = "black", size  =  0.2),  
    axis.title.x = element_text(size = base_size, color = "black", margin = margin(0, 10, 0, 0)),  
    axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),  
    axis.ticks.length = unit(0.3, "lines"),   
    # Specify legend options
    legend.background = element_rect(color = NA, fill = "white"),  
    legend.key = element_rect(color = "black",  fill = "white"),  
    legend.key.size = unit(1.2, "lines"),  
    legend.key.height = NULL,  
    legend.key.width = NULL,      
    legend.text = element_text(size = base_size*0.8, color = "black"),  
    legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
    legend.position = "right",  
    legend.text.align = NULL,  
    legend.title.align = NULL,  
    legend.direction = "vertical",  
    legend.box = NULL, 
    # Specify panel options
    panel.background = element_rect(fill = "white", color  =  NA),  
    panel.border = element_rect(fill = NA, color = "black"),  
    panel.margin = unit(0.5, "lines"),   
    # Specify facetting options
    strip.background = element_rect(fill = "grey30", color = "grey10"),  
    strip.text.x = element_text(size = base_size*0.8, color = "black"),  
    strip.text.y = element_text(size = base_size*0.8, color = "black",angle = -90),  
    # Specify plot options
    plot.background = element_rect(color = "white", fill = "white"),  
    plot.title = element_text(size = base_size*1.2, color = "black"),  
    plot.margin = unit(rep(1, 4), "lines")
    
  )
  
}

# Load in all CTD data from 2013 to 2018 for reservoirs
CTD <- read_csv("YSI_Profiles.csv")

# Convert datetime
CTD$DateTime <- format(as.POSIXct(strptime(CTD$DateTime, "%m/%d/%Y %H:%M", tz = "EST")),format="%m/%d/%Y")
CTD$DateTime <- as.POSIXct(strptime(CTD$DateTime, "%m/%d/%Y", tz = "EST"))

# Separate out FCR and BVR data for site 50
FCR <- CTD %>% filter(Reservoir=="FCR"&Site==50)
BVR <- CTD %>% filter(Reservoir=="BVR"&Site==50)

# Filter dates from 2016 to 2017
FCR_date <- FCR %>% filter(FCR$DateTime>=as.Date("2016-01-01")&FCR$DateTime<=as.Date("2017-12-31"))
BVR_date <- BVR %>% filter(BVR$DateTime>=as.Date("2016-01-01")&BVR$DateTime<=as.Date("2017-12-31"))

# Plot DO by depth for FCR from 2016 to 2017: filter depths for 0.1, 5, 9
FCR_DO <- FCR_date %>% filter(Depth_m==0.1|Depth_m==5|Depth_m==9)
fcr_do_depth <- ggplot(data=FCR_DO,aes(x=DateTime,y=DO_mgL,group=Depth_m,color=as.factor(Depth_m)))+
  geom_line(size=1.5)+
  labs(x="Date",y=bquote('DO (mg '*L^-1*')'))+
  theme_classic(base_size=20)+
  theme(axis.ticks = element_line(color = "black"),axis.text.x=element_text(angle=90))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/fcr_do_depth.jpg",fcr_do_depth,width=15,height=10)

# Plot DO by depth for BVR from 2016 to 2017: filter depths for 0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
BVR_DO <- BVR_date %>% filter(Depth_m==0.1|Depth_m==1|Depth_m==2|Depth_m==3|Depth_m==4|Depth_m==5|Depth_m==6|Depth_m==7|Depth_m==8|Depth_m==9|Depth_m==10|Depth_m==11)
bvr_do_depths <- ggplot(data=BVR_DO,aes(x=DateTime,y=DO_mgL,group=Depth_m,color=as.factor(Depth_m)))+
  geom_line(size=1.5)+
  labs(x="Date",y=bquote('DO (mg '*L^-1*')'))+
  theme_classic(base_size=20)+
  theme(axis.ticks=element_line(color="black"),axis.text.x=element_text(angle=90))

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/bvr_do_depth.jpg",bvr_do_depths,width=15,height=10)

# Select FCR data from 0.8 m
FCR_0.8 <- FCR_date %>% filter(Depth_m==0.8) %>% select(Reservoir, Site, DateTime, Depth_m, Temp_C)
# Average temperatures when two replicates/day
FCR_0.8 <- aggregate(as.numeric(FCR_0.8$Temp_C), by=list(FCR_0.8$DateTime),FUN="mean")
FCR_0.8$x <- na.approx(FCR_0.8$x)

# Select BVR data from 1.0 m
BVR_1 <- BVR_date %>% filter(Depth_m==1) %>% select(Reservoir, Site, DateTime, Depth_m, Temp_C)
# Average temperatures when two replicates/day
BVR_1 <- aggregate(as.numeric(BVR_1$Temp_C),by=list(BVR_1$DateTime),FUN="mean")
BVR_1$x <- na.approx(BVR_1$x)

# Select FCR data from 8 m
FCR_8 <- FCR_date %>% filter(Depth_m==8) %>% select(Reservoir, Site, DateTime, Depth_m, Temp_C)
FCR_8 <- aggregate(as.numeric(FCR_8$Temp_C), by=list(FCR_8$DateTime),FUN="mean")
FCR_8$x <- na.approx(FCR_8$x)

# Select BVR data from 10 m
BVR_10 <- BVR_date %>% filter(Depth_m==10) %>% select(Reservoir, Site, DateTime, Depth_m, Temp_C)
BVR_10 <- aggregate(as.numeric(BVR_10$Temp_C),by=list(BVR_10$DateTime),FUN="mean")
BVR_10$x <- na.approx(BVR_10$x)

# Plot both depths for Temp
temp <- ggplot(FCR_0.8,aes(Group.1,x))+geom_line()+geom_point(size=3)+
  geom_line(data=FCR_8,colour="blue")+geom_point(data=FCR_8,colour="blue",size=3)+
  labs(x="Date",y="Temp (Deg C)")+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/FCR&BVR_Temp.jpg",temp,width=15,height=10)

temp_bvr <- ggplot(BVR_1,aes(Group.1,x))+geom_line()+geom_point(size=3)+
  geom_line(data=BVR_10,colour="blue")+geom_point(data=BVR_10,colour="blue",size=3)+
  labs(x="Date",y="Temp (Deg C)")+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR_Temp.jpg",temp_bvr,width=15,height=10)

# Select DO data at FCR from 0.1 and 8 m
FCR_0.1DO <- FCR_date %>% filter(Depth_m==0.1) %>% select(Reservoir,Site,DateTime,Depth_m,DO_mgL)
FCR_0.1DO <- aggregate(as.numeric(FCR_0.1DO$DO_mgL),by=list(FCR_0.1DO$DateTime),FUN="mean")
FCR_0.1DO$x <- na.approx(FCR_0.1DO$x)

FCR_8DO <- FCR_date %>% filter(Depth_m==8) %>% select(Reservoir,Site,DateTime,Depth_m,DO_mgL)
FCR_8DO <- aggregate(as.numeric(FCR_8DO$DO_mgL),by=list(FCR_8DO$DateTime),FUN="mean")
FCR_8DO$x <- na.approx(FCR_8DO$x)

# Select DO data from BVR at 0.1 and 10 m
BVR_0.1DO <- BVR_date %>% filter(Depth_m==0.1) %>% select(Reservoir,Site,DateTime,Depth_m,DO_mgL)
BVR_0.1DO <- aggregate(as.numeric(BVR_0.1DO$DO_mgL),by=list(BVR_0.1DO$DateTime),FUN="mean")
BVR_0.1DO$x <- na.approx(BVR_0.1DO$x)

BVR_11DO <- BVR_date %>% filter(Depth_m==10) %>% select(Reservoir,Site,DateTime,Depth_m,DO_mgL)
BVR_11DO <- aggregate(as.numeric(BVR_11DO$DO_mgL),by=list(BVR_11DO$DateTime),FUN="mean")
BVR_11DO$x <- na.approx(BVR_11DO$x)

# Plot DO data from 0.1 and 8 m
do <- ggplot(FCR_0.1DO,aes(Group.1,x))+geom_line()+geom_point(size=3)+
  geom_line(data=FCR_8DO,colour="blue")+geom_point(data=FCR_8DO,colour="blue",size=3)+
  labs(x="Date",y="DO (mg/L)")+ylim(0,17)+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/FCR&BVR_DO.jpg",do,width=15,height=10)

# Plot BVR DO data from 0.1 and 11 m
do_bvr <- ggplot(BVR_0.1DO,aes(Group.1,x))+geom_line()+geom_point(size=3)+
  geom_line(data=BVR_11DO,colour="blue")+geom_point(data=BVR_11DO,colour="blue",size=3)+
  labs(x="Date",y="DO (mg/L)")+ylim(0,17)+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR_DO.jpg",do_bvr,width=15,height=10)

# Average temperature over the epi and hypo (similar to averaging over CO2 and CH4)
FCR_hypo <- FCR_date %>% filter(Depth_m>=5) %>% group_by(DateTime) %>% summarise_all(funs(mean)) %>% arrange(DateTime) %>%
  select(DateTime, Temp_C, DO_mgL) %>% mutate(grouping = "FCR_hypo")
FCR_hypo$Temp_C <- na.approx(FCR_hypo$Temp_C)
FCR_hypo$DO_mgL <- na.approx(FCR_hypo$DO_mgL)

FCR_epi <- FCR_date %>% filter(Depth_m<5&Depth_m>0) %>% group_by(DateTime) %>% summarise_all(funs(mean)) %>% arrange(DateTime) %>%
  select(DateTime, Temp_C, DO_mgL) %>% mutate(grouping = "FCR_epi")
FCR_epi$Temp_C <- na.approx(FCR_epi$Temp_C)
FCR_epi$DO_mgL <- na.approx(FCR_epi$DO_mgL)

# Combine FCR Epi and Hypo data and export as .csv
FCR_avg <- rbind(FCR_hypo,FCR_epi,deparse.level = 1)
write_csv(FCR_avg, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/FCR_avg")

# Average temp and DO over the epi and hypo (similar to CO2 and CH4 data)
BVR_hypo <- BVR_date %>% filter(Depth_m>=5) %>% group_by(DateTime) %>% summarise_all(funs(mean)) %>% arrange(DateTime) %>% 
    select(DateTime, Temp_C, DO_mgL) %>% mutate(grouping="BVR_hypo")
BVR_hypo$Temp_C <- na.approx(BVR_hypo$Temp_C)
BVR_hypo$DO_mgL <- na.approx(BVR_hypo$DO_mgL)

BVR_epi <- BVR_date %>% filter(Depth_m<5&Depth_m>0) %>% group_by(DateTime) %>% summarise_all(funs(mean)) %>% arrange(DateTime) %>% 
    select(DateTime,Temp_C,DO_mgL) %>% mutate(grouping="BVR_epi")
BVR_epi$Temp_C <- na.approx(BVR_epi$Temp_C)
BVR_epi$DO_mgL <- na.approx(BVR_epi$DO_mgL)

# Combine BVR Epi and Hypo data and export as .csv
BVR_avg <- rbind(BVR_hypo,BVR_epi,deparse.level=1)
write_csv(BVR_avg,path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/BVR_avg")

# Plot FCR Epi vs. Hypo DO and Temp
temp_avg <- ggplot(FCR_epi,aes(DateTime,Temp_C))+geom_line()+geom_point(size=3)+
  geom_line(data=FCR_hypo,colour="blue")+geom_point(data=FCR_hypo,colour="blue",size=3)+
  labs(x="Date",y="Temperature (deg C)")+ylim(0,30)+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/FCR_TempAvg.jpg",temp_avg,width=15,height=10)

do_avg <- ggplot(FCR_epi,aes(DateTime,DO_mgL))+geom_line()+geom_point(size=3)+
  geom_line(data=FCR_hypo,colour="blue")+geom_point(data=FCR_hypo,colour="blue",size=3)+
  labs(x="Date",y="DO (mg/L)")+ylim(0,15)+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/FCR_DO_Avg.jpg",do_avg,width=15,height=10)

# Plot BVR Epi vs. Hypo DO and Temp
temp_avg_bvr <- ggplot(BVR_epi,aes(DateTime,Temp_C))+geom_line()+geom_point(size=3)+
  geom_line(data=BVR_hypo,colour="blue")+geom_point(data=BVR_hypo,colour="blue",size=3)+
  labs(x="Date",y="Temperature (deg C)")+ylim(0,30)+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR_Temp_Avg.jpg",temp_avg_bvr,width=15,height=10)

do_avg_bvr <- ggplot(BVR_epi,aes(DateTime,DO_mgL))+geom_line()+geom_point(size=3)+
  geom_line(data=BVR_hypo,colour="blue")+geom_point(data=BVR_hypo,colour="blue",size=3)+
  labs(x="Date",y="DO (mg/L)")+ylim(0,15)+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR_DO_Avg.jpg",do_avg_bvr,width=15,height=10)

# Save r file as: MetaData_Temp