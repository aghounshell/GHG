### Script to organize BVR CTD+YSI data for input into Lake Analyzer in Matlab
### From 2016-2018; to calculate Schmidt Stability number
### Following: https://github.com/CareyLabVT/Reservoirs/blob/master/Scripts/BVR_2016_LakeAnalyzer_Layers.R
### A Hounshell, 08 Jan 2020

# Load in necessary toolboxes:
library(dplyr)
library(tidyr)
library(lubridate)
library(rLakeAnalyzer)

# This reads all the files into the R environment
layer <- read.csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/BVR_CTDysi_merge.csv", header = T)
layer$Date <- as.POSIXct(strptime(layer$Date, "%Y-%m-%d", tz = "EST"))

df.final<-data.frame()

layer1<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
layer3<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
layer4<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
layer5<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
layer6<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
layer7<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
layer8<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
layer9<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
layer10<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
layer11<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
layer12<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
layer13<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
layer14<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
layer15<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
layer16<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
layer17<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
layer18<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
layer19<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
layer20<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.5)))
layer21<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10)))
layer22<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.5)))
layer23<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,
                 layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23)

bvr_layers <- arrange(df.final, Date)
bvr_layers$Depth_m <- round(as.numeric(bvr_layers$Depth_m), digits = .5)

for(i in 1:length(bvr_layers$Depth_m)){
  if(bvr_layers$Depth_m[i] == 0.2){
    bvr_layers$Depth_m[i] = 0.1
  }
  if(bvr_layers$Depth_m[i] == 0.3){
    bvr_layers$Depth_m[i] = 0.1
  }
  if(bvr_layers$Depth_m[i] == 10.8){
    bvr_layers$Depth_m[i] = 11
  }
  if(bvr_layers$Depth_m[i] == 10.9){
    bvr_layers$Depth_m[i] = 11
  }
}

bvr_new <- bvr_layers %>%
  select(Date,Depth_m,Temp_C)#%>%
  spread(Depth_m,Temp_C)
