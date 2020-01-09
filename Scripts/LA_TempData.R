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
layer <- read.csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_CTDysi_merge16to18.csv", header = T)
layer$Date <- as.POSIXct(strptime(layer$Date, "%Y-%m-%dT%H:%M:%SZ", tz = "EST"))

df.final<-data.frame()

layer1<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer1$Depth_m <- 0.1
layer2<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.5)))
layer2$Depth_m <- 0.5
layer3<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1)))
layer3$Depth_m <- 1.0
layer4<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.5)))
layer4$Depth_m <- 1.5
layer5<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2)))
layer5$Depth_m <- 2.0
layer6<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.5)))
layer6$Depth_m <- 2.5
layer7<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3)))
layer7$Depth_m <- 3.0
layer8<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.5)))
layer8$Depth_m <- 3.5
layer9<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4)))
layer9$Depth_m <- 4.0
layer10<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 4.5)))
layer10$Depth_m <- 4.5
layer11<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5)))
layer11$Depth_m <- 5.0
layer12<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
layer12$Depth_m <- 5.5
layer13<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6)))
layer13$Depth_m <- 6.0
layer14<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.5)))
layer14$Depth_m <- 6.5
layer15<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7)))
layer15$Depth_m <- 7.0
layer16<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 7.5)))
layer16$Depth_m <- 7.5
layer17<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8)))
layer17$Depth_m <- 8.0
layer18<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.5)))
layer18$Depth_m <- 8.5
layer19<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9)))
layer19$Depth_m <- 9.0
layer20<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.5)))
layer20$Depth_m <- 9.5
layer21<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10)))
layer21$Depth_m <- 10.0
layer22<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 10.5)))
layer22$Depth_m <- 10.5
layer23<-layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 11)))
layer23$Depth_m <- 11.0

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12,layer13,
                 layer14,layer15,layer16,layer17,layer18,layer19,
                 layer20,layer21,layer22,layer23)

bvr_layers <- arrange(df.final, Date)
bvr_layers$Depth_m <- round(as.numeric(bvr_layers$Depth_m), digits = .5)

bvr_layers_temp <- bvr_layers %>% select(Date,Depth_m,Temp_C) %>% group_by(Date,Depth_m) %>% summarise_each(funs(mean))

bvr_new <- bvr_layers_temp %>% spread(Depth_m,Temp_C)

bvr_new <- bvr_new[complete.cases(bvr_new),]

names(bvr_new)[1] <- "dateTime"
names(bvr_new)[2] <- "temp0.1"
names(bvr_new)[3] <- "temp0.5"
names(bvr_new)[4] <- "temp1.0"
names(bvr_new)[5] <- "temp1.5"
names(bvr_new)[6] <- "temp2.0"
names(bvr_new)[7] <- "temp2.5"
names(bvr_new)[8] <- "temp3.0"
names(bvr_new)[9] <- "temp3.5"
names(bvr_new)[10] <- "temp4.0"
names(bvr_new)[11] <- "temp4.5"
names(bvr_new)[12] <- "temp5.0"
names(bvr_new)[13] <- "temp5.5"
names(bvr_new)[14] <- "temp6.0"
names(bvr_new)[15] <- "temp6.5"
names(bvr_new)[16] <- "temp7.0"
names(bvr_new)[17] <- "temp7.5"
names(bvr_new)[18] <- "temp8.0"
names(bvr_new)[19] <- "temp8.5"
names(bvr_new)[20] <- "temp9.0"
names(bvr_new)[21] <- "temp9.5"
names(bvr_new)[22] <- "temp10.0"
names(bvr_new)[23] <- "temp10.5"
names(bvr_new)[24] <- "temp11.0"

write.table(bvr_new, "C:/Users/ahoun/OneDrive/Documents/Lake Analyzer/Source/BVR_Data/BVR.wtr", sep="\t",row.names=FALSE)
