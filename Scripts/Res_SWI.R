### Script to plot SWI temperature for GHG MS
### 13 Feb 2020, A Hounshell
### Data exported from FCR_Depths and BVR_Depths, respectively

# Load in packages
pacman::p_load(tidyverse,ggplot2,ggpubr,matrixStats,zoo,lubridate)

# Load in FCR data
fcr_16 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_16_fcr.csv")
fcr_17 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_17_fcr.csv")
fcr_18 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_18_fcr.csv")

# Load in BVR data
bvr_16 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_16_bvr.csv")
bvr_17 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_17_bvr.csv")
bvr_18 <- read_csv("C:/Users/ahoun/OneDrive/Desktop/GHG/Data_Output/swi_18_bvr.csv")

# Calculate average difference bewteen FCR and BVR
diff_16 <- mean(fcr_16$FCR_7) - mean(bvr_16$BVR_10)
diff_17 <- mean(fcr_17$FCR_7) - mean(bvr_17$BVR_10)
diff_18 <- mean(fcr_18$FCR_7) - mean(bvr_18$BVR_10)

# Calculate difference for 2018
fcr_diff_16to18 <- mean(fcr_16$FCR_7) - mean(fcr_18$FCR_7)
fcr_diff_17to18 <- mean(fcr_17$FCR_7) - mean(fcr_18$FCR_7)
bvr_diff_16to18 <- mean(bvr_16$BVR_10) - mean(bvr_18$BVR_10)
bvr_diff_17to18 <- mean(bvr_17$BVR_10) - mean(bvr_18$BVR_10)

# Plot FCR data
fcr <- ggplot()+
  geom_line(data=fcr_16,aes(x=doy,y=FCR_7,color="2016"),size=1.1)+
  geom_point(data=fcr_16,aes(x=doy,y=FCR_7,color="2016"),size=4)+
  geom_line(data=fcr_17,aes(x=doy,y=FCR_7,color="2017"),size=1.1)+
  geom_point(data=fcr_17,aes(x=doy,y=FCR_7,color="2017"),size=4)+
  geom_line(data=fcr_18,aes(x=doy,y=FCR_7,color="2018"),size=1.1)+
  geom_point(data=fcr_18,aes(x=doy,y=FCR_7,color="2018"),size=4)+
  scale_color_manual(breaks=c("2016","2017","2018"), labels=c("FCR 2016","FCR 2017","FCR 2018"),
                     values=c('#91bfff','#2c2cff','#0F2080'))+
  geom_vline(xintercept = 280,linetype="dashed",color="#91bfff",size=1)+ #Turnover FCR 2016
  geom_vline(xintercept = 298,linetype="dashed",color="#2c2cff",size=1)+ #Turnover FCR 2017
  geom_vline(xintercept = 294,linetype="dashed",color="#0F2080",size=1)+ #Turnover FCR 2018
  xlab("Day of Year")+
  labs(color="")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  ylim(0,19)+
  xlim(0,365)+
  theme_classic(base_size=15)

bvr <- ggplot()+
  geom_line(data=bvr_16,aes(x=doy,y=BVR_10,color="2016"),size=1.1)+
  geom_point(data=bvr_16,aes(x=doy,y=BVR_10,color="2016"),size=4)+
  geom_line(data=bvr_17,aes(x=doy,y=BVR_10,color="2017"),size=1.1)+
  geom_point(data=bvr_17,aes(x=doy,y=BVR_10,color="2017"),size=4)+
  geom_line(data=bvr_18,aes(x=doy,y=BVR_10,color="2018"),size=1.1)+
  geom_point(data=bvr_18,aes(x=doy,y=BVR_10,color="2018"),size=4)+
  geom_vline(xintercept = 315,linetype="dashed",color="#ffa700",size=1)+ #Turnover BVR 2016
  geom_vline(xintercept = 311,linetype="dashed",color="#ffa07a",size=1)+ #Turnover BVR 2017
  geom_vline(xintercept = 302,linetype="dashed",color="#F5793A",size=1)+ #Turnover BVR 2018
  scale_color_manual(breaks=c("2016","2017","2018"), labels=c("BVR 2016","BVR 2017","BVR 2018"),
                     values=c('#ffa700','#ffa07a','#F5793A'))+
  xlab("Day of Year")+
  labs(color="")+
  ylab(expression('Temperature ('*~degree*C*')'))+
  ylim(0,19)+
  xlim(0,365)+
  theme_classic(base_size=15)

ggarrange(fcr,bvr,legend="right",ncol=2,nrow=1)
