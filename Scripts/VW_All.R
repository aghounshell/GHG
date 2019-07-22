### Script to plot calculated VW averaged pCO2 and pCH4 for FCR and BVR, Summer 2015 and 2016
### A Hounshell, 22Jul19

# Load libraries needed
pacman::p_load(tidyverse,ggplot2)

## Load in data
# BVR pCH4
bvr_ch4 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_ch4.csv")

# BVR pCO2
bvr_co2 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/BVR_VW_co2.csv")

# FCR pCH4
fcr_ch4 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_ch4.csv")

# FCR pCO2
fcr_co2 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Data_Output/FCR_VW_co2.csv")

## Simple plots for initial data visualization
# BVR ch4
bvr_ch4 <- ggplot(bvr_ch4,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  geom_point(aes(y=WaterCol,col="WaterCol"))+
  geom_point(aes(y=Epi,col="Epi"))+
  geom_point(aes(y=Meta,col="Meta"))+
  geom_point(aes(y=Hypo,col="Hypo"))+
  xlab('Date')+
  ylab('BVR CH4')+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/vw_bvr_pch4.jpg",bvr_ch4,width=15,height=10)

# BVR co2
bvr_co2 <- ggplot(bvr_co2,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  geom_point(aes(y=WaterCol,col="WaterCol"))+
  geom_point(aes(y=Epi,col="Epi"))+
  geom_point(aes(y=Meta,col="Meta"))+
  geom_point(aes(y=Hypo,col="Hypo"))+
  xlab('Date')+
  ylab('BVR CO2')+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/vw_bvr_pco2.jpg",bvr_co2,width=15,height=10)

# FCR ch4
fcr_ch4 <- ggplot(fcr_ch4,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  geom_point(aes(y=WaterCol,col="WaterCol"))+
  geom_point(aes(y=Epi,col="Epi"))+
  geom_point(aes(y=Meta,col="Meta"))+
  geom_point(aes(y=Hypo,col="Hypo"))+
  xlab('Date')+
  ylab('FCR CH4')+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/vw_fcr_pch4.jpg",fcr_ch4,width=15,height=10)

# FCR co2
fcr_co2 <- ggplot(fcr_co2,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = WaterCol, col="WaterCol"))+
  geom_line(aes(y = Epi, col="Epi"))+
  geom_line(aes(y = Meta, col="Meta"))+
  geom_line(aes(y = Hypo, col="Hypo"))+
  geom_point(aes(y=WaterCol,col="WaterCol"))+
  geom_point(aes(y=Epi,col="Epi"))+
  geom_point(aes(y=Meta,col="Meta"))+
  geom_point(aes(y=Hypo,col="Hypo"))+
  xlab('Date')+
  ylab('FCR CO2')+
  theme_classic()

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/vw_fcr_pco2.jpg",fcr_co2,width=15,height=10)
