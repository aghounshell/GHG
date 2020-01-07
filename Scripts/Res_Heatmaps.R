### Script to plot FCR and BVR Temp and DO heatmaps in a single figure
### For AGU poster and (hopefully) final paper
### A Hounshell, 18Nov19

# Load libraries
pacman::p_load(akima,dplyr,ggplot2,tidyverse,reshape2,gridExtra,grid,colorRamps,RColorBrewer,lubridate,ggpubr)

# Load in csv files: from BVR_Heatmaps and FCR_Heatmaps, respectively
fcr_temp_16 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_TempInterp_16.csv")
fcr_temp_16$date <- as.POSIXct(strptime(fcr_temp_16$date, "%Y-%m-%d", tz = "EST"))
fcr_temp_17 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_TempInterp_17.csv")
fcr_temp_17$date <- as.POSIXct(strptime(fcr_temp_17$date, "%Y-%m-%d", tz = "EST"))
fcr_temp_18 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_TempInterp_18.csv")
fcr_temp_18$date <- as.POSIXct(strptime(fcr_temp_18$date, "%Y-%m-%d", tz = "EST"))

fcr_do_16 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_DOinterp_16.csv")
fcr_do_16$date <- as.POSIXct(strptime(fcr_do_16$date, "%Y-%m-%d", tz = "EST"))
fcr_do_17 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_DOinterp_17.csv")
fcr_do_17$date <- as.POSIXct(strptime(fcr_do_17$date, "%Y-%m-%d", tz = "EST"))
fcr_do_18 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/FCR_DOinterp_18.csv")
fcr_do_18$date <- as.POSIXct(strptime(fcr_do_18$date, "%Y-%m-%d", tz = "EST"))

bvr_temp_16 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_TempInterp_16.csv")
bvr_temp_16$date <- as.POSIXct(strptime(bvr_temp_16$date, "%Y-%m-%d", tz = "EST"))
bvr_temp_17 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_TempInterp_17.csv")
bvr_temp_17$date <- as.POSIXct(strptime(bvr_temp_17$date, "%Y-%m-%d", tz = "EST"))
bvr_temp_18 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_TempInterp_18.csv")
bvr_temp_18$date <- as.POSIXct(strptime(bvr_temp_18$date, "%Y-%m-%d", tz = "EST"))

bvr_do_16 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_DOinterp_16.csv")
bvr_do_16$date <- as.POSIXct(strptime(bvr_do_16$date, "%Y-%m-%d", tz = "EST"))
bvr_do_17 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_DOinterp_17.csv")
bvr_do_17$date <- as.POSIXct(strptime(bvr_do_17$date, "%Y-%m-%d", tz = "EST"))
bvr_do_18 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/BVR_DOinterp_18.csv")
bvr_do_18$date <- as.POSIXct(strptime(bvr_do_18$date, "%Y-%m-%d", tz = "EST"))

###############DO Plotting##################

fcr_do_16$date <- as.Date(fcr_do_16$x,origin="2016-01-01")
fcr_do_17$date <- as.Date(fcr_do_17$x,origin="2017-01-01")
fcr_do_18$date <- as.Date(fcr_do_18$x,origin="2018-01-01")

fcr_temp_16$date <- as.Date(fcr_temp_16$x,origin="2016-01-01")
fcr_temp_17$date <- as.Date(fcr_temp_17$x,origin="2017-01-01")
fcr_temp_18$date <- as.Date(fcr_temp_18$x,origin="2018-01-01")

bvr_do_16$date <- as.Date(bvr_do_16$x,origin="2016-01-01")
bvr_do_17$date <- as.Date(bvr_do_17$x,origin="2017-01-01")
bvr_do_18$date <- as.Date(bvr_do_18$x,origin="2018-01-01")

bvr_temp_16$date <- as.Date(bvr_temp_16$x,origin="2016-01-01")
bvr_temp_17$date <- as.Date(bvr_temp_17$x,origin="2017-01-01")
bvr_temp_18$date <- as.Date(bvr_temp_18$x,origin="2018-01-01")

# FCR DO 16
fcr_do_16_plot <- ggplot(fcr_do_16, aes(x=date, y=y))+
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
  geom_point(aes(x = as.Date("2016-10-07"),y = 0),shape=15, size=2)+ #Turnover
  geom_point(aes(x = as.Date("2016-05-30"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2016-06-27"),y = 0),shape=6, size=2)+ #EM
  geom_point(aes(x = as.Date("2016-07-25"),y = 0),shape=6, size=2)+ #EM
  labs(x = "2016", y = "Depth (m)", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  xlim(as.Date("2016-04-01"),as.Date("2016-11-30"))+
  theme_classic(base_size=15)

# FCR DO 17
fcr_do_17_plot <- ggplot(fcr_do_17, aes(x=date, y=y))+
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
  geom_point(aes(x = as.Date("2017-10-30"),y = 0),shape=15, size=2)+ #Turnover
  labs(x = "2017", y = "", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  xlim(as.Date("2017-04-01"),as.Date("2017-11-30"))+
  theme_classic(base_size=15)

# FCR DO 18
fcr_do_18_plot <- ggplot(fcr_do_18, aes(x=date, y=y))+
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
  geom_point(aes(x = as.Date("2018-04-23"),y = 0),shape=25,color="black",fill="black",size=2)+ #Oxygen on
  geom_point(aes(x = as.Date("2018-07-30"),y = 0),shape=17,size=2)+ #SSS Off
  geom_point(aes(x = as.Date("2018-10-21"),y = 0),shape=15, size=2)+ #Turnover
  labs(x = "2018", y = "", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  xlim(as.Date("2018-04-01"),as.Date("2018-11-30"))+
  theme_classic(base_size=15)

# BVR DO 16
bvr_do_16_plot <- ggplot(bvr_do_16, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 4, color="black")+
  geom_hline(yintercept = 7, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x = as.Date("2016-11-11"),y=0),shape=15,size=2)+ #Turnover
  labs(x = "2016", y = "Depth (m)", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  xlim(as.Date("2016-04-01"),as.Date("2016-11-30"))+
  theme_classic(base_size=15)

# BVR DO 17
bvr_do_17_plot <- ggplot(bvr_do_17, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 4, color="black")+
  geom_hline(yintercept = 7, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x=as.Date("2017-11-07"),y=0),shape=15,size=2)+ #Turnover
  labs(x = "2017", y = "", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  xlim(as.Date("2017-04-01"),as.Date("2017-11-30"))+
  theme_classic(base_size=15)

# BVR DO 18
bvr_do_18_plot <- ggplot(bvr_do_18, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 4, color="black")+
  geom_hline(yintercept = 7, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x=as.Date("2018-10-29"),y=0),shape=15,size=2)+ #Turnover
  labs(x = "2018", y = "", fill=expression("DO (mg L"^-1*")"))+
  scale_fill_gradientn(colours = rev(blue2green2red(60)), na.value="gray")+
  xlim(as.Date("2018-04-01"),as.Date("2018-11-30"))+
  theme_classic(base_size=15)

do_heatmap <- ggarrange(fcr_do_16_plot, fcr_do_17_plot, fcr_do_18_plot, bvr_do_16_plot, 
                        bvr_do_17_plot, bvr_do_18_plot, common.legend=TRUE,legend="right", 
                        ncol=3, nrow=2)

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/DO_heatmap_2.jpg",do_heatmap,width=12,height=6,
       units=c("in"))

##################TEMP Plotting########################

# FCR Temp 16
fcr_temp_16_plot <- ggplot(fcr_temp_16, aes(x=date, y=y))+
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
  xlim(as.Date("2016-04-01"),as.Date("2016-11-30"))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic(base_size=15)

# FCR Temp 17
fcr_temp_17_plot <- ggplot(fcr_temp_17, aes(x=date, y=y))+
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
  labs(x = "2017", y = "", fill=expression(''*~degree*C*''))+
  xlim(as.Date("2017-04-01"),as.Date("2017-11-30"))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic(base_size=15)

# FCR Temp 18
fcr_temp_18_plot <- ggplot(fcr_temp_18, aes(x=date, y=y))+
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
  geom_point(aes(x = as.Date("2018-04-23"),y = 0),shape=25,color="black",fill="black",size=2)+ #Oxygen on
  geom_point(aes(x = as.Date("2018-07-30"),y = 0),shape=17,size=2)+ #SSS Off
  geom_point(aes(x = as.Date("2018-10-21"),y = 0),shape=15, size=2)+ #Turnover
  labs(x = "2018", y = "", fill=expression(''*~degree*C*''))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  xlim(as.Date("2018-04-01"),as.Date("2018-11-30"))+
  theme_classic(base_size=15)

# BVR Temp 16
bvr_temp_16_plot <- ggplot(bvr_temp_16, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 4, color="black")+
  geom_hline(yintercept = 7, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x = as.Date("2016-11-11"),y=0),shape=15,size=2)+ #Turnover
  labs(x = "2016", y = "Depth (m)", fill=expression(''*~degree*C*''))+
  xlim(as.Date("2016-04-01"),as.Date("2016-11-30"))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic(base_size=15)

# BVR Temp 17
bvr_temp_17_plot <- ggplot(bvr_temp_17, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 4, color="black")+
  geom_hline(yintercept = 7, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x=as.Date("2017-11-07"),y=0),shape=15,size=2)+ #Turnover
  labs(x = "2017", y = "", fill=expression(''*~degree*C*''))+
  xlim(as.Date("2017-04-01"),as.Date("2017-11-30"))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  theme_classic(base_size=15)

# BVR Temp 18
bvr_temp_18_plot <- ggplot(bvr_temp_18, aes(x=date, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse()+
  geom_hline(yintercept = 4, color="black")+
  geom_hline(yintercept = 7, color="black")+
  geom_hline(yintercept = 0.1, linetype="dashed", colour="white")+
  geom_hline(yintercept = 3, linetype="dashed", colour="white")+
  geom_hline(yintercept = 6, linetype="dashed", colour="white")+
  geom_hline(yintercept = 9, linetype="dashed", colour="white")+
  geom_point(aes(x=as.Date("2018-10-29"),y=0),shape=15,size=2)+ #Turnover
  labs(x = "2018", y = "", fill=expression(''*~degree*C*''))+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  xlim(as.Date("2018-04-01"),as.Date("2018-11-30"))+
  theme_classic(base_size=15)

temp_heatmap <- ggarrange(fcr_temp_16_plot, fcr_temp_17_plot, fcr_temp_18_plot, bvr_temp_16_plot, 
                        bvr_temp_17_plot, bvr_temp_18_plot, common.legend=TRUE,legend="right", 
                        ncol=3, nrow=2)

ggsave("C:/Users/ahoun/Dropbox/VT_GHG/GHG/Fig_Output/Temp_heatmap_2.jpg",temp_heatmap,width=12,height=6,
       units=c("in"))
