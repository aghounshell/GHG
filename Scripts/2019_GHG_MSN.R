# Script to look at GHG Data from FCR MSN in Aug 14-15, 2019
# Adapted from RPM, 2019_TS_GHG_plots
# A Hounshell 06 Sep 2019

# Load in libraries
pacman::p_load(tidyverse,
               zoo, 
               see,
               gganimate)

# Load in data: includes the date/time of analysis
ghg_msn <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_MSN_2019_ADate.csv")

### FCR 
#############################################################
### Organize and Plot the 2019 FCR GHG MSN dissolved dataset ###
ghg_msn$ch4_umolL <- na.approx(ghg_msn$ch4_umolL)
ghg_msn$co2_umolL <- na.approx(ghg_msn$co2_umolL)

ghg_msn$Analysis_Date <- as.POSIXct(strptime(ghg_msn$Analysis_Date, "%d-%b-%y", tz="EST"))

### Determine the mean of the two reps (bad samples have been QA/QC'd in excel) ###
ghg_0.1 <- ghg_msn %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_1.6 <- ghg_msn %>%filter(Depth_m == 1.6) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_3.8 <- ghg_msn %>%filter(Depth_m == 3.8) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_5 <- ghg_msn %>%filter(Depth_m == 5) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_6.2 <- ghg_msn %>%filter(Depth_m == 6.2) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_9 <- ghg_msn %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Calculatre the SD of the mean ###
ghg_0.1_sd <- ghg_msn %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_1.6_sd <- ghg_msn %>%filter(Depth_m == 1.6) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_3.8_sd <- ghg_msn %>%filter(Depth_m == 3.8) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_5_sd <- ghg_msn %>%filter(Depth_m == 5) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_6.2_sd <- ghg_msn %>%filter(Depth_m == 6.2) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

ghg_9_sd <- ghg_msn %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Bind it all together ###
ghg_all <- rbind(ghg_0.1, ghg_3.8, ghg_5, ghg_6.2, ghg_9, deparse.level = 1)
ghg_all_sd <- rbind(ghg_0.1_sd, ghg_3.8_sd, ghg_5_sd, ghg_6.2_sd, ghg_9_sd, deparse.level = 1)

### Bind it all together ###
ghg_all_plus_sd <- cbind(ghg_all, ghg_all_sd[,3:4], deparse.level = 1)
names(ghg_all_plus_sd)[5] <- "ch4_sd"
names(ghg_all_plus_sd)[6] <- "co2_sd"

### Also want to separate data by day of analysis: skeptical samples run on 8/21-8/21 are valid
### Determine the mean of the two reps (bad samples have been QA/QC'd in excel) ###
ghg_0.1 <- ghg_msn %>%filter(Depth_m == 0.1) %>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_1.6 <- ghg_msn %>%filter(Depth_m == 1.6)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_3.8 <- ghg_msn %>%filter(Depth_m == 3.8) %>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_5 <- ghg_msn %>%filter(Depth_m == 5) %>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_6.2 <- ghg_msn %>%filter(Depth_m == 6.2)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_9 <- ghg_msn %>%filter(Depth_m == 9)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

### Calculatre the SD of the mean ###
ghg_0.1_sd <- ghg_msn %>%filter(Depth_m == 0.1)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_1.6_sd <- ghg_msn %>%filter(Depth_m == 1.6) %>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_3.8_sd <- ghg_msn %>%filter(Depth_m == 3.8)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_5_sd <- ghg_msn %>%filter(Depth_m == 5)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_6.2_sd <- ghg_msn %>%filter(Depth_m == 6.2) %>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

ghg_9_sd <- ghg_msn %>%filter(Depth_m == 9)%>% filter(Analysis_Date<as.Date("2019-08-18")) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, Analysis_Date, ch4_umolL, co2_umolL)

### Bind it all together ###
ghg_select <- rbind(ghg_0.1, ghg_3.8, ghg_5, ghg_6.2, ghg_9, deparse.level = 1)
ghg_select_sd <- rbind(ghg_0.1_sd, ghg_3.8_sd, ghg_5_sd, ghg_6.2_sd, ghg_9_sd, deparse.level = 1)

### Bind it all together ###
ghg_select_plus_sd <- cbind(ghg_select, ghg_select_sd[,4:5], deparse.level = 1)
names(ghg_select_plus_sd)[6] <- "ch4_sd"
names(ghg_select_plus_sd)[7] <- "co2_sd"

### Make the TS plots of the gases in the Water Column ### 

#CH4: all data
ggplot(ghg_all_plus_sd, aes(x = Date, y = ch4_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  xlab("")+
  ylab(expression(umol/L~CH[4]))+
  ggtitle("All data")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))

#CH4: select data
ggplot(ghg_select_plus_sd, aes(x = Date, y = ch4_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  xlab("")+
  ylab(expression(umol/L~CH[4]))+
  ggtitle("Select data")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))

# Zoom into 0-1 umol/L CH4
ggplot(ghg_select_plus_sd, aes(x = Date, y = ch4_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  xlab("")+
  ylab(expression(umol/L~CH[4]))+
  ylim(0,1)+
  ggtitle("Select data")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))

#CO2: all data
ggplot(ghg_all_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  ylim(0,700)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  ggtitle("All data")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))

#CO2: select data
ggplot(ghg_select_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  ylim(0,700)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  ggtitle("Select data")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))

#CO2: select data; zoom in
ggplot(ghg_select_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  ylim(0,700)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  ylim(300,800)+
  ggtitle("Select data")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))
#############################################################

