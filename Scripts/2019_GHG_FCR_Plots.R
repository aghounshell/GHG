## Adapted from RPM Summer 2019
## To plot full GHGs from Summer 2019 (for MSN context)
## A Hounshell 16 Sep 19

pacman::p_load(tidyverse,
               zoo, 
               see,
               gganimate)

ghg_19 <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/GC_TS_2019_Rready_22Aug19.csv")

### FCR 
#############################################################
### Organize and Plot the 2019 FCR GHG dissolved datasets ###

### Extract just FCR ###
fcr_ghg_19 <- ghg_19 %>%
  filter(Reservoir =="FCR") %>%
  arrange(Depth_m, Date)

fcr_ghg_19$ch4_umolL <- na.approx(fcr_ghg_19$ch4_umolL)
fcr_ghg_19$co2_umolL <- na.approx(fcr_ghg_19$co2_umolL)

### Determine the mean of the two reps (bad samples have been QA/QC'd in excel) ###
fcr_ghg_19_0.1 <- fcr_ghg_19 %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_1.6 <- fcr_ghg_19 %>%filter(Depth_m == 1.6) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_3.8 <- fcr_ghg_19 %>%filter(Depth_m == 3.8) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_5 <- fcr_ghg_19 %>%filter(Depth_m == 5) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_6.2 <- fcr_ghg_19 %>%filter(Depth_m == 6.2) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_8 <- fcr_ghg_19 %>%filter(Depth_m == 8) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_9 <- fcr_ghg_19 %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_100 <- fcr_ghg_19 %>%filter(Depth_m == 100) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_200 <- fcr_ghg_19 %>%filter(Depth_m == 200) %>%group_by(Date)%>%summarise_all(funs(mean)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Calculatre the SD of the mean ###
fcr_ghg_19_0.1_sd <- fcr_ghg_19 %>%filter(Depth_m == 0.1) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_1.6_sd <- fcr_ghg_19 %>%filter(Depth_m == 1.6) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_3.8_sd <- fcr_ghg_19 %>%filter(Depth_m == 3.8) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_5_sd <- fcr_ghg_19 %>%filter(Depth_m == 5) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_6.2_sd <- fcr_ghg_19 %>%filter(Depth_m == 6.2) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_8_sd <- fcr_ghg_19 %>%filter(Depth_m == 8) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_9_sd <- fcr_ghg_19 %>%filter(Depth_m == 9) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_100_sd <- fcr_ghg_19 %>%filter(Depth_m == 100) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

fcr_ghg_19_200_sd <- fcr_ghg_19 %>%filter(Depth_m == 200) %>%group_by(Date)%>%summarise_all(funs(sd)) %>%
  select(Date, Depth_m, ch4_umolL, co2_umolL)

### Bind it all together ###
fcr_ghg_19_all <- rbind(fcr_ghg_19_0.1, fcr_ghg_19_1.6, fcr_ghg_19_3.8, fcr_ghg_19_5, fcr_ghg_19_6.2,fcr_ghg_19_8,fcr_ghg_19_9, deparse.level = 1)
fcr_ghg_19_all_sd <- rbind(fcr_ghg_19_0.1_sd, fcr_ghg_19_1.6_sd, fcr_ghg_19_3.8_sd, fcr_ghg_19_5_sd, fcr_ghg_19_6.2_sd,fcr_ghg_19_8_sd,fcr_ghg_19_9_sd, deparse.level = 1)

fcr_ghg_19_all_inf <- rbind(fcr_ghg_19_100, fcr_ghg_19_200, deparse.level = 1)
fcr_ghg_19_all_inf_sd <- rbind(fcr_ghg_19_100_sd, fcr_ghg_19_200_sd, deparse.level = 1)

### Bind it all together ###
fcr_ghg_19_all_plus_sd <- cbind(fcr_ghg_19_all, fcr_ghg_19_all_sd[,3:4], deparse.level = 1)
names(fcr_ghg_19_all_plus_sd)[5] <- "ch4_sd"
names(fcr_ghg_19_all_plus_sd)[6] <- "co2_sd"

fcr_ghg_19_all_inf_plus_sd <- cbind(fcr_ghg_19_all_inf, fcr_ghg_19_all_inf_sd[,3:4], deparse.level = 1)
names(fcr_ghg_19_all_inf_plus_sd)[5] <- "ch4_sd"
names(fcr_ghg_19_all_inf_plus_sd)[6] <- "co2_sd"



### Make the TS plots of the gases in the Water Column ### 
#CH4
ggplot(fcr_ghg_19_all_plus_sd, aes(x = Date, y = ch4_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=ch4_umolL-ch4_sd, ymax=ch4_umolL+ch4_sd), width=5,
                position=position_dodge(0))+
  ylim(0,30)+
  xlab("")+
  geom_vline(aes(xintercept = as.Date("2019-08-14")))+
  ylab(expression(umol/L~CH[4]))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))
  
#CO2
ggplot(fcr_ghg_19_all_plus_sd, aes(x = Date, y = co2_umolL, group = as.factor(Depth_m), color = as.factor(Depth_m), fill = as.factor(Depth_m)))+
  geom_line(lwd = 1.5)+
  geom_point(size = 4,pch = 21)+
  geom_errorbar(aes(ymin=co2_umolL-co2_sd, ymax=co2_umolL+co2_sd), width=5,
                position=position_dodge(0))+
  ylim(0,700)+
  xlab("")+
  ylab(expression(umol/L~CO[2]))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.title = element_blank(),
        axis.text.x = element_text(angle=90))
#############################################################

