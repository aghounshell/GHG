### Script to look at the feasibilty of including a variable Metalimnion for FCR GHG Data
### GHG data collected at: 0.1, 1.6, 3.8, 5, 6.2, 8, and 9 m depths consistently
### A Hounshell, 15 July 2019

### NOTE (07/15/19): Ultimately decided to designate (based on the depth locations of the GHG samples 
### collected):
#         Epi = 0.1 m + 1.6 m
#         Meta = 3.8 m + 5 m
#         Hypo = 6.2 m + 8 m + 9 m
# Will likely average Temp and DO across these same depth ranges for consistency

# Load libraries needed
pacman::p_load(tidyverse,ggplot2)

# Load in data
# FCR Thermocline data (as calculated by Lake Analyzer)
thermo <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data/FCR_Thermo.csv")
thermo$DateTime <- as.POSIXct(thermo$DateTime, "%m/%d/%Y", tz = "EST")
# FCR GHG data from 2016-2017
ghg <- read_csv("C:/Users/ahoun/Dropbox/VT_GHG/GHG_R/Data_Output/GHG_FCR_16_17.csv")

# First round to the nearest 0.1 m for the Upper and Lower Meta
thermo$SmetaB_round <- round(thermo$SmetaB,1)
thermo$SmetaT_round <- round(thermo$SmetaT,1)

# Start by plotting the location of the Meta Surface and Meta Bottom
# Then overlay sampling locations: is it feasible to have a variable Meta location?
ggplot(thermo,aes(x = DateTime, y = value, color = variable))+
  geom_line(aes(y = SmetaT_round, col="SmetaT_round"))+
  geom_line(aes(y = SmetaB_round, col="SmetaB_round"))+
  geom_hline(yintercept = 1.6)+
  geom_hline(yintercept = 3.8)+
  geom_hline(yintercept = 5)+
  geom_hline(yintercept = 6.2)+
  geom_hline(yintercept = 8)+
  theme_classic()
