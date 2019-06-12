### Time Series plotting of Dissolved GHGs in FCR and BVR over the course of three years ###
### Primary Author of this script: Ryan P McClure ###
### Last updated: 070518 ###
### Adapted, A.G. Hounshell, 27 May 2019 ####

# load libraries (Only need tidyverse for now)
library(dplyr)
library(ggplot2)
library(zoo)
library(gridExtra)
library(grid)
library(tidyverse)
library(lubridate)

#setWD
setwd("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data")

# Set up graph theme
theme_classic = function(base_size = 12, base_family = "") {
  
  theme(
    # Specify axis options
    axis.line = element_blank(),  
    axis.text.x = element_text(size = base_size*0.8, color = "black", lineheight = 0.9),  
    axis.text.y = element_text(size = base_size*0.8, color = "black", lineheight = 0.9),  
    axis.ticks = element_line(color = "white", size  =  0.2),  
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

# Read in the Dissolved GHG data from '15 through '17
GHG <- read_csv("Initial Dissolved DATA 15_17.csv")

# Convert excel dates to date-time in R
GHG$datetime <- as.POSIXct(strptime(GHG$datetime, "%m/%d/%Y", tz = "EST"))

# Separate FCR and BVR data (FCR = 1; BVR = 2)
GHG_FCR <- GHG %>% filter(reservoir == 1)
GHG_BVR <- GHG %>% filter(reservoir == 2)

# Separate data by depth (0.1 m); aggregate data by date and average if two reps, use single rep when only 1 rep collected
GHG_FCR0.1co2_umol_L <- GHG_FCR %>% filter(depth == 0.1) %>% select(datetime, co2_umol_L) 
GHG_FCR0.1co2_umol_L <- aggregate(GHG_FCR0.1co2_umol_L$co2_umol_L, by=list(GHG_FCR0.1co2_umol_L$datetime), FUN = "mean")
GHG_FCR0.1co2_umol_L <- mutate(GHG_FCR0.1co2_umol_L, depth = 0.1)

GHG_FCR0.1ch4_umol_L <- GHG_FCR %>% filter(depth == 0.1) %>% select(datetime, ch4_umol_L)
GHG_FCR0.1ch4_umol_L <- aggregate(as.numeric(GHG_FCR0.1ch4_umol_L$ch4_umol_L), by=list(GHG_FCR0.1ch4_umol_L$datetime), FUN = "mean")
GHG_FCR0.1ch4_umol_L <- mutate(GHG_FCR0.1ch4_umol_L, depth = 0.1)

# Separate data by depth (1.6 m)
GHG_FCR1.6co2_umol_L <- GHG_FCR %>% filter(depth == 1.6) %>% select(datetime, co2_umol_L)
GHG_FCR1.6co2_umol_L <- aggregate(GHG_FCR1.6co2_umol_L$co2_umol_L, by=list(GHG_FCR1.6co2_umol_L$datetime), FUN = "mean")
GHG_FCR1.6co2_umol_L <- mutate(GHG_FCR1.6co2_umol_L, depth = 1.6)

GHG_FCR1.6ch4_umol_L <- GHG_FCR %>% filter(depth == 1.6) %>% select(datetime, ch4_umol_L)
GHG_FCR1.6ch4_umol_L <- aggregate(as.numeric(GHG_FCR1.6ch4_umol_L$ch4_umol_L), by=list(GHG_FCR1.6ch4_umol_L$datetime), FUN = "mean")
GHG_FCR1.6ch4_umol_L <- mutate(GHG_FCR1.6ch4_umol_L, depth = 1.6)

# Separate by depth (3.8 m)
GHG_FCR3.8co2_umol_L <- GHG_FCR %>% filter(depth == 3.8) %>% select(datetime, co2_umol_L)
GHG_FCR3.8co2_umol_L <- aggregate(GHG_FCR3.8co2_umol_L$co2_umol_L, by=list(GHG_FCR3.8co2_umol_L$datetime), FUN = "mean")
GHG_FCR3.8co2_umol_L <- mutate(GHG_FCR3.8co2_umol_L, depth = 3.8)

GHG_FCR3.8ch4_umol_L <- GHG_FCR %>% filter(depth == 3.8) %>% select(datetime, ch4_umol_L)
GHG_FCR3.8ch4_umol_L <- aggregate(as.numeric(GHG_FCR3.8ch4_umol_L$ch4_umol_L), by=list(GHG_FCR3.8ch4_umol_L$datetime), FUN = "mean")
GHG_FCR3.8ch4_umol_L <- mutate(GHG_FCR3.8ch4_umol_L, depth = 3.8)

# Separate by depth (5 m)
GHG_FCR5co2_umol_L <- GHG_FCR %>% filter(depth == 5) %>% select(datetime, co2_umol_L)
GHG_FCR5co2_umol_L <- aggregate(GHG_FCR5co2_umol_L$co2_umol_L, by=list(GHG_FCR5co2_umol_L$datetime), FUN = "mean")
GHG_FCR5co2_umol_L <- mutate(GHG_FCR5co2_umol_L, depth = 5)

GHG_FCR5ch4_umol_L <- GHG_FCR %>% filter(depth == 5) %>% select(datetime, ch4_umol_L)
GHG_FCR5ch4_umol_L <- aggregate(as.numeric(GHG_FCR5ch4_umol_L$ch4_umol_L), by=list(GHG_FCR5ch4_umol_L$datetime), FUN = "mean")
GHG_FCR5ch4_umol_L <- mutate(GHG_FCR5ch4_umol_L, depth = 5)

# Separate by depth (6.2 m)
GHG_FCR6.2co2_umol_L <- GHG_FCR %>% filter(depth == 6.2) %>% select(datetime, co2_umol_L)
GHG_FCR6.2co2_umol_L <- aggregate(GHG_FCR6.2co2_umol_L$co2_umol_L, by=list(GHG_FCR6.2co2_umol_L$datetime), FUN = "mean")
GHG_FCR6.2co2_umol_L <- mutate(GHG_FCR6.2co2_umol_L, depth = 6.2)

GHG_FCR6.2ch4_umol_L <- GHG_FCR %>% filter(depth == 6.2) %>% select(datetime, ch4_umol_L)
GHG_FCR6.2ch4_umol_L <- aggregate(as.numeric(GHG_FCR6.2ch4_umol_L$ch4_umol_L), by=list(GHG_FCR6.2ch4_umol_L$datetime), FUN = "mean")
GHG_FCR6.2ch4_umol_L <- mutate(GHG_FCR6.2ch4_umol_L, depth = 6.2)

# Separate by depth (8 m)
GHG_FCR8co2_umol_L <- GHG_FCR %>% filter(depth == 8) %>% select(datetime, co2_umol_L)
GHG_FCR8co2_umol_L <- aggregate(GHG_FCR8co2_umol_L$co2_umol_L, by=list(GHG_FCR8co2_umol_L$datetime), FUN = "mean")
GHG_FCR8co2_umol_L <- mutate(GHG_FCR8co2_umol_L, depth = 8)

GHG_FCR8ch4_umol_L <- GHG_FCR %>% filter(depth == 8) %>% select(datetime, ch4_umol_L)
GHG_FCR8ch4_umol_L <- aggregate(as.numeric(GHG_FCR8ch4_umol_L$ch4_umol_L), by=list(GHG_FCR8ch4_umol_L$datetime), FUN = "mean")
GHG_FCR8ch4_umol_L <- mutate(GHG_FCR8ch4_umol_L, depth = 8)

# Separate by depth (9 m)
GHG_FCR9co2_umol_L <- GHG_FCR %>% filter(depth == 9) %>% select(datetime, co2_umol_L)
GHG_FCR9co2_umol_L <- aggregate(GHG_FCR9co2_umol_L$co2_umol_L, by=list(GHG_FCR9co2_umol_L$datetime), FUN = "mean")
GHG_FCR9co2_umol_L <- mutate(GHG_FCR9co2_umol_L, depth = 9)

GHG_FCR9ch4_umol_L <- GHG_FCR %>% filter(depth == 9) %>% select(datetime, ch4_umol_L)
GHG_FCR9ch4_umol_L <- aggregate(as.numeric(GHG_FCR9ch4_umol_L$ch4_umol_L), by=list(GHG_FCR9ch4_umol_L$datetime), FUN = "mean")
GHG_FCR9ch4_umol_L <- mutate(GHG_FCR9ch4_umol_L, depth = 9)

# Separate inflow data (inf)
GHG_FCRinfco2_umol_L <- GHG_FCR %>% filter(depth == "Inf") %>% select(datetime, co2_umol_L)
GHG_FCRinfco2_umol_L <- aggregate(GHG_FCRinfco2_umol_L$co2_umol_L, by=list(GHG_FCRinfco2_umol_L$datetime), FUN = "mean")
GHG_FCRinfco2_umol_L <- mutate(GHG_FCRinfco2_umol_L, depth = "inf")

GHG_FCRinfch4_umol_L <- GHG_FCR %>% filter(depth == "Inf") %>% select(datetime, ch4_umol_L)
GHG_FCRinfch4_umol_L <- aggregate(as.numeric(GHG_FCRinfch4_umol_L$ch4_umol_L), by=list(GHG_FCRinfch4_umol_L$datetime), FUN = "mean")
GHG_FCRinfch4_umol_L <- mutate(GHG_FCRinfch4_umol_L, depth = "inf")

# Re-combine all depths
GHG_FCR_co2_umol_L <- rbind(GHG_FCR0.1co2_umol_L,GHG_FCR1.6co2_umol_L,GHG_FCR3.8co2_umol_L,GHG_FCR5co2_umol_L,GHG_FCR6.2co2_umol_L,GHG_FCR8co2_umol_L,GHG_FCR9co2_umol_L,GHG_FCRinfco2_umol_L, deparse.level = 1)
GHG_FCR_ch4_umol_L <- rbind(GHG_FCR0.1ch4_umol_L,GHG_FCR1.6ch4_umol_L,GHG_FCR3.8ch4_umol_L,GHG_FCR5ch4_umol_L,GHG_FCR6.2ch4_umol_L,GHG_FCR8ch4_umol_L,GHG_FCR9ch4_umol_L,GHG_FCRinfch4_umol_L, deparse.level = 1)

# Re-combine all data (Co2 and CH4)
GHG_FCR_2 <- cbind(GHG_FCR_co2_umol_L, GHG_FCR_ch4_umol_L, deparse.level = 1)
GHG_FCR_2 <- GHG_FCR_2[,c(1,2,5,6)]

names(GHG_FCR_2)[1] <- "datetime"
names(GHG_FCR_2)[2] <- "co2_umol_L"
names(GHG_FCR_2)[3] <- "ch4_umol_L"
names(GHG_FCR_2)[4] <- "depth"
GHG_FCR_2$co2_umol_L <- na.approx(GHG_FCR_2$co2_umol_L)

GHG_FCR_15_17 <- GHG_FCR_2 %>% select(datetime, depth, ch4_umol_L, co2_umol_L) %>% arrange(datetime)

# Export 'cleaned' version of GHG_FCR data (removes NaN values; calculates mean between replicates; organizes by time and depth)
write_csv(GHG_FCR_15_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_FCR_15_17")

# Export data for 2016-2018 ONLY (aka: include Summer 2016 and Summer 2017 data)
GHG_FCR_16_17 <- GHG_FCR_15_17 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_FCR_16_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_FCR_16_17")

# Select and export out data from 0.1 for FCR ONLY
GHG_FCR_0.1 <- GHG_FCR_15_17 %>% filter(depth==0.1)
write_csv(GHG_FCR_0.1, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_FCR_0.1")

# Select and export out data from 0.1 for FCR from 2016-2018 ONLY
GHG_FCR_0.1_16_17 <- GHG_FCR_0.1 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_FCR_0.1_16_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_FCR_0.1_16_17")

# Start graphing
# CH4
FCR_CH4 <- ggplot(data=GHG_FCR_15_17, mapping=aes(x=datetime, y=ch4_umol_L, group=depth, color = as.factor(depth))) +
  geom_line() + 
  geom_point(size = 3) + 
  labs(colour = "DEPTH(m)", x = "Date", y = "Dissolved CH4 (umol/L)") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/GHG_FCR_CH4.jpg",FCR_CH4,width=15,height=10)

FCR_CO2 <- ggplot(GHG_FCR_15_17, aes(x=datetime, co2_umol_L, group=depth, color = as.factor(depth))) +
  geom_line() + 
  geom_point(size = 3) +
  labs(colour = "DEPTH(m)", x = "Date", y = "Dissolved CO2 (umol/L)") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/GHG_FCR_CO2.jpg",FCR_CO2,width=15,height=10)

## Compare BVR and FCR over the entire study period
# Calculate GHG in the Hypo of FCR (> 5 m)
FCR_hypo <- GHG %>% filter(reservoir == 1) %>%
  filter(depth >= 5) %>% filter(depth!="Inf") %>% filter(depth!=5.5) %>% group_by(datetime) %>% summarise_all(funs(mean)) %>% arrange(datetime)%>%
  select(datetime, reservoir, ch4_umol_L, co2_umol_L) %>% mutate(grouping = "FCR_hypo")

# Calculate GHG in the Epi of FCR (< 5 m)
FCR_epi <- GHG %>% filter(reservoir == 1) %>%
  filter(depth < 5) %>% filter(depth > 0) %>% group_by(datetime) %>% summarise_all(funs(mean)) %>% arrange(datetime)%>%
  select(datetime, reservoir, ch4_umol_L, co2_umol_L) %>% mutate(grouping = "FCR_epi")

# Calculate GHG in the Hypo of BVR (> 5 m)
BVR_hypo <- GHG %>% filter(reservoir == 2) %>%
  filter(depth >= 5) %>% group_by(datetime) %>% summarise_all(funs(mean)) %>% arrange(datetime)%>%
  select(datetime, reservoir, ch4_umol_L, co2_umol_L) %>% mutate(grouping = "BVR_hypo")

# Calculate GHG in the Epi of BVR (< 5 m)
BVR_epi <- GHG %>% filter(reservoir == 2) %>% 
  filter(depth < 5) %>% filter(depth > 0) %>% group_by(datetime) %>% summarise_all(funs(mean)) %>% arrange(datetime) %>%
  select(datetime, reservoir, ch4_umol_L, co2_umol_L) %>% mutate(grouping = "BVR_epi")
# Remove 'extra' day in epi data table
BVR_epi <- BVR_epi[-c(37),]

GHG_data <- bind_rows(FCR_epi, FCR_hypo, BVR_epi, BVR_hypo)

GHG_data$co2_umol_L <- na.approx(GHG_data$co2_umol_L)

# Export FCR and BVR data (together)
write_csv(GHG_data, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG__BVR&FCR")

# Export FCR and BVR data (together) for 2016-2017 ONLY
GHG_data_16_17 <- GHG_data %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_data_16_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/FCR&BVR_16_17")

# Export BVR data at 0.1 m
GHG_BVR0.1co2_umol_L <- GHG_BVR %>% filter(depth == 0.1) %>% select(datetime, co2_umol_L) 
GHG_BVR0.1co2_umol_L <- aggregate(GHG_BVR0.1co2_umol_L$co2_umol_L, by=list(GHG_BVR0.1co2_umol_L$datetime), FUN = "mean")
GHG_BVR0.1co2_umol_L <- mutate(GHG_BVR0.1co2_umol_L, depth = 0.1)

GHG_BVR0.1ch4_umol_L <- GHG_BVR %>% filter(depth == 0.1) %>% select(datetime, ch4_umol_L)
GHG_BVR0.1ch4_umol_L <- aggregate(as.numeric(GHG_BVR0.1ch4_umol_L$ch4_umol_L), by=list(GHG_BVR0.1ch4_umol_L$datetime), FUN = "mean")
GHG_BVR0.1ch4_umol_L <- mutate(GHG_BVR0.1ch4_umol_L, depth = 0.1)

GHG_BVR_0.1 <- cbind(GHG_BVR0.1ch4_umol_L,GHG_BVR0.1co2_umol_L,deparse.level = 1)
GHG_BVR_0.1 <- GHG_BVR_0.1[,c(1,2,5,6)]

names(GHG_BVR_0.1)[1] <- "datetime"
names(GHG_BVR_0.1)[2] <- "ch4_umol_L"
names(GHG_BVR_0.1)[3] <- "co2_umol_L"
names(GHG_BVR_0.1)[4] <- "depth"
GHG_BVR_0.1$co2_umol_L <- na.approx(GHG_BVR_0.1$co2_umol_L)

# Export out BVR 0.1 GHG data
# Export 'cleaned' version of GHG_FCR data (removes NaN values; calculates mean between replicates; organizes by time and depth)
write_csv(GHG_BVR_0.1, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_BVR_0.1")

# Export data from BVR at 0.1 m for 2016-2017 only
GHG_BVR_0.1_16_17 <- GHG_BVR_0.1 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_BVR_0.1_16_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/BVR_0.1_16_17")

# Separate out data from remaining BVR depths (3,6,9,11 m)
# Export BVR data at 3 m
GHG_BVR3co2_umol_L <- GHG_BVR %>% filter(depth == 3) %>% select(datetime, co2_umol_L) 
GHG_BVR3co2_umol_L <- aggregate(GHG_BVR3co2_umol_L$co2_umol_L, by=list(GHG_BVR3co2_umol_L$datetime), FUN = "mean")
GHG_BVR3co2_umol_L <- mutate(GHG_BVR3co2_umol_L, depth = 3)

GHG_BVR3ch4_umol_L <- GHG_BVR %>% filter(depth == 3) %>% select(datetime, ch4_umol_L)
GHG_BVR3ch4_umol_L <- aggregate(as.numeric(GHG_BVR3ch4_umol_L$ch4_umol_L), by=list(GHG_BVR3ch4_umol_L$datetime), FUN = "mean")
GHG_BVR3ch4_umol_L <- mutate(GHG_BVR3ch4_umol_L, depth = 3)

# Export BVR data at 6 m
GHG_BVR6co2_umol_L <- GHG_BVR %>% filter(depth == 6) %>% select(datetime, co2_umol_L) 
GHG_BVR6co2_umol_L <- aggregate(GHG_BVR6co2_umol_L$co2_umol_L, by=list(GHG_BVR6co2_umol_L$datetime), FUN = "mean")
GHG_BVR6co2_umol_L <- mutate(GHG_BVR6co2_umol_L, depth = 6)

GHG_BVR6ch4_umol_L <- GHG_BVR %>% filter(depth == 6) %>% select(datetime, ch4_umol_L)
GHG_BVR6ch4_umol_L <- aggregate(as.numeric(GHG_BVR6ch4_umol_L$ch4_umol_L), by=list(GHG_BVR6ch4_umol_L$datetime), FUN = "mean")
GHG_BVR6ch4_umol_L <- mutate(GHG_BVR6ch4_umol_L, depth = 6)

# Export BVR data at 9 m
GHG_BVR9co2_umol_L <- GHG_BVR %>% filter(depth == 9) %>% select(datetime, co2_umol_L) 
GHG_BVR9co2_umol_L <- aggregate(GHG_BVR9co2_umol_L$co2_umol_L, by=list(GHG_BVR9co2_umol_L$datetime), FUN = "mean")
GHG_BVR9co2_umol_L <- mutate(GHG_BVR9co2_umol_L, depth = 9)

GHG_BVR9ch4_umol_L <- GHG_BVR %>% filter(depth == 9) %>% select(datetime, ch4_umol_L)
GHG_BVR9ch4_umol_L <- aggregate(as.numeric(GHG_BVR9ch4_umol_L$ch4_umol_L), by=list(GHG_BVR9ch4_umol_L$datetime), FUN = "mean")
GHG_BVR9ch4_umol_L <- mutate(GHG_BVR9ch4_umol_L, depth = 9)

# Export BVR data at 11 m
GHG_BVR11co2_umol_L <- GHG_BVR %>% filter(depth == 11) %>% select(datetime, co2_umol_L) 
GHG_BVR11co2_umol_L <- aggregate(GHG_BVR11co2_umol_L$co2_umol_L, by=list(GHG_BVR11co2_umol_L$datetime), FUN = "mean")
GHG_BVR11co2_umol_L <- mutate(GHG_BVR11co2_umol_L, depth = 11)

GHG_BVR11ch4_umol_L <- GHG_BVR %>% filter(depth == 11) %>% select(datetime, ch4_umol_L)
GHG_BVR11ch4_umol_L <- aggregate(as.numeric(GHG_BVR11ch4_umol_L$ch4_umol_L), by=list(GHG_BVR11ch4_umol_L$datetime), FUN = "mean")
GHG_BVR11ch4_umol_L <- mutate(GHG_BVR11ch4_umol_L, depth = 11)

# Re-combine all depths
GHG_BVR_co2_umol_L <- rbind(GHG_BVR0.1co2_umol_L,GHG_BVR3co2_umol_L,GHG_BVR6co2_umol_L,GHG_BVR9co2_umol_L,GHG_BVR11co2_umol_L, deparse.level = 1)
GHG_BVR_ch4_umol_L <- rbind(GHG_BVR0.1ch4_umol_L,GHG_BVR3ch4_umol_L,GHG_BVR6ch4_umol_L,GHG_BVR9ch4_umol_L,GHG_BVR11ch4_umol_L, deparse.level = 1)

# Re-combine all data (Co2 and CH4)
GHG_BVR_2 <- cbind(GHG_BVR_co2_umol_L, GHG_BVR_ch4_umol_L, deparse.level = 1)
GHG_BVR_2 <- GHG_BVR_2[,c(1,2,5,6)]

names(GHG_BVR_2)[1] <- "datetime"
names(GHG_BVR_2)[2] <- "co2_umol_L"
names(GHG_BVR_2)[3] <- "ch4_umol_L"
names(GHG_BVR_2)[4] <- "depth"

GHG_BVR_15_17 <- GHG_BVR_2 %>% select(datetime, depth, ch4_umol_L, co2_umol_L) %>% arrange(datetime)

# Export 'cleaned' version of GHG_FCR data (removes NaN values; calculates mean between replicates; organizes by time and depth)
write_csv(GHG_BVR_15_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_BVR_15_17")

# Export data for 2016-2018 ONLY (aka: include Summer 2016 and Summer 2017 data)
GHG_BVR_16_17 <- GHG_BVR_15_17 %>% filter(datetime>=as.Date("2016-01-01"))
write_csv(GHG_BVR_16_17, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/GHG_BVR_16_17")

# Plot BVR data by depth
bvr_ch4 <- ggplot(data=GHG_BVR_15_17, mapping=aes(x=datetime, y=ch4_umol_L, group=depth, color = as.factor(depth))) +
  geom_line() + 
  geom_point(size = 3) + 
  labs(colour = "DEPTH(m)", x = "Date", y = "Dissolved CH4 (umol/L)") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/GHG_BVR_CH4.jpg",bvr_ch4,width=15,height=10)

bvr_co2 <- ggplot(GHG_BVR_15_17, aes(x=datetime, co2_umol_L, group=depth, color = as.factor(depth))) +
  geom_line() + 
  geom_point(size = 3) +
  labs(colour = "DEPTH(m)", x = "Date", y = "Dissolved CO2 (umol/L)") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/GHG_BVR_CO2.jpg",bvr_co2,width=15,height=10)

# Graph FCR vs. BVR Epi and Hypo data
co2 <- ggplot(GHG_data_16_17, aes(x=datetime, y=co2_umol_L, group=grouping, color = as.factor(grouping))) +
  geom_line() + 
  geom_point(size = 2) +
  geom_vline(xintercept = c(as.POSIXct("2016-04-18"), as.POSIXct("2016-12-02"), as.POSIXct("2017-04-18"), as.POSIXct("2017-12-02")), linetype="dotted", 
             color = "white", size=1.5) +
  labs(colour = "DEPTH(m)", x = "", y = "Dissolved CO2 (umol/L)", title = "Dissolved carbon dioxide") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR&FCR_co2.jpg",co2,width=15,height=10)

ch4 <- ggplot(GHG_data_16_17, aes(x=datetime, y=ch4_umol_L, group=grouping, color = as.factor(grouping))) +
  geom_line() + 
  geom_point(size = 2) +
  geom_vline(xintercept = c(as.POSIXct("2016-04-18"), as.POSIXct("2016-12-02"), as.POSIXct("2017-04-18"), as.POSIXct("2017-12-02")), linetype="dotted", 
             color = "white", size=1.5) +
  labs(colour = "DEPTH (m)", x = "", y = "Dissolved CH4 (umol/L)", title = "Dissolved methane") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR&FCR_ch4.jpg",ch4,width=15,height=10)

# Plot by reservoir: BVR and FCR for both CO2 and CH4
# Select Epi and Hypo data from 2016 to 2017 only
BVR_epi_16_17 <- BVR_epi %>% filter(datetime>=as.Date("2016-01-01"))
BVR_hypo_16_17 <- BVR_hypo %>% filter(datetime>=as.Date("2016-01-01"))

ch4_bvr <- ggplot(BVR_epi_16_17,aes(datetime,ch4_umol_L))+geom_line()+geom_point(size=3)+
  geom_line(data=BVR_hypo_16_17,colour="blue")+geom_point(data=BVR_hypo_16_17,colour="blue",size=3)+
  labs(x="Date",y="CH4 (umol/L)")+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR_ch4.jpg",ch4_bvr,width=15,height=10)

co2_bvr <- ggplot(BVR_epi_16_17,aes(datetime,co2_umol_L))+geom_line()+geom_point(size=3)+
  geom_line(data=BVR_hypo_16_17,colour="blue")+geom_point(data=BVR_hypo_16_17,colour="blue",size=3)+
  labs(x="Date",y="CO2 (umol/L)")+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/BVR_co2.jpg",co2_bvr,width=15,height=10)

FCR_epi_16_17 <- FCR_epi %>% filter(datetime>=as.Date("2016-01-01"))
FCR_hypo_16_17 <- FCR_hypo %>% filter(datetime>=as.Date("2016-01-01"))

ch4_fcr <- ggplot(FCR_epi_16_17,aes(datetime,ch4_umol_L))+geom_line()+geom_point(size=3)+
  geom_line(data=FCR_hypo_16_17,colour="blue")+geom_point(data=FCR_hypo_16_17,colour="blue",size=3)+
  labs(x="Date",y="CH4 (umol/L)")+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/FCR_ch4.jpg",ch4_fcr,width=15,height=10)

co2_fcr <- ggplot(FCR_epi_16_17,aes(datetime,co2_umol_L))+geom_line()+geom_point(size=3)+
  geom_line(data=FCR_hypo_16_17,colour="blue")+geom_point(data=FCR_hypo_16_17,colour="blue",size=3)+
  labs(x="Date",y="CO2 (umol/L)")+theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/FCR_co2.jpg",co2_fcr,width=15,height=10)

# Save as Rfile: pGHG_TS