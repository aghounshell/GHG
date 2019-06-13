### Script to calculate relationship between wind speed at ROA and wind speed at FCR
### Met station
### A Hounshell, 07 Jun 2019

# Load libraries
library(tidyverse)

#setWD
setwd("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data")

# Load in ROA met data (from NOAA NCDC; 'standard' units)
# January 1, 2015 to December 31, 2017; loaded as daily averaged values
roa <- read_csv("DailyWind_ROA.csv")
roa$DATE <- as.POSIXct(strptime(roa$DATE, "%m/%d/%Y", tz = "EST"))

# Convert ROA wind data from mph to m/s
roa$wind_ms <- roa$AWND_mph*0.44704

# Separate wind data
roa_wind <- roa %>% filter(DATE>=as.Date("2015-07-07")) %>% select(DATE,wind_ms)
roa_wind <- roa_wind %>% mutate(grouping="roa")
names(roa_wind)[1] <- "Date"
names(roa_wind)[2] <- "Wind_ms"

# Load in FCR Met data
# July 7, 2015 to December 31, 2017
met_2015 <- read_csv("Met_Data_2015-2016.csv")
met_2017 <- read_csv("Met_Data_2017.csv")

# Average wind data over each day for each set of years: 2015-2016 and then 2017
met_avg_2015 <- met_2015 %>% select(TIMESTAMP,WS_ms_Avg)
met_avg_2015$TIMESTAMP <- as.POSIXct(strptime(met_avg_2015$TIMESTAMP, "%m/%d/%Y", tz = "EST"))
met_avg_2015 <- aggregate(met_avg_2015$WS_ms_Avg, by=list(met_avg_2015$TIMESTAMP), FUN = "mean")
names (met_avg_2015)[1] <- "Date"
names (met_avg_2015)[2] <- "Wind_ms"

met_avg_2017 <- met_2017 %>% select(TIMESTAMP,WS_ms_Avg)
met_avg_2017$TIMESTAMP <- as.POSIXct(strptime(met_avg_2017$TIMESTAMP, "%m/%d/%Y", tz = "EST"))
met_avg_2017 <- aggregate(met_avg_2017$WS_ms_Avg, by=list(met_avg_2017$TIMESTAMP), FUN = "mean")
names (met_avg_2017)[1] <- "Date"
names (met_avg_2017)[2] <- "Wind_ms"

# Combine 2015, 2016, and 2017
met_wind <- rbind(met_avg_2015,met_avg_2017)
met_wind <- met_wind %>% mutate(grouping="met")

# Merge roa and met data
wind_merge <- merge(roa_wind,met_wind,by.x="DATE",by.y="Date",all.x=TRUE)
names(wind_merge)[2] <- "roa_wind"
names(wind_merge)[3] <- "grouping_roa"
names(wind_merge)[4] <- "met_wind"
names(wind_merge)[5] <- "grouping_met"

# Combine into one matrix
wind <- rbind(met_wind,roa_wind)

# Plot wind data from ROA and Met wind data for selected time periods
ggplot(wind, aes(x=Date, y=Wind_ms, group=grouping, color = as.factor(grouping))) +
  geom_line() + 
  labs(colour="",x = "", y = "Avg Wind Speed (m/s)") + 
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/Wind.jpg",wind,width=15,height=10)

# Plot wind data at roa vs. wind data from met station
wind_lim <- ggplot(wind_merge,aes(x=roa_wind,y=met_wind))+geom_point(size=2)+
  geom_smooth(method=lm)+
  labs(x="ROA Avg Wind Speed (m/s)",y="Met Avg Wind Speed (m/s)")+
  xlim(0,10)+ylim(0,10)+
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/Wind_lm.jpg",wind_lim,width=15,height=10)

# Check linear relationship between roa wind speed and met wind speed
reg <- lm(met_wind~roa_wind, data=wind_merge)
summary(reg)

# Calculate FCR wind speed using roa data
calc_wind <- roa_wind %>% select(Date,Wind_ms)
calc_wind$Wind_ms <- (wind_merge$roa_wind*0.3070)+1.1641

# Plot calculated wind vs. measured wind at FCR
fcr_wind <- cbind.data.frame(wind_merge,calc_wind$Wind_ms)
names(fcr_wind)[6] <- "calc"

ggplot(fcr_wind,aes(x=met_wind,y=calc))+geom_point(size=2)+
  geom_abline(slope=1,intercept=0)+
  labs(x="Met Avg Wind Speed (m/s)",y="Calc Avg Wind Speed (m/s)")+
  xlim(0,10)+ylim(0,10)+
  theme_classic()

# Calculate wind speed at FCR using ROA when there is no met data
roa_wind_all <- roa %>% select(DATE,wind_ms)
all_wind <- merge(roa_wind_all,met_wind,by.x="DATE",by.y="Date",all.x=TRUE)
names(all_wind)[1] <- "Date"
names(all_wind)[2] <- "roa_wind"
names(all_wind)[3] <- "met_wind"
names(all_wind)[4] <- "grouping"

# Calculate FCR wind from ROA wind data
all_wind$calc <- (all_wind$roa_wind*0.3070)+1.1641
all_wind_nan <- is.na(all_wind$met_wind)
all_wind$met_wind[all_wind_nan] <- with(all_wind, ((all_wind$roa_wind*0.3070)+1.1641))

# Plot
ggplot(all_wind,aes(x=roa_wind,y=met_wind))+geom_point(size=2)+
  labs(x="ROA Wind (m/s)",y="FCR Wind (m/s)")+
  xlim(0,10)+ylim(0,10)+
  theme_classic()

# Export out wind data (as a combination of Met wind data and calculated wind data)
write_csv(all_wind, path = "C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Data_Output/Calc_FCR_Wind.csv")

# Plot Roa wind data and met data
calcwind <- ggplot(all_wind,aes(Date))+geom_line(aes(y=roa_wind,colour="roa_wind"))+
  geom_line(aes(y=met_wind,colour="met_wind"))+labs(colour="",y="Wind Speed (m/s)")+
  theme_classic()

ggsave("C:/Users/ahounshell/OneDrive/VT/GHG/GHG_R/Fig_Output/calc_wind.jpg",calcwind,width=15,height=10)

# Saved as Rfile: Wind_Calc