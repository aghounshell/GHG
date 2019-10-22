### Script to calculate atmospheric CO2 concentrations from SCT
### https://www.esrl.noaa.gov/gmd/dv/data/index.php?site=SCT
### A Hounshell, 21 Oct 2019

### Load in data
pacman::p_load(ncdf4,tidyverse,ggplot2,zoo)

flux <- nc_open('C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT/CO2/co2_sct_tower-insitu_1_ccgg_HourlyData (1).nc')

# Export metadata out as text file
{sink('C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT/CO2/co2_metadata.txt')
  print(flux)
  sink()}

# Extract date/time, elevation, and CH4 (ppb)
datetime <- ncvar_get(flux,"datetime")
datetime <- as.POSIXct(strptime(datetime, "%Y-%m-%d", tz="EST"))
co2 <- ncvar_get(flux,"value")


# Combine datetime and ch4 in one dataframe
carbon <- cbind.data.frame(datetime,co2)

# Average methane concentrations across days
co2_avg <- carbon %>% group_by(datetime) %>% summarize_all(funs(mean)) %>% arrange(datetime)

# Plot for full record
ggplot(co2_avg,aes(x=datetime,y=co2))+
  geom_line()

# Separate by year (2016 and 2017)
co2_2016 <- co2_avg %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
co2_2017 <- co2_avg %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

# Plot to check
ggplot(co2_2016,aes(x=datetime,y=co2))+
  geom_line()

ggplot(co2_2017,aes(x=datetime,y=co2))+
  geom_line()

# Read in calculate n/V from Atm_CH4
met_avg_2016 <- read.csv("C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT/met_avg_2016.csv")
met_avg_2016$datetime <- as.POSIXct(strptime(met_avg_2016$datetime, "%Y-%m-%d", tz="EST"))
met_avg_2017 <- read.csv("C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT/met_avg_2017.csv")
met_avg_2017$datetime <- as.POSIXct(strptime(met_avg_2017$datetime, "%Y-%m-%d", tz="EST"))

# Use nV to calculate umol/L of CH4 for each day in 2016 and 2017
# First for 2016
co2_2016_merge <- merge(met_avg_2016, co2_2016, by="datetime", all.x=TRUE, all.y=TRUE)
co2_2016_merge$co2 <- na.approx(co2_2016_merge$co2,na.rm=FALSE)
co2_2016_merge$co2_umol_l <- co2_2016_merge$co2*co2_2016_merge$nV*1000000/(1e6)

# Then for 2017
co2_2017_merge <- merge(met_avg_2017,co2_2017,by="datetime",all.x=TRUE,all.y=TRUE)
co2_2017_merge$co2 <- na.approx(co2_2017_merge$co2,na.rm=FALSE)
co2_2017_merge$co2_umol_l <- co2_2017_merge$co2*co2_2017_merge$nV*1000000/(1e6)

# Export out for both 2016 and 2017
write_csv(co2_2016_merge, path = "C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_co2_2016")
write_csv(co2_2017_merge, path = "C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_co2_2017")

# Saved R File as 'AtmCO2'