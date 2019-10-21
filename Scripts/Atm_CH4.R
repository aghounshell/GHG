### File to open .nc files for tower flux data (CO2 and CH4) from SCT
### https://www.esrl.noaa.gov/gmd/dv/data/index.php?site=SCT
### A Hounshell, 21 Oct 2019

### Load in data
pacman::p_load(ncdf4,tidyverse,ggplot2)

flux <- nc_open('C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT/ch4_tower-insitu_1_ccgg_netCDF/ch4_sct_tower-insitu_1_ccgg_HourlyData.nc')

# Export metadata out as text file
{sink('C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT/ch4_sct.txt')
  print(flux)
  sink()}

# Extract date/time, elevation, and CH4 (ppb)
datetime <- ncvar_get(flux,"datetime")
datetime <- as.POSIXct(strptime(datetime, "%Y-%m-%d", tz="EST"))
elevation <- ncvar_get(flux,"elevation")
ch4 <- ncvar_get(flux,"value")

# Combine datetime and ch4 in one dataframe
methane <- cbind.data.frame(datetime,ch4)

# Average methane concentrations across days
ch4_avg <- methane %>% group_by(datetime) %>% summarize_all(funs(mean)) %>% arrange(datetime)

# Plot for full record
ggplot(ch4_avg,aes(x=datetime,y=ch4))+
  geom_line()

# Separate by year (2016 and 2017)
ch4_2016 <- ch4_avg %>% filter(datetime>=as.Date('2016-01-01')&datetime<=as.Date('2016-12-31'))
ch4_2017 <- ch4_avg %>% filter(datetime>=as.Date('2017-01-01')&datetime<=as.Date('2017-12-31'))

# Plot to check
ggplot(ch4_2016,aes(x=datetime,y=ch4))+
  geom_line()

ggplot(ch4_2017,aes(x=datetime,y=ch4))+
  geom_line()

# Load in daily Met data for SCT (From KDNL)
# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:13837/detail
met <- read.csv("C:/Users/ahoun/OneDrive/Desktop/Fluxes/SCT_MetData_Clean.csv",stringsAsFactors = FALSE)
met$DATE <- as.POSIXct(strptime(met$DATE, "%Y-%m-%d", tz="EST"))
met$HourlyDryBulbTemperature <- as.numeric(met$HourlyDryBulbTemperature)
met$HourlySeaLevelPressure <- as.numeric(met$HourlySeaLevelPressure)

met_avg <- met %>% group_by(DATE) %>% summarize_all(funs(mean)) %>% arrange(DATE)

# Convert Temp from Deg F to K
met_avg$Kelvin = (met_avg$HourlyDryBulbTemperature+459.67)*(5/9)
met_avg$Kelvin <- na.approx(met_avg$Kelvin,na.rm=FALSE)
met_avg$atm = met_avg$HourlySeaLevelPressure/29.921
met_avg$atm <- na.approx(met_avg$atm,na.rm=FALSE)

met_avg_conv <- met_avg %>% select(DATE,Kelvin,atm)

# Calculate n/V for each day (mols/L)
met_avg_conv$nV = met_avg_conv$atm/(0.0820573660809596*met_avg_conv$Kelvin)

met_avg_2016 <- met_avg_conv %>% filter(DATE>=as.Date('2016-01-01')&DATE<=as.Date('2016-12-31'))
names(met_avg_2016)[1] <- 'datetime'
met_avg_2017 <- met_avg_conv %>% filter(DATE>=as.Date('2017-01-01')&DATE<=as.Date('2017-12-31'))
names(met_avg_2017)[1] <- 'datetime'

write_csv(met_avg_2016, path = "C:/Users/ahoun/OneDrive/Desktop/Fluxes/met_avg_2016")
write_csv(met_avg_2017, path = "C:/Users/ahoun/OneDrive/Desktop/Fluxes/met_avg_2017")

# Use nV to calculate umol/L of CH4 for each day in 2016 and 2017
# First for 2016
ch4_2016_merge <- merge(met_avg_2016, ch4_2016, by="datetime", all.x=TRUE, all.y=TRUE)
ch4_2016_merge$ch4 <- na.approx(ch4_2016_merge$ch4,na.rm=FALSE)
ch4_2016_merge$ch4_umol_l <- ch4_2016_merge$ch4*ch4_2016_merge$nV*1000000/(1e9)

# Then for 2017
ch4_2017_merge <- merge(met_avg_2017,ch4_2017,by="datetime",all.x=TRUE,all.y=TRUE)
ch4_2017_merge$ch4 <- na.approx(ch4_2017_merge$ch4,na.rm=FALSE)
ch4_2017_merge$ch4_umol_l <- ch4_2017_merge$ch4*ch4_2017_merge$nV*1000000/(1e9)

# Export out for both 2016 and 2017
write_csv(ch4_2016_merge, path = "C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_ch4_2016")
write_csv(ch4_2017_merge, path = "C:/Users/ahoun/OneDrive/Desktop/Fluxes/atm_ch4_2017")

# Saved R File as 'AtmCH4'