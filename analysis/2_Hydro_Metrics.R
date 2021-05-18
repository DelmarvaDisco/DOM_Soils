#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Hydrologic Regime and Microbrial Dynamics
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 3/4/2020
#Purpose: Develop hydrologic regime metrics for Microbrial Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(lubridate)
library(tidyverse)
library(raster)

#Read data
df<-read_csv('data/waterLevel_at_sampling_location.csv')
soil <- read_csv('data/20210516_KW_SoilHorizElev.csv')

#Filter to desired water year
df <- df %>% filter(Timestamp > "2019-10-01" & Timestamp < "2020-10-01")

#Identify threshold of interest
threshold<- -0.5

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Estimate metrics-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Estimate Annual Metrics----------------------------------------------------
#Sort based on site & station
annual <- df %>% arrange(wetland, station, Timestamp)

#Create collumn with bianary indicator of saturation
annual<-annual %>% mutate(inun = if_else(y_n>threshold, 1,0))

#Identify individual periods of saturation
annual<-annual %>% 
  mutate(event = if_else(wetland == lead(wetland) &
                           station==lead(station) &
                           inun == 1 & 
                           lead(inun) == 0, 
                         1, 0))

#Checking number of observations for each site and station
observ <- annual %>% group_by(wetland,station) %>% 
          summarise(n_obs = length(Timestamp))

#Summarise Data
annual<-annual %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(n_observations    = length(Timestamp),
            min_waterLevel    = min(y_n,    na.rm = T), 
            mean_waterLevel   = mean(y_n,   na.rm = T), 
            median_waterLevel = median(y_n, na.rm = T), 
            max_waterLevel    = max(y_n,    na.rm = T), 
            dur_day           = sum(inun,   na.rm=T),
            percent_sat       = (sum(inun,   na.rm=T)/n_observations),
            n_events          = sum(event,  na.rm = T),
            CV_waterLevel     = cv(y_n, na.rm = T))

#2.2 Estimate Monthly Metrics---------------------------------------------------
#Sort based on site & station
monthly <- df %>% arrange(wetland, station, Timestamp)

#Create collumn with bianary indicator of saturation
monthly<-monthly %>% mutate(inun = if_else(y_n>threshold, 1,0))

#Identify individual events of saturation
monthly<-monthly %>% mutate(event = if_else(wetland == lead(wetland) &
                                              station==lead(station) &
                                              inun == 1 & 
                                              lead(inun) == 0, 
                                            1, 0))

#Filter to March!
monthly<-monthly %>% mutate(month = lubridate::month(Timestamp)) %>% filter(month==3)

#Summarise data
monthly<-monthly %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(min_depth_m = min(y_n), 
            mean_depth_m = mean(y_n), 
            median_depth_m = median(y_n), 
            max_depth_m = max(y_n), 
            dur_day = sum(inun),
            n_events = sum(event))

#Add 1 event if month ended in saturation...
monthly<-monthly %>% mutate(n_events = if_else(dur_day>0 & n_events==0, 1, n_events))

#2.3 Soil Horizon Annual Metrics------------------------------------------------

#Join together water level data and soil horizon elevations
join <- left_join(df,soil,by=c("wetland","station"))

#Sort based on site & station
soil_annual <- join %>% arrange(wetland, station, Timestamp)

#Create collumn with bianary indicator of saturation in each horizon
soil_annual<- soil_annual %>% mutate(inunO = if_else(y_n>O_lower,1,0),
                                     inunA = if_else(y_n>A_lower,1,0),
                                     inunB = if_else(y_n>B_lower,1,0))

#Identify individual periods of saturation in each horizon
soil_annual<-soil_annual %>% 
  mutate(Oevent = if_else(wetland == lead(wetland) &
                           station==lead(station) &
                           inunO == 1 & 
                           lead(inunO) == 0, 1, 0),
         Aevent = if_else(wetland == lead(wetland) &
                            station==lead(station) &
                            inunA == 1 & 
                            lead(inunA) == 0, 1, 0),
         Bevent = if_else(wetland == lead(wetland) &
                            station==lead(station) &
                            inunB == 1 & 
                            lead(inunB) == 0, 1, 0))
#Summarise Data
soil_annual_metrics<-soil_annual %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(n_observations    = length(Timestamp),
            O_dur_day           = sum(inunO,   na.rm=T),
            O_percent_sat       =(sum(inunO,   na.rm=T)/n_observations),
            O_n_events          = sum(Oevent,  na.rm = T),
            A_dur_day           = sum(inunA,   na.rm=T),
            A_percent_sat       =(sum(inunA,   na.rm=T)/n_observations),
            A_n_events          = sum(Aevent,  na.rm = T),
            B_dur_day           = sum(inunB,   na.rm=T),
            B_percent_sat       =(sum(inunB,   na.rm=T)/n_observations),
            B_n_events          = sum(Bevent,  na.rm = T),)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Export data----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(annual, "data//annual_metrics.csv")
write_csv(monthly, "data//monthly_metrics.csv")

#for 2019-2020 water year
write_csv(annual, "data//annual_metrics_2020.csv")

#save 2019-2020 water year data as separate csv
write_csv(df, "data//2020wateryear.csv")

#Horizon annual metrics
write_csv(soil_annual_metrics, "data//horizon_annual_metrics.csv")
