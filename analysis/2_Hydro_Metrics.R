#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Hydrologic Regime and Microbrial Dynamics
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/13/2021
#Purpose: Develop hydrologic regime metrics for ESOM Analysis
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
threshold<- -0.3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Estimate metrics-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Estimate Annual Metrics----------------------------------------------------
#Sort based on site & station
annual <- df %>% arrange(wetland, station, Timestamp)

#Create column with binary indicator of saturation
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

#Summarize Data
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

#Create column with binary indicator of saturation
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

#Create column with binary indicator of saturation in each horizon
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

#Override metrics for DB KW-1W, ND KW-1W, and ND KW-2E
#because these sites had O/A combined horizons so if A is wet, consider O wet also

#DB KW-1W 
soil_annual_metrics[1,4] = 293 #O_dur_day = A_dur_day
soil_annual_metrics[1,5] = 0.8027397 #O_percent_sat = A_percent_sat

#ND KW-1W
soil_annual_metrics[5,4] = 295 #O_dur_day = A_dur_day
soil_annual_metrics[5,5] = 0.8082192 #O_percent_sat = A_percent_sat

#ND KW-2E
soil_annual_metrics[6,4] = 264 #O_dur_day = A_dur_day
soil_annual_metrics[6,5] = 0.7232877 #O_percent_sat = A_percent_sat


#2.4 Trial Run Event Duration Calc------------------------------------------------
#using all 3 years of data and generic threshold for now
#Sort based on site & station
df <- df %>% arrange(wetland, station, Timestamp)

#Define periods where water level is above threshold
df <- df %>% mutate(inun = if_else(y_n>threshold, 1,0))

#Define when each event starts - use lag instead of lead
df <- df %>% 
    mutate(event_start = if_else(wetland == lag(wetland) &
                                 station==lag(station) & 
                                 inun==1 & 
                                 lag(inun)==0,1,0))

#Define event id - df already arranged
df <- df %>% 
  #cumulative sum by event start to create event id
    mutate(id_event = cumsum(event_start)) %>% 
  #remove event id from rows below threshold
    mutate(id_event = id_event*event_start)

#Metrics for each event
#Estimate metric for each event
event_metrics <- df %>% 
  group_by(wetland, station, id_event) %>% 
  summarise(
    duration = max(Timestamp)-min(Timestamp))
#Summarize events
event_summary <- event_metrics %>% 
  group_by(wetland, station, id_event) %>% 
  summarise(dur_mean = mean(duration, na.rm=T),
            n_events = n())


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
