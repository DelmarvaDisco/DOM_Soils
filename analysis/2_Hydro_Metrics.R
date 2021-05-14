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

#Read data
df<-read_csv('data/waterLevel_at_sampling_location.csv')

#Filter to desired water year
df <- df %>% filter(Timestamp > "2019-09-30" & Timestamp < "2020-10-02")

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

DB <- annual %>% filter(wetland == "DB")
ND <- annual %>% filter(wetland == "ND")
QB <- annual %>% filter(wetland == "QB")
TB <- annual %>% filter(wetland == "TB")

#Summarise Data
annual<-annual %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(min_waterLevel    = min(y_n,    na.rm = T), 
            mean_waterLevel   = mean(y_n,   na.rm = T), 
            median_waterLevel = median(y_n, na.rm = T), 
            max_waterLevel    = max(y_n,    na.rm = T), 
            dur_day           = sum(inun,   na.rm=T),
            percent_sat       = (sum(inun,   na.rm=T)/1468),
            n_events          = sum(event,  na.rm = T))

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Export data----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(annual, "data//annual_metrics.csv")
write_csv(monthly, "data//monthly_metrics.csv")
#save 2019-2020 water year data as separate csv
write_csv(df, "data//2020wateryear.csv")
