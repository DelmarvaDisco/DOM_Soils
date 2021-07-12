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
df <- df %>% drop_na()
soil <- read_csv('data/20210516_KW_SoilHorizElev.csv') 
#note that QB KW-1W, KW-2E and TB KW-1W, KW-2E didn't have a lower
#B horiz elevation, so I added -0.5 since it falls within the generic sampling zone
#Also was getting some results where B had lower inundation dur than A did, which doesn't make sense
#because if A is wet, then B is too

#Filter to desired water year
df <- df %>% filter(Timestamp > "2019-09-30" & Timestamp < "2020-10-01")

#Identify threshold of interest
threshold<- -0.5

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
#observ <- annual %>% group_by(wetland,station) %>% 
          #summarise(n_obs = length(Timestamp))

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
soil_annual <- join %>% arrange(wetland, station, Timestamp) %>% drop_na(y_n)

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
            B_n_events          = sum(Bevent,  na.rm = T))

#Override metrics for DB KW-1W, ND KW-1W, and ND KW-2E
#because these sites had O/A combined horizons so if A is wet, consider O wet also

#DB KW-1W 
#soil_annual_metrics[1,4] = soil_annual_metrics[1,7] #O_dur_day = A_dur_day
#soil_annual_metrics[1,5] = soil_annual_metrics[1,8] #O_percent_sat = A_percent_sat

#ND KW-1W
#soil_annual_metrics[5,4] = soil_annual_metrics[5,7] #O_dur_day = A_dur_day
#soil_annual_metrics[5,5] = soil_annual_metrics[5,8] #O_percent_sat = A_percent_sat

#ND KW-2E
#soil_annual_metrics[6,4] = soil_annual_metrics[6,7] #O_dur_day = A_dur_day
#soil_annual_metrics[6,5] = soil_annual_metrics[6,8] #O_percent_sat = A_percent_sat

#Group by station only
soil_station_horizon <- soil_annual_metrics %>% 
  group_by(station) %>% 
  summarise(mean_O_dur_day  = mean(O_dur_day),
            sd_O_dur_day    = sd(O_dur_day),
            mean_O_n_events = mean(O_n_events),
            sd_O_n_events   = sd(O_n_events),
            mean_A_dur_day  = mean(A_dur_day),
            sd_A_dur_day    = sd(A_dur_day),
            mean_A_n_events = mean(A_n_events),
            sd_A_n_events   = sd(A_n_events),
            mean_B_dur_day  = mean(B_dur_day),
            sd_B_dur_day    = sd(B_dur_day),
            mean_B_n_events = mean(B_n_events),
            sd_B_n_events   = sd(B_n_events),)

#2.4 Threshold Event Duration Calc------------------------------------------------
#using all 3 years of data and generic threshold for now
#Sort based on site & station
df <- df %>% arrange(wetland, station, Timestamp) %>% drop_na()

#Define periods where water level is above threshold
df <- df %>% mutate(inun = if_else(y_n>threshold, 1,0))

#Define when each event starts or ends - lead or lag functions
#Here I use lead and say, if inundation is currently 0 and the next day is 1, then the event starts
df <- df %>% 
    mutate(event_start= if_else(wetland == lead(wetland) &
                                 station==lead(station) & 
                                 inun==0 & 
                                 lead(inun)==1,1,0))

#Define event id
df <- df %>% 
  group_by(wetland, station) %>% 
  #cumulative sum by event start to create event id
    mutate(id_event = cumsum(event_start)) %>% 
  #remove event id from rows below threshold
    mutate(id_event = inun*id_event)  

#Remove periods not in event (i.e. any time event_id = 0)
df <- df %>% filter(id_event!=0)

#Metrics for each event
#Estimate metric for each event
event_metrics <- df %>% 
  group_by(wetland, station, id_event) %>% 
  summarise(duration = max(Timestamp)-min(Timestamp))
#Summarize events
event_summary <- event_metrics %>% 
  group_by(wetland, station) %>% 
  summarise(dur_mean = mean(duration, na.rm=T),
            dur_min = min(duration,na.rm=T),
            dur_max = max(duration,na.rm=T),
            dur_median = median(duration,na.rm=T),
            n_events = n())

#2.5 Horizon Specific Event Duration Calc------------------------------------------------

#Join together water level data and soil horizon elevations
join <- left_join(df,soil,by=c("wetland","station"))

#Sort based on site & station
soil_annual <- join %>% arrange(wetland, station, Timestamp) %>% drop_na(y_n)

#Create column with binary indicator of saturation in each horizon
soil_annual<- soil_annual %>% mutate(inunO = if_else(y_n>O_lower,1,0),
                                     inunA = if_else(y_n>A_lower,1,0),
                                     inunB = if_else(y_n>B_lower,1,0))

#Identify individual periods of saturation in each horizon
soil_annual<-soil_annual %>% 
  mutate(Oevent = if_else(wetland == lead(wetland) &
                            station==lead(station) &
                            inunO == 0 & 
                            lead(inunO) == 1, 1, 0),
         Aevent = if_else(wetland == lead(wetland) &
                            station==lead(station) &
                            inunA == 0 & 
                            lead(inunA) == 1, 1, 0),
         Bevent = if_else(wetland == lead(wetland) &
                            station==lead(station) &
                            inunB == 0 & 
                            lead(inunB) == 1, 1, 0))

#Define event id
soil_annual <- soil_annual %>% 
  group_by(wetland, station) %>% 
  #cumulative sum by event start to create event id
  mutate(O_id_event = cumsum(Oevent),
         A_id_event = cumsum(Aevent),
         B_id_event = cumsum(Bevent)) %>% 
  #remove event id from rows below threshold
  mutate(O_id_event = inunO*O_id_event,
         A_id_event = inunA*A_id_event,
         B_id_event = inunB*B_id_event)  

#how to remove ID = 0 without removing transect points where there were no events?
#Remove periods not in event (i.e. any time event_id = 0)
#soil_annual <- soil_annual %>% filter(O_id_event!=0,
                                      #A_id_event!=0,
                                      #B_id_event!=0)

#Metrics for each event
#Estimate metric for each event and horizon
soil_Oevent_metrics <- soil_annual %>% 
  filter(O_id_event!=0) %>% 
  group_by(wetland, station, O_id_event) %>% 
  summarise(
    Oduration = max(Timestamp)-min(Timestamp))

soil_Aevent_metrics <- soil_annual %>%
  filter(A_id_event!=0) %>% 
  group_by(wetland, station, A_id_event) %>%
  summarise(
    Aduration = max(Timestamp)-min(Timestamp))

soil_Bevent_metrics <- soil_annual %>% 
  filter(B_id_event!=0) %>% 
  group_by(wetland, station, B_id_event) %>% 
  summarise(
    Bduration = max(Timestamp)-min(Timestamp))


#Summarize events
O_event_summary <- soil_Oevent_metrics %>% 
  group_by(wetland, station) %>% 
  summarise(O_dur_mean = mean(Oduration, na.rm=T),
            O_dur_min = min(Oduration, na.rm=T),
            O_dur_max = max(Oduration, na.rm=T),
            O_dur_sd = sd(Oduration, na.rm=T),
            O_n_events = n())

A_event_summary <- soil_Aevent_metrics %>% 
  group_by(wetland, station) %>% 
  summarise(A_dur_mean = mean(Aduration, na.rm=T),
            A_dur_min = min(Aduration, na.rm=T),
            A_dur_max = max(Aduration, na.rm=T),
            A_dur_sd = sd(Aduration, na.rm=T),
            A_n_events = n())

B_event_summary <- soil_Bevent_metrics %>% 
  group_by(wetland, station) %>% 
  summarise(B_dur_mean = mean(Bduration, na.rm=T),
            B_dur_min = min(Bduration, na.rm=T),
            B_dur_max = max(Bduration, na.rm=T),
            B_dur_sd = sd(Bduration, na.rm=T),
            B_n_events = n())

#join tables together
soil_event_summary <- full_join(O_event_summary,A_event_summary,by = c("wetland", "station"))
soil_event_summary <- full_join(soil_event_summary,B_event_summary,by=c("wetland", "station"))


#create new data frame
soil_sum <- as.data.frame(soil_event_summary)
#replace NA with 0
soil_sum[is.na(soil_sum)] <- 0


#add zeros for uplands with no events
ND_KW_U <- c(wetland="ND",station="KW-4U",
             O_dur_mean=as.difftime(0,units="days"),
             O_dur_min= as.difftime(0,units="days"),
             O_dur_max= as.difftime(0,units="days"),
             O_dur_sd=  as.difftime(0,units="days"),
             O_n_events=as.difftime(0,units="days"),
             A_dur_mean=as.difftime(0,units="days"),
             A_dur_min= as.difftime(0,units="days"),
             A_dur_max= as.difftime(0,units="days"),
             A_dur_sd=  as.difftime(0,units="days"),
             A_n_events=as.difftime(0,units="days"),
             B_dur_mean=as.difftime(0,units="days"),
             B_dur_min= as.difftime(0,units="days"),
             B_dur_max= as.difftime(0,units="days"),
             B_dur_sd=  as.difftime(0,units="days"),
             B_n_events=as.difftime(0,units="days"))

QB_KW_U <- c(wetland="QB",station="KW-4U",
             O_dur_mean=as.difftime(0,units="days"),
             O_dur_min= as.difftime(0,units="days"),
             O_dur_max= as.difftime(0,units="days"),
             O_dur_sd=  as.difftime(0,units="days"),
             O_n_events=as.difftime(0,units="days"),
             A_dur_mean=as.difftime(0,units="days"),
             A_dur_min= as.difftime(0,units="days"),
             A_dur_max= as.difftime(0,units="days"),
             A_dur_sd=  as.difftime(0,units="days"),
             A_n_events=as.difftime(0,units="days"),
             B_dur_mean=as.difftime(0,units="days"),
             B_dur_min= as.difftime(0,units="days"),
             B_dur_max= as.difftime(0,units="days"),
             B_dur_sd=  as.difftime(0,units="days"),
             B_n_events=as.difftime(0,units="days"))

TB_KW_U <- c(wetland="TB",station="KW-4U",
             O_dur_mean=as.difftime(0,units="days"),
             O_dur_min= as.difftime(0,units="days"),
             O_dur_max= as.difftime(0,units="days"),
             O_dur_sd=  as.difftime(0,units="days"),
             O_n_events=as.difftime(0,units="days"),
             A_dur_mean=as.difftime(0,units="days"),
             A_dur_min= as.difftime(0,units="days"),
             A_dur_max= as.difftime(0,units="days"),
             A_dur_sd=  as.difftime(0,units="days"),
             A_n_events=as.difftime(0,units="days"),
             B_dur_mean=as.difftime(0,units="days"),
             B_dur_min= as.difftime(0,units="days"),
             B_dur_max= as.difftime(0,units="days"),
             B_dur_sd=  as.difftime(0,units="days"),
             B_n_events=as.difftime(0,units="days"))

soil_sum_rbind = rbind(soil_sum,ND_KW_U)
soil_sum_rbind = rbind(soil_sum_rbind,QB_KW_U)
soil_sum_rbind = rbind(soil_sum_rbind,TB_KW_U)

#arrange
soil_event_summary <- soil_sum_rbind %>% arrange(wetland, station)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Export data----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#original code
write_csv(annual, "data//annual_metrics_threshold.csv")
write_csv(monthly, "data//monthly_metrics_thredhold.csv")

#save 2020 water year data as separate csv
write_csv(df, "data//waterLevel_2020only.csv")

### 2020 water year ###
#threshold#
#2020 annual metrics using threshold = -0.5
write_csv(annual, "data//annual_metrics_2020_threshold.csv")
#event metrics for threshold = -0.5
#write_csv(event_metrics, "data//threshold_event_metrics_2020.csv")
#summary of event metrics for threshold = -0.5
write_csv(event_summary,"data//threshold_event_summary_2020.csv") 

# soil horizon#
#2020 annual metrics for soil horizons
write_csv(soil_annual_metrics, "data//horizon_annual_metrics_2020.csv") 
#event metrics for soil horizons
#write_csv(soil_event_metrics, "data//horizon_event_metrics_2020.csv") 
#summary of event metrics for soil horizons
write_csv(soil_event_summary,"data//horizon_event_summary_2020.csv") 

### 2017-2020 water years ###
#threshold#
#2017-20 annual metrics using threshold = -0.5
write_csv(annual,"data//annual_metrics_2017-2020_threshold.csv") 
#event metrics for threshold = -0.5
#write_csv(event_metrics, "data//threshold_event_metrics_2017-2020.csv")
#summary of event metrics for threshold = -0.5
write_csv(event_summary,"data//threshold_event_summary_2017-2020.csv") 

#soil horizon#
#2017-20 annual metrics for soil horizons
write_csv(soil_annual_metrics,"data//horizon_annual_metrics_2017-2020.csv")
#event metrics for soil horizons
#write_csv(soil_event_metrics, "data//horizon_event_metrics_2017-2020.csv")
#summary of event metrics for soil horizons
write_csv(soil_event_summary,"data//horizon_event_summary_2017-2020.csv") 



