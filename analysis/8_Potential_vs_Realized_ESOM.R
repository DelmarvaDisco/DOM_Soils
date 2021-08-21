#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Estimating potential vs realized DOM export
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 6/9/2021
#Updated: 
#Purpose: Thought experiment to see what realized export could be based on site 
#         hydrology in the Upland O and Wetland O horizon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

ChristDavid <- read.csv("data/ChristDavid1996.csv") 
Chow <- read.csv("data/Chow2006.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Modeling First Order Decay-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Christ and David 1996
DOC <- ChristDavid$DOC_mgC_gSoil
time <- ChristDavid$Time_d
CD.fit <- nls(DOC ~ C0*exp(-k*time),
              start=list(C0=0.6,k=0.01))
summary(CD.fit,corr=T) #k = 0.014
confint(CD.fit)
qqnorm(resid(CD.fit))
qqline(resid(CD.fit))
shapiro.test(resid(CD.fit))
plot(fitted(CD.fit),resid(CD.fit), pch=19, xlab="Predicted
conc", ylab = "residual", main="Residual plot")
abline(h=0, lty="dashed")

plot(time,DOC,pch=19,ylab="DOC (mg C/g soil)",xlab="Time (d)")
tm <- seq(0,70,by=5)
new.time <- data.frame(time=tm)
predic.DOC <- predict(CD.fit,new.time)
plot(tm,predic.DOC,type="l",ylab="DOC (mg C/g soil)",xlab="Time (d)",
     main="Christ & David 1996 - First Order Decay")
points(time, DOC, pch=19)

#Chow 2006
#Christ and David 1996
DOC <- Chow$DOC_mgC_gSoil
time <- Chow$Time_d
CD.fit <- nls(DOC ~ C0*exp(-k*time),
              start=list(C0=0.4,k=0.01))
summary(CD.fit,corr=T) #k = 0.007
confint(CD.fit)
qqnorm(resid(CD.fit))
qqline(resid(CD.fit))
shapiro.test(resid(CD.fit))
plot(fitted(CD.fit),resid(CD.fit), pch=19, xlab="Predicted
conc", ylab = "residual", main="Residual plot")
abline(h=0, lty="dashed")

plot(time,DOC,pch=19,ylab="DOC (mg C/g soil)",xlab="Time (d)")
tm <- seq(0,70,by=5)
new.time <- data.frame(time=tm)
predic.DOC <- predict(CD.fit,new.time)
plot(tm,predic.DOC,type="l",ylab="DOC (mg C/g soil)",xlab="Time (d)",
     main="Christ & David 1996 - First Order Decay")
points(time, DOC, pch=19)

#Average K values
k = (0.014+0.007)/2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Dur of sat for calculating K-----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Assuming you have ESOM at two points in time, spring and autumn, find
#the duration of saturation during the 2020 water year leading up to these two points.
#This will allow you to calculate k in the first order exponential decay equation

#Similar set up to 1_waterlevel_Calc.R script
#load relevant packages
library(lubridate)
library(tidyverse)
library(raster)

#Read data
df<-read_csv('data/waterLevel_at_sampling_location.csv')
df <- df %>% drop_na()
soil <- read_csv('data/20210516_KW_SoilHorizElev.csv') 
soil_TB <- read_csv('data/20210516_KW_SoilHorizElev_TBedit.csv') 
#note that QB KW-1W, KW-2E and TB KW-1W, KW-2E didn't have a lower
#B horiz elevation, so I added -0.5 since it falls within the generic sampling zone
#Also was getting some results where B had lower inundation dur than A did, which doesn't make sense
#because if A is wet, then B is too

#Filter to desired water year
#spring sampling was Mar 15, 2020
sat_spring <- df %>% filter(Timestamp > "2019-09-30" & Timestamp < "2020-03-10")
#autumn sampling was Sept 21, 2020
sat_autumn <- df %>% filter(Timestamp > "2019-09-30" & Timestamp < "2020-09-21")

#3.1 Spring First ---------------------------------------------------

#Join together water level data and soil horizon elevations
join <- left_join(sat_spring,soil,by=c("wetland","station"))
join <- left_join(sat_spring,soil_TB,by=c("wetland","station"))

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
            sd_B_n_events   = sd(B_n_events))

#3.2 Now Autumn ---------------------------------------------------

#Join together water level data and soil horizon elevations
join <- left_join(sat_autumn,soil,by=c("wetland","station"))
join <- left_join(sat_autumn,soil_TB,by=c("wetland","station"))

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
            sd_B_n_events   = sd(B_n_events))
