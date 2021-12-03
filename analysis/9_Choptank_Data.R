#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Choptank Mean Daily Discharge 2010-2020 Exploration
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 2021-12-02
#Updated: 
#Purpose: Gather Choptank Mean Daily Discharge data to determine if the 2020
#         water year was a typical water water year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Gage info
#USGS 01491000 CHOPTANK RIVER NEAR GREENSBORO, MD
#LOCATION.--Lat 38?59'49.9", long 75?47'08.9", referenced to North American Datum of 1983,
# Caroline County, MD, Hydrologic Unit 02060005, on right bank at highway bridge (removed),
# 0.1 mi upstream from Gravelly Branch, 2.0 mi northeast of Greensboro, and 60 mi upstream
# from mouth.
#DRAINAGE AREA.--113 mi2.

#clear environment
remove(list=ls())

#load packages
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Pull data from USGS -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

siteNumber <- "01491000" 
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" #discharge, cfs

#2010-2020
startDate <- "2009-10-01"  
endDate <- "2020-09-30"

discharge <- readNWISdv(siteNumber, parameterCd, startDate, endDate)
discharge <- addWaterYear(discharge)
names(discharge)[5]<- "Flow"
discharge$Mon_Day <- as.Date(discharge$Date)
discharge$Mon_Day <- format(discharge$Mon_Day,format= "%m-%d")

#Summarize water year data 2010-2020
MeanQ_WY <- discharge %>% group_by(waterYear) %>% 
  summarize(Mean_Daily_Q = mean(Flow,na.rm=T))
MeanQ_2010_2020 <- mean(MeanQ_WY$Mean_Daily_Q)
MeanQ_All <- mean(discharge$Flow,na.rm=T)
MedianQ_2010_2020 <- median(MeanQ_WY$Mean_Daily_Q,na.rm=T)

#All data 1948-2020
startDate <- "1948-10-01"  
endDate <- "2020-09-30"

allQ <- readNWISdv(siteNumber, parameterCd, startDate, endDate)
allQ <- addWaterYear(allQ)
names(allQ)[5]<- "Flow"

#Summarize water year data 1948-2020
MeanQ_WY_1948_2020 <- allQ %>% group_by(waterYear) %>% 
  summarize(Mean_Daily_Q = mean(Flow,na.rm=T))
MeanQ_allQ <- mean(MeanQ_WY_all$Mean_Daily_Q)
MeanQ_all <- mean(allQ$Flow,na.rm=T)
MedianQ_all <- median(MeanQ_WY_all$Mean_Daily_Q,na.rm=T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plot data -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.1 2010-2020 -----
WY2010 <- discharge %>% filter(Date > "2009-09-30" & Date < "2010-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2011 <- discharge %>% filter(Date > "2010-09-30" & Date < "2011-10-01") %>% mutate(wy_doy = seq(1:n())) 
WY2012 <- discharge %>% filter(Date > "2011-09-30" & Date < "2012-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2013 <- discharge %>% filter(Date > "2012-09-30" & Date < "2013-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2014 <- discharge %>% filter(Date > "2013-09-30" & Date < "2014-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2015 <- discharge %>% filter(Date > "2014-09-30" & Date < "2015-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2016 <- discharge %>% filter(Date > "2015-09-30" & Date < "2016-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2017 <- discharge %>% filter(Date > "2016-09-30" & Date < "2017-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2018 <- discharge %>% filter(Date > "2017-09-30" & Date < "2018-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2019 <- discharge %>% filter(Date > "2018-09-30" & Date < "2019-10-01") %>% mutate(wy_doy = seq(1:n()))
WY2020 <- discharge %>% filter(Date > "2019-09-30" & Date < "2020-10-01") %>% mutate(wy_doy = seq(1:n()))


#Hydrograph split by WY
ggplot()+
  geom_line(data = WY2010, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2011, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2012, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2013, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2014, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2015, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2016, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2017, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2018, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2019, aes(wy_doy,Flow,col=waterYear))+
  geom_line(data = WY2020, aes(wy_doy,Flow,col=waterYear))+
  ggtitle("Choptank Daily Flow Water Years 2010-2020")+
  labs(x = "Water Year Day", y = "Flow (cfs)") +
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_color_viridis_c() +
  scale_x_continuous(breaks = c(1, 93, 184, 275,366), labels = c("Oct 1", "Jan 1", "Apr 1", "July 1","Oct 1")) +
  theme_bw()


#Hydrograph  of all data
ggplot()+
  geom_line(data=discharge,aes(Date,Flow))+
  geom_line(data = WY2020, aes(Date,Flow),col="red")+
  ggtitle("Choptank Flow Water Years 2010-2020")+
  labs(x = "Water Year", y = "Flow (cfs)") +
  theme_bw()

#Bar graph of mean annual flow by water year
ggplot(MeanQ_WY,aes(factor(waterYear),Mean_Daily_Q))+
  geom_bar(stat="identity",fill="blue")+
  ggtitle("Choptank Mean Annual Flow Water Years 2010-2020")+
  labs(x = "Water Year", y = "Mean Annual Flow (cfs)") +
  theme_bw()+
  theme(axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  geom_hline(yintercept = mean(MeanQ_WY$Mean_Daily_Q),color="red",size=2)

#Cumulative discharge plot
cumulative_dat <- group_by(discharge, waterYear) %>%
  mutate(cumulative_dis = cumsum(Flow), 
         wy_doy = seq(1:n()))

ggplot(cumulative_dat, aes(x = wy_doy, y = cumulative_dis, group = waterYear)) +
  geom_line(aes(color = waterYear)) +
  scale_color_viridis_c() +
  scale_x_continuous(breaks = c(1, 93, 184, 275), labels = c("Oct 1", "Jan 1", "Apr 1", "July 1")) +
  theme_bw() +
  ggtitle("Cumulative Discharge by Water Year 2010-2020")+
  labs(color = "Water Year", x = "", y = "Cumulative Discharge (cfs)")+
  theme(axis.text.y   = element_text(size=16),
      axis.text.x   = element_text(size=16),
      axis.title.y  = element_text(size=16),
      axis.title.x  = element_text(size=16),
      panel.border = element_rect(colour = "black", fill=NA, size=0.5))

## 3.2 1948-2020 ----

#Rank flows
MeanQ_WY_1948_2020 <- MeanQ_WY_1948_2020 %>% mutate(ranks = rank(Mean_Daily_Q))
#Visualize ranks
ggplot(MeanQ_WY_1948_2020, aes(waterYear,Mean_Daily_Q,col=ranks))+
  geom_point(size=4)+
  ggtitle("Ranking of Choptank Mean Annual Flow Water Years 1948-2020")+
  labs(x = "Water Year", y = "Mean Annual Flow (cfs)") +
  theme_bw()+
  theme(axis.text.y   = element_text(size=16),
       axis.text.x   = element_text(size=16),
       axis.title.y  = element_text(size=16),
       axis.title.x  = element_text(size=16),
       panel.border = element_rect(colour = "black", fill=NA, size=0.5))
