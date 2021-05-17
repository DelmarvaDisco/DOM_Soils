#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: ESOM and Hydro Metrics
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 4/1/2021
#Updated: 4/9/2021
#Purpose: How do hydrologic metrics influence ESOM?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(lubridate)
library(tidyverse)
library(ggplot2)

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv")
annual <- read_csv("data/annual_metrics_2020.csv") #annual_metrics depends on what water year 

#Join tables
data <- inner_join(df, annual, by=c("wetland","station"))
glimpse(data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Mean WL and ESOM ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ESOM vs Mean WL colored by station
ggplot(data, aes(mean_waterLevel,EOC_mgC_L,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw() 

#ESOM vs Mean WL colored by horizon
ggplot(data, aes(mean_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#FI vs Mean WL
#By horizon
ggplot(data, aes(mean_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By transect point
ggplot(data, aes(mean_waterLevel,FI,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean WL") + 
  theme_bw()

#SUVA vs Mean WL
#overall
ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By horizon
ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By transect point
ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()

#HIX
ggplot(data, aes(mean_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#SSR
ggplot(data, aes(mean_waterLevel,SSR)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#%Protein
ggplot(data, aes(mean_waterLevel,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("%Protein") + 
  ggtitle("Wetland %Protein vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Min/max WL and ESOM ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ESOM vs Min WL colored by station
ggplot(data, aes(min_waterLevel,EOC_mgC_L,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Min WL") + 
  theme_bw() 
ggplot(data, aes(min_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Minimum Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm') 
#Max
ggplot(data, aes(max_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm') 


#FI
#Min
#By horizon
ggplot(data, aes(min_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Minimum Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#Max
ggplot(data, aes(min_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Maximum Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#SUVA
#min
#overall
ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By horizon
ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By transect point
ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()
#max

#HIX
ggplot(data, aes(mean_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#SSR
ggplot(data, aes(mean_waterLevel,SSR)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#%Protein
ggplot(data, aes(mean_waterLevel,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("%Protein") + 
  ggtitle("Wetland %Protein vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Duration Inundation and ESOM ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC
#by horizon
ggplot(data, aes(dur_day,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#overall
ggplot(data, aes(dur_day,EOC_mgC_L)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#FI
ggplot(data, aes(dur_day,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Duration") + 
  theme_bw()

#SUVA
ggplot(data, aes(dur_day,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 # Alternations Wet/Dry and ESOM -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC
ggplot(data, aes(n_events,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs N Saturation Events") + 
  theme_bw()

ggplot(data, aes(n_events,EOC_mgC_L,col=station)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs N Saturation Events") + 
  theme_bw()

#FI
ggplot(data, aes(n_events,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("FI") + 
  ggtitle("Wetland FI vs N Saturation Events") + 
  theme_bw()

#SUVA
ggplot(data, aes(n_events,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs N Saturation Events") + 
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Percent of year saturated -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC
ggplot(data, aes(percent_sat,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of year saturated") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs % of year saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

ggplot(data, aes(n_events,EOC_mgC_L)) +
  geom_point(size=2.5) +
  xlab("% of year saturated") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs % of year saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#FI
ggplot(data, aes(n_events,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of year saturated") +
  ylab("FI") + 
  ggtitle("Wetland FI vs % of year saturated") + 
  theme_bw()

#SUVA
ggplot(data, aes(n_events,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of year saturated)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs % of year saturated") + 
  theme_bw()
