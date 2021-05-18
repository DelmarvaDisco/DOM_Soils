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
library(ggpubr)

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv")
annual <- read_csv("data/annual_metrics_2020.csv") #annual_metrics depends on what water year 

#Join tables
data <- inner_join(df, annual, by=c("wetland","station"))
glimpse(data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Mean/Min/Max WL and ESOM ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 2.1 EOC ###---------------------------------------
#2.1.1 ESOM vs Mean WL ------------------------------------
mstat <- ggplot(data, aes(mean_waterLevel,EOC_mgC_L,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw() 
meanEOC <- ggplot(data, aes(mean_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')

#2.1.2 ESOM vs Min WL ------------------------------------
ggplot(data, aes(min_waterLevel,EOC_mgC_L,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Min WL") + 
  theme_bw() 
minEOC <- ggplot(data, aes(min_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Minimum Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm') 

#2.1.3 ESOM vs Max WL ------------------------------------
maxEOC <- ggplot(data, aes(max_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm') 

#2.1.4 Plot
figureEOC <- ggarrange(mstat, meanEOC, minEOC, maxEOC,
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2)
figureEOC

### 2.2 FI ###---------------------------------------
#2.2.1 FI vs Mean WL-------------------------------------
FImean <- ggplot(data, aes(mean_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By transect point
FIoverall <- ggplot(data, aes(mean_waterLevel,FI)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.2.2 FI vs Min WL-------------------------------------
FImin <- ggplot(data, aes(min_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Minimum Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.2.3 FI vs Max WL---------------------------------------
FImax <- ggplot(data, aes(min_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Maximum Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.2.4 Plot---------------------------------------
figureFI <- ggarrange(FIoverall,FImean, FImin, FImax,
                       labels = c("A", "B", "C","D"),
                       ncol = 2, nrow = 2)
figureFI

### 2.3 SUVA ###---------------------------------------
#2.3.1 SUVA vs Mean WL---------------------------------------
#overall
SUVAoverall <- ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By horizon
SUVAmean <- ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
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

#2.3.2 SUVA vs Min WL---------------------------------------
#overall
ggplot(data, aes(min_waterLevel,SUVA254_L_mgm)) +
  geom_point(size=2.5) +
  xlab("Min Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By horizon
SUVAmin <- ggplot(data, aes(min_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Min Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.3.3 SUVA vs Max WL ---------------------------------------
#overall
ggplot(data, aes(max_waterLevel,SUVA254_L_mgm)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#By horizon
SUVAmax <- ggplot(data, aes(max_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.3.4 Plot---------------------------------------
figureSUVA <- ggarrange(SUVAoverall,SUVAmean, SUVAmin, SUVAmax,
                      labels = c("A", "B", "C","D"),
                      ncol = 2, nrow = 2)
figureSUVA


### 2.4 HIX ###---------------------------------------
#2.4.1 HIX vs Mean WL---------------------------------------
#overall
HIXoverall <- ggplot(data, aes(mean_waterLevel,HIX)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#by horizon
HIXmean <- ggplot(data, aes(mean_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.4.2 HIX vs Min WL---------------------------------------
HIXmin <- ggplot(data, aes(min_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Min Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.4.3 HIX vs Max WL---------------------------------------
HIXmax <- ggplot(data, aes(max_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.4.4 Plot ---------------------------------------
figureHIX <- ggarrange(HIXoverall,HIXmean, HIXmin, HIXmax,
                        labels = c("A", "B", "C","D"),
                        ncol = 2, nrow = 2)
figureHIX

### 2.5 SSR ###---------------------------------------
#2.5.1 HIX vs Mean WL---------------------------------------
SSRoverall <- ggplot(data, aes(mean_waterLevel,SSR)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
SSRmean <- ggplot(data, aes(mean_waterLevel,SSR,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.5.2 Plot---------------------------------------
figureSSR <- ggarrange(SSRoverall,SSRmean,
                       labels = c("A", "B"),
                       ncol = 1, nrow = 2)
figureSSR

### 2.6 %Protein ### ---------------------------------------
#2.6.1 %P vs Mean WL ---------------------------------------
Poverall <- ggplot(data, aes(mean_waterLevel,Percent_Protein)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("%Protein") + 
  ggtitle("Wetland %Protein vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
Pmean <- ggplot(data, aes(mean_waterLevel,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("%Protein") + 
  ggtitle("Wetland %Protein vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#2.6.2 Plot---------------------------------------
figureP <- ggarrange(Poverall,Pmean,
                       labels = c("A", "B"),
                       ncol = 1, nrow = 2)
figureP

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Duration Inundation and ESOM ----------------------------------------------
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
#4.0 # Alternations Wet/Dry and ESOM -------------------------------------------
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
#5.0 Percent of year saturated -------------------------------------------
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
