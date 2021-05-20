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
soil <- read_csv("data/horizon_annual_metrics.csv")

#Join tables
#extraction results and annual WL metrics
data <- inner_join(df, annual, by=c("wetland","station"))
glimpse(data)
#extraction results and soil horizon metrics
soildata <- inner_join(df, soil, by=c("wetland","station"))
glimpse(soildata)

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

#2.1.4 EOC Plot
figureEOC <- ggarrange(mstat, 
                       meanEOC, 
                       minEOC, 
                       maxEOC,
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

#2.2.4 FI Plot---------------------------------------
figureFI <- ggarrange(FIoverall,
                      FImean, 
                      FImin, 
                      FImax,
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

#2.3.4 SUVA Plot---------------------------------------
figureSUVA <- ggarrange(SUVAoverall,
                        SUVAmean, 
                        SUVAmin, 
                        SUVAmax,
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

#2.4.4 HIX Plot ---------------------------------------
figureHIX <- ggarrange(HIXoverall,
                       HIXmean, 
                       HIXmin, 
                       HIXmax,
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

#2.5.2 SSR Plot---------------------------------------
figureSSR <- ggarrange(SSRoverall,SSRmean,
                       labels = c("A", "B"),
                       ncol = 1, nrow = 2)
figureSSR

### 2.6 %Protein ###---------------------------------------
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

#2.6.2 Protein Plot---------------------------------------
figureP <- ggarrange(Poverall,Pmean,
                       labels = c("A", "B"),
                       ncol = 1, nrow = 2)
figureP

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Duration Inundation, Wet/Dry Alternations, % of year Saturated  -----------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 3.1 EOC ###---------------------------------------
#3.1.1 EOC vs Dur ---------------------------------------
#horizon
EOCdur <- ggplot(data, aes(dur_day,EOC_mgC_L,col=Generic_Horizon)) +
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

#3.1.2 EOC vs N events ---------------------------------------
EOCn <- ggplot(data, aes(n_events,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs N Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')

ggplot(data, aes(n_events,EOC_mgC_L)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs N Saturation Events") + 
  theme_bw() +
  geom_smooth(method = 'lm')

#3.1.3 % Yr Sat ---------------------------------------
EOCsat <- ggplot(data, aes(percent_sat,EOC_mgC_L,col=Generic_Horizon)) +
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

#3.1.4 CV of water level ---------------------------------------
EOCcv <- ggplot(data, aes(CV_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("CV of Water Level") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs CV") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#3.1.4 EOC Plot ---------------------------------------

figureEOC <- ggarrange(EOCdur, 
                       EOCsat, 
                       EOCn, 
                       EOCcv,
                     labels = c("A", "B","C","D"),
                     ncol = 2, nrow = 2)
figureEOC


### 3.2 FI ###---------------------------------------
#3.2.1 FI vs Dur ---------------------------------------
FIdur<- ggplot(data, aes(dur_day,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.2.2 FI vs N events -----------------------------------
FIn <- ggplot(data, aes(n_events,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("FI") + 
  ggtitle("Wetland FI vs N Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.2.3 % Yr Sat ---------------------------------------
FIsat <- ggplot(data, aes(percent_sat,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of Year Saturated") +
  ylab("FI") + 
  ggtitle("Wetland FI vs % of Year Saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.2.4 CV of water level --------------------------------
FIcv <- ggplot(data, aes(CV_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("CV of Water Level") +
  ylab("FI") + 
  ggtitle("Wetland FI vs CV of Water Level") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.2.5 FI Plot ---------------------------------------
figureFI <- ggarrange(FIdur, 
                      FIsat, 
                      FIn, 
                      FIcv,
                      labels = c("A", "B","C","D"),
                      ncol = 2, nrow = 2)

figureFI

### 3.3 SUVA ###--------------------------------------- 
#3.2.1 SUVA vs Dur ---------------------------------------
SUVAdur <- ggplot(data, aes(dur_day,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#3.3.2 SUVA vs N events -----------------------------------
SUVAn <- ggplot(data, aes(n_events,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs N Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.3.3 % Yr Sat ---------------------------------------
SUVAsat <- ggplot(data, aes(percent_sat,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of Year Saturated") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs % of Year Saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.3.4 CV of water level --------------------------------
SUVAcv <- ggplot(data, aes(CV_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("CV of Water Level") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Water Level CV") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.3.5 SUVA Plot ---------------------------------------
figureSUVA <- ggarrange(SUVAdur, 
                        SUVAsat, 
                        SUVAn, 
                        SUVAcv,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureSUVA

### 3.4 HIX ###---------------------------------------
#3.4.1 HIX vs Dur ---------------------------------------
HIXdur <- ggplot(data, aes(dur_day,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#3.4.2 HIXvs N events -----------------------------------
HIXn <- ggplot(data, aes(n_events,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs N Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.4.3 % Yr Sat ---------------------------------------
HIXsat <- ggplot(data, aes(percent_sat,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of Year Saturated") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs % of Year Saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.4.4 CV of water level --------------------------------
HIXcv <- ggplot(data, aes(CV_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("CV of Water Level") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Water Level CV") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.4.5 HIX Plot ---------------------------------------
figureHIX <- ggarrange( HIXdur, 
                        HIXsat, 
                        HIXn, 
                        HIXcv,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureHIX

### 3.5 SSR ###---------------------------------------
#3.5.1 SSR vs Dur ---------------------------------------
SSRdur <- ggplot(data, aes(dur_day,SSR,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#3.5.2 SSR vs N events -----------------------------------
SSRn <- ggplot(data, aes(n_events,SSR,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs N Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.5.3 % Yr Sat ---------------------------------------
SSRsat <- ggplot(data, aes(percent_sat,SSR,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of Year Saturated") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs % of Year Saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.5.4 CV of water level --------------------------------
SSRcv <- ggplot(data, aes(CV_waterLevel,SSR,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("CV of Water Level") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Water Level CV") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.5.5 SSR Plot ---------------------------------------
figureSSR <- ggarrange( SSRdur, 
                        SSRsat, 
                        SSRn, 
                        SSRcv,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureSSR

### 3.6 %Protein ###---------------------------------------
#3.6.1 % Protein vs Dur ---------------------------------------
Pdur <- ggplot(data, aes(dur_day,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Duration (d)") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#3.6.2 % Protein vs N events -----------------------------------
Pn <- ggplot(data, aes(n_events,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("n events") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs N Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.6.3 % Yr Sat ---------------------------------------
Psat <- ggplot(data, aes(percent_sat,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("% of Year Saturated") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs % of Year Saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.6.4 CV of water level --------------------------------
Pcv <- ggplot(data, aes(CV_waterLevel,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("CV of Water Level") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs Water Level CV") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.6.5 % Protein Plot ---------------------------------------
figureP <- ggarrange( Pdur, 
                      Psat, 
                      Pn, 
                      Pcv,
                      labels = c("A", "B","C","D"),
                      ncol = 2, nrow = 2)
figureP

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Horizon Specific N events and Sat Dur -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 4.1 EOC ###---------------------------------------
#4.1.1 EOC vs Dur ---------------------------------------
#horizon
ggplot() +
  geom_point(data=soildata, aes(O_dur_day,EOC_mgC_L,col="red",size=1))+
  geom_point(data=soildata, aes(A_dur_day,EOC_mgC_L,col="green",size=1))+
  geom_point(data=soildata, aes(B_dur_day,EOC_mgC_L,col="blue",size=1))+
  xlab("Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Duration") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#3.1.2 EOC vs N events ---------------------------------------
ggplot() +
  geom_point(data=soildata, aes(O_n_events,EOC_mgC_L,col="red",size=1))+
  geom_point(data=soildata, aes(A_n_events,EOC_mgC_L,col="green",size=1))+
  geom_point(data=soildata, aes(B_n_events,EOC_mgC_L,col="blue",size=1))+
  xlab("N events") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Number of Saturation Events") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#3.1.3 % Yr Sat ---------------------------------------
ggplot() +
  geom_point(data=soildata, aes(O_percent_sat,EOC_mgC_L,col="red",size=1))+
  geom_point(data=soildata, aes(A_percent_sat,EOC_mgC_L,col="green",size=1))+
  geom_point(data=soildata, aes(B_percent_sat,EOC_mgC_L,col="blue",size=1))+
  xlab("% year saturated") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs % of Year Saturated") + 
  theme_bw()+
  geom_smooth(method = 'lm')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Other Plots -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##5.1 CV of Water Level-----------------------------
#CV by transect point
ggplot(data, aes(station,CV_waterLevel,fill=station))+
  geom_boxplot()+
  xlab("Transect Point") +
  ylab("CV") + 
  ggtitle("Coefficient of Variation By Transect Point") + 
  theme_bw()

