#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: ESOM and Hydro Metrics 2020
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 4/1/2021
#Updated: 4/9/2021
#Purpose: How do 2020 water year hydrologic metrics influence ESOM?
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

#File read depends on time frame desired
#2017-2020 water years
#annual <- read_csv("data/annual_metrics_2017-2020_threshold.csv") 
#soil <- read_csv("data/horizon_annual_metrics_2017-2020.csv")
#event <- read_csv("data//horizon_event_summary_2017-2020.csv") 

#2020 water year only
annual <- read_csv("data/annual_metrics_2020_threshold.csv") #annual_metrics used for water level
soil <- read_csv("data/horizon_annual_metrics_2020.csv") #horizon metrics used for duration/n events
event <- read_csv("data//horizon_event_summary_2020.csv") #event duration calculated separately from other metrics

#Join tables
#extraction results and annual WL metrics
data <- inner_join(df, annual, by=c("wetland","station"))
glimpse(data)
#extraction results and soil horizon metrics
soildata <- inner_join(df, soil, by=c("wetland","station"))
glimpse(soildata)
#extraction results and horizon event duration
eventdata <- inner_join(df,event,by=c("wetland","station"))
glimpse(eventdata)

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
meanEOC <-ggplot(data, aes(mean_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -0.5)+
  stat_cor(label.x = 0)

EOCoverall <- ggplot(data, aes(mean_waterLevel,EOC_mgC_L)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -0.5)+
  stat_cor(label.x = 0)

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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.25)+
  stat_cor(label.x = -0.95)

#2.1.3 ESOM vs Max WL ------------------------------------
maxEOC <- ggplot(data, aes(max_waterLevel,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = 0)+
  stat_cor(label.x = 0.5)

#2.1.4 EOC Plot
figureEOC <- ggarrange(EOCoverall, 
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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -0.5)+
  stat_cor(label.x = -0.1)
#By transect point
FIoverall <- ggplot(data, aes(mean_waterLevel,FI)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -0.5)+
  stat_cor(label.x = -0.1)

#2.2.2 FI vs Min WL-------------------------------------
FImin <- ggplot(data, aes(min_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Minimum Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.4)+
  stat_cor(label.x = -1)

#2.2.3 FI vs Max WL---------------------------------------
FImax <- ggplot(data, aes(min_waterLevel,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Maximum Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.35)+
  stat_cor(label.x = -1)

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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -0.5,label.y=0.5)+
  stat_cor(label.x = -0.2,label.y=0.5)
#By horizon
SUVAmean <- ggplot(data, aes(mean_waterLevel,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA (L/mg-m)") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")
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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")

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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")

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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")
#by horizon
HIXmean <- ggplot(data, aes(mean_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")

#2.4.2 HIX vs Min WL---------------------------------------
HIXmin <- ggplot(data, aes(min_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Min Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Min WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")

#2.4.3 HIX vs Max WL---------------------------------------
HIXmax <- ggplot(data, aes(max_waterLevel,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Max Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Max WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.npc = "center",label.y.npc="bottom")+
  stat_cor(label.x.npc = 0.75,label.y.npc="bottom")

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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.=0.5)+
  stat_cor(label.x.npc = 0.75)
SSRmean <- ggplot(data, aes(mean_waterLevel,SSR,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.=0.5)+
  stat_cor(label.x.npc = 0.75)

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
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.=0.5)+
  stat_cor(label.x.npc = 0.75)
Pmean <- ggplot(data, aes(mean_waterLevel,Percent_Protein,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("%Protein") + 
  ggtitle("Wetland %Protein vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x.=0.5)+
  stat_cor(label.x.npc = 0.75)

#2.6.2 Protein Plot---------------------------------------
figureP <- ggarrange(Poverall,Pmean,
                       labels = c("A", "B"),
                       ncol = 1, nrow = 2)
figureP

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Duration Inundation, N events, % of year Saturated - using threshold of -0.3m ------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#Filter down data by generic horizon
O <- soildata %>% filter(Generic_Horizon == "1O")
A <- soildata %>% filter(Generic_Horizon == "2A")
B <- soildata %>% filter(Generic_Horizon == "3B")

### 4.1 EOC ###---------------------------------------
#4.1.1 EOC vs Dur ---------------------------------------
#horizon
EOCdur <- ggplot() +
  geom_point(data=O, aes(O_dur_day,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_day,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_day,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_day,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_day,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_day,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_day,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 25)+
  stat_regline_equation(data=A,aes(A_dur_day,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 24)+
  stat_regline_equation(data=B,aes(B_dur_day,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 23)+
  stat_cor(data=O,aes(O_dur_day,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 25)+
  stat_cor(data=A,aes(A_dur_day,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 24)+
  stat_cor(data=B,aes(B_dur_day,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 23)

#transect point
ggplot() +
  geom_point(data=O, aes(O_dur_day,EOC_mgC_L,col=station),size=4)+
  geom_point(data=A, aes(A_dur_day,EOC_mgC_L,col=station),size=4)+
  geom_point(data=B, aes(B_dur_day,EOC_mgC_L,col=station),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Duration") + 
  theme_bw()

#4.1.2 EOC vs N events ---------------------------------------
#horizon
EOCn <- ggplot() +
  geom_point(data=O, aes(O_n_events,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("N events in each horizon") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Number of Horizon Specific Saturation Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,EOC_mgC_L,col=Generic_Horizon),label.x = 1,label.y = 25)+
  stat_regline_equation(data=A,aes(A_n_events,EOC_mgC_L,col=Generic_Horizon),label.x = 1,label.y = 24)+
  stat_regline_equation(data=B,aes(B_n_events,EOC_mgC_L,col=Generic_Horizon),label.x = 1,label.y = 23)+
  stat_cor(data=O,aes(O_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 25)+
  stat_cor(data=A,aes(A_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 24)+
  stat_cor(data=B,aes(B_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 23)

#transect point
EOCstat <- ggplot() +
  geom_point(data=O, aes(O_n_events,EOC_mgC_L,col=station),size=4)+
  geom_point(data=A, aes(A_n_events,EOC_mgC_L,col=station),size=4)+
  geom_point(data=B, aes(B_n_events,EOC_mgC_L,col=station),size=4)+
  xlab("N events in each horizon") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Number of Horizon Specific Saturation Events") + 
  theme_bw()


#4.1.3 % Yr Sat ---------------------------------------
#horizon
EOCpercent <- ggplot() +
  geom_point(data=O, aes(O_percent_sat,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_percent_sat,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_percent_sat,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("% year horizon saturated") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs % of year Horizon is Saturated") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_percent_sat,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_percent_sat,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_percent_sat,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_percent_sat,EOC_mgC_L,col=Generic_Horizon),label.x = 0.25,label.y = 25)+
  stat_regline_equation(data=A,aes(A_percent_sat,EOC_mgC_L,col=Generic_Horizon),label.x = 0.25,label.y = 24)+
  stat_regline_equation(data=B,aes(B_percent_sat,EOC_mgC_L,col=Generic_Horizon),label.x = 0.25,label.y = 23)+
  stat_cor(data=O,aes(O_percent_sat,EOC_mgC_L,col=Generic_Horizon),label.x = 0.5,label.y = 25)+
  stat_cor(data=A,aes(A_percent_sat,EOC_mgC_L,col=Generic_Horizon),label.x = 0.5,label.y = 24)+
  stat_cor(data=B,aes(B_percent_sat,EOC_mgC_L,col=Generic_Horizon),label.x = 0.5,label.y = 23)

#transect point
ggplot() +
  geom_point(data=O, aes(O_percent_sat,EOC_mgC_L,col=station),size=4)+
  geom_point(data=A, aes(A_percent_sat,EOC_mgC_L,col=station),size=4)+
  geom_point(data=B, aes(B_percent_sat,EOC_mgC_L,col=station),size=4)+
  xlab("% year horizon saturated") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs % of year Horizon is Saturated") + 
  theme_bw()

#4.1.4 Plot ---------------------------------------
figureEOC <- ggarrange( EOCstat, 
                        EOCdur,
                        EOCn,
                        EOCpercent,
                      labels = c("A", "B","C","D"),
                      ncol = 2, nrow = 2)
figureEOC

### 4.2 FI ###---------------------------------------
#4.2.1 FI vs Dur ---------------------------------------
#horizon
FIdur <- ggplot() +
  geom_point(data=O, aes(O_dur_day,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_day,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_day,FI,col=Generic_Horizon),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_day,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_day,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_day,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_day,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.89)+
  stat_regline_equation(data=A,aes(A_dur_day,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.87)+
  stat_regline_equation(data=B,aes(B_dur_day,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.85)+
  stat_cor(data=O,aes(O_dur_day,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.89)+
  stat_cor(data=A,aes(A_dur_day,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.87)+
  stat_cor(data=B,aes(B_dur_day,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.85)


#transect point
ggplot() +
  geom_point(data=O, aes(O_dur_day,FI,col=station),size=4)+
  geom_point(data=A, aes(A_dur_day,FI,col=station),size=4)+
  geom_point(data=B, aes(B_dur_day,FI,col=station),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Duration") + 
  theme_bw()

#4.2.2 FI vs N events ---------------------------------------
#horizon
FIn <- ggplot() +
  geom_point(data=O, aes(O_n_events,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,FI,col=Generic_Horizon),size=4)+
  xlab("N events in each horizon") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Number of Horizon Specific Saturation Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,FI,col=Generic_Horizon),label.x = 1,label.y = 1.89)+
  stat_regline_equation(data=A,aes(A_n_events,FI,col=Generic_Horizon),label.x = 1,label.y = 1.87)+
  stat_regline_equation(data=B,aes(B_n_events,FI,col=Generic_Horizon),label.x = 1,label.y = 1.85)+
  stat_cor(data=O,aes(O_n_events,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.89)+
  stat_cor(data=A,aes(A_n_events,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.87)+
  stat_cor(data=B,aes(B_n_events,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.85)

#transect point
FIstat <- ggplot() +
  geom_point(data=O, aes(O_n_events,FI,col=station),size=4)+
  geom_point(data=A, aes(A_n_events,FI,col=station),size=4)+
  geom_point(data=B, aes(B_n_events,FI,col=station),size=4)+
  xlab("N events in each horizon") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Number of Horizon Specific Saturation Events") + 
  theme_bw()


#4.2.3 FI vs % Yr Sat ---------------------------------------
#horizon
FIpercent <- ggplot() +
  geom_point(data=O, aes(O_percent_sat,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_percent_sat,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_percent_sat,FI,col=Generic_Horizon),size=4)+
  xlab("% year horizon saturated") +
  ylab("FI") + 
  ggtitle("Wetland FI vs % of year Horizon is Saturated") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_percent_sat,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_percent_sat,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_percent_sat,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_percent_sat,FI,col=Generic_Horizon),label.x = 0.25,label.y = 1.89)+
  stat_regline_equation(data=A,aes(A_percent_sat,FI,col=Generic_Horizon),label.x = 0.25,label.y = 1.87)+
  stat_regline_equation(data=B,aes(B_percent_sat,FI,col=Generic_Horizon),label.x = 0.25,label.y = 1.85)+
  stat_cor(data=O,aes(O_percent_sat,FI,col=Generic_Horizon),label.x = 0.5,label.y = 1.89)+
  stat_cor(data=A,aes(A_percent_sat,FI,col=Generic_Horizon),label.x = 0.5,label.y = 1.87)+
  stat_cor(data=B,aes(B_percent_sat,FI,col=Generic_Horizon),label.x = 0.5,label.y = 1.85)

#transect point
ggplot() +
  geom_point(data=O, aes(O_percent_sat,FI,col=station),size=4)+
  geom_point(data=A, aes(A_percent_sat,FI,col=station),size=4)+
  geom_point(data=B, aes(B_percent_sat,FI,col=station),size=4)+
  xlab("% year horizon saturated") +
  ylab("FI") + 
  ggtitle("Wetland FI vs % of year Horizon is Saturated") + 
  theme_bw()

#4.2.4 FI Plot ---------------------------------------
figureFI <- ggarrange( FIstat, 
                       FIdur,
                       FIn, 
                       FIpercent,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureFI

### 4.3 SUVA254 ###---------------------------------------
#4.3.1 SUVA vs Dur ---------------------------------------
#horizon
SUVAdur <- ggplot() +
  geom_point(data=O, aes(O_dur_day,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_day,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_day,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("SUVA254 (L/mg-m)") + 
  ggtitle("Wetland SUVA254 vs Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_day,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_day,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_day,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_day,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.6)+
  stat_regline_equation(data=A,aes(A_dur_day,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.5)+
  stat_regline_equation(data=B,aes(B_dur_day,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.4)+
  stat_cor(data=O,aes(O_dur_day,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.6)+
  stat_cor(data=A,aes(A_dur_day,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.5)+
  stat_cor(data=B,aes(B_dur_day,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.4)
SUVAdur

#transect point
ggplot() +
  geom_point(data=O, aes(O_dur_day,SUVA254_L_mgm,col=station),size=4)+
  geom_point(data=A, aes(A_dur_day,SUVA254_L_mgm,col=station),size=4)+
  geom_point(data=B, aes(B_dur_day,SUVA254_L_mgm,col=station),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("SUVA254 (L/mg-m)") + 
  ggtitle("Wetland SUVA254 vs Duration") + 
  theme_bw()

#4.3.2 SUVA vs N events ---------------------------------------
#horizon
SUVAn <- ggplot() +
  geom_point(data=O, aes(O_n_events,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  xlab("N events in each horizon") +
  ylab("SUVA254 (L/mg-m)") + 
  ggtitle("Wetland SUVA254 vs Number of Horizon Specific Saturation Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = "center",label.y = 3.0)+
  stat_regline_equation(data=A,aes(A_n_events,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = "center",label.y = 2.9)+
  stat_regline_equation(data=B,aes(B_n_events,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = "center",label.y = 2.8)+
  stat_cor(data=O,aes(O_n_events,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.75,label.y = 3.0)+
  stat_cor(data=A,aes(A_n_events,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.75,label.y = 2.9)+
  stat_cor(data=B,aes(B_n_events,SUVA254_L_mgm,col=Generic_Horizon),label.x.npc = 0.75,label.y = 2.8)
SUVAn

#transect point
SUVAstat <- ggplot() +
  geom_point(data=O, aes(O_n_events,SUVA254_L_mgm,col=station),size=4)+
  geom_point(data=A, aes(A_n_events,SUVA254_L_mgm,col=station),size=4)+
  geom_point(data=B, aes(B_n_events,SUVA254_L_mgm,col=station),size=4)+
  xlab("N events in each horizon") +
  ylab("SUVA254 (L/mg-m)") + 
  ggtitle("Wetland SUVA254 vs Number of Horizon Specific Saturation Events") + 
  theme_bw()


#4.3.3 SUVA254 vs % Yr Sat ---------------------------------------
#horizon
SUVApercent <- ggplot() +
  geom_point(data=O, aes(O_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),size=4)+
  xlab("% year horizon saturated") +
  ylab("SUVA254 (L/mg-m)") + 
  ggtitle("Wetland SUVA254 vs % of year Horizon is Saturated") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_percent_sat,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_percent_sat,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_percent_sat,SUVA254_L_mgm,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),label.x = 0.5,label.y = 1.0)+
  stat_regline_equation(data=A,aes(A_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),label.x = 0.5,label.y = 0.9)+
  stat_regline_equation(data=B,aes(B_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),label.x = 0.5,label.y = 0.8)+
  stat_cor(data=O,aes(O_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),label.x = 0.75,label.y = 1.0)+
  stat_cor(data=A,aes(A_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),label.x = 0.75,label.y = 0.9)+
  stat_cor(data=B,aes(B_percent_sat,SUVA254_L_mgm,col=Generic_Horizon),label.x = 0.75,label.y = 0.8)
SUVApercent

#transect point
ggplot() +
  geom_point(data=O, aes(O_percent_sat,SUVA254_L_mgm,col=station),size=4)+
  geom_point(data=A, aes(A_percent_sat,SUVA254_L_mgm,col=station),size=4)+
  geom_point(data=B, aes(B_percent_sat,SUVA254_L_mgm,col=station),size=4)+
  xlab("% year horizon saturated") +
  ylab("SUVA254 (L/mg-m)") + 
  ggtitle("Wetland SUVA vs % of year Horizon is Saturated") + 
  theme_bw()

#4.3.4 SUVA254 Plot ---------------------------------------
figureSUVA <- ggarrange( SUVAstat, 
                         SUVAdur, 
                         SUVAn, 
                         SUVApercent,
                       labels = c("A", "B","C","D"),
                       ncol = 2, nrow = 2)
figureSUVA

### 4.4 HIX ###---------------------------------------
#4.4.1 HIX vs Dur ---------------------------------------
#horizon
HIXdur <- ggplot() +
  geom_point(data=O, aes(O_dur_day,HIX,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_day,HIX,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_day,HIX,col=Generic_Horizon),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_day,HIX,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_day,HIX,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_day,HIX,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_day,HIX,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.60)+
  stat_regline_equation(data=A,aes(A_dur_day,HIX,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.59)+
  stat_regline_equation(data=B,aes(B_dur_day,HIX,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.58)+
  stat_cor(data=O,aes(O_dur_day,HIX,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.60)+
  stat_cor(data=A,aes(A_dur_day,HIX,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.59)+
  stat_cor(data=B,aes(B_dur_day,HIX,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.58)
HIXdur

#transect point
ggplot() +
  geom_point(data=O, aes(O_dur_day,HIX,col=station),size=4)+
  geom_point(data=A, aes(A_dur_day,HIX,col=station),size=4)+
  geom_point(data=B, aes(B_dur_day,HIX,col=station),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Duration") + 
  theme_bw()

#4.4.2 HIX vs N events ---------------------------------------
#horizon
HIXn <- ggplot() +
  geom_point(data=O, aes(O_n_events,HIX,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,HIX,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,HIX,col=Generic_Horizon),size=4)+
  xlab("N events in each horizon") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Number of Horizon Specific Saturation Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,HIX,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,HIX,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,HIX,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,HIX,col=Generic_Horizon),label.x.npc = "center",label.y = 0.60)+
  stat_regline_equation(data=A,aes(A_n_events,HIX,col=Generic_Horizon),label.x.npc = "center",label.y = 0.59)+
  stat_regline_equation(data=B,aes(B_n_events,HIX,col=Generic_Horizon),label.x.npc = "center",label.y = 0.58)+
  stat_cor(data=O,aes(O_n_events,HIX,col=Generic_Horizon),label.x.npc = 0.75,label.y = 0.60)+
  stat_cor(data=A,aes(A_n_events,HIX,col=Generic_Horizon),label.x.npc = 0.75,label.y = 0.59)+
  stat_cor(data=B,aes(B_n_events,HIX,col=Generic_Horizon),label.x.npc = 0.75,label.y = 0.58)
HIXn

#transect point
HIXstat <- ggplot() +
  geom_point(data=O, aes(O_n_events,HIX,col=station),size=4)+
  geom_point(data=A, aes(A_n_events,HIX,col=station),size=4)+
  geom_point(data=B, aes(B_n_events,HIX,col=station),size=4)+
  xlab("N events in each horizon") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Number of Horizon Specific Saturation Events") + 
  theme_bw()


#4.4.3 HIX vs % Yr Sat ---------------------------------------
#horizon
HIXpercent <- ggplot() +
  geom_point(data=O, aes(O_percent_sat,HIX,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_percent_sat,HIX,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_percent_sat,HIX,col=Generic_Horizon),size=4)+
  xlab("% year horizon saturated") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs % of year Horizon is Saturated") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_percent_sat,HIX,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_percent_sat,HIX,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_percent_sat,HIX,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_percent_sat,HIX,col=Generic_Horizon),label.x = 0.25,label.y = 0.60)+
  stat_regline_equation(data=A,aes(A_percent_sat,HIX,col=Generic_Horizon),label.x = 0.25,label.y = 0.59)+
  stat_regline_equation(data=B,aes(B_percent_sat,HIX,col=Generic_Horizon),label.x = 0.25,label.y = 0.58)+
  stat_cor(data=O,aes(O_percent_sat,HIX,col=Generic_Horizon),label.x = 0.5,label.y = 0.60)+
  stat_cor(data=A,aes(A_percent_sat,HIX,col=Generic_Horizon),label.x = 0.5,label.y = 0.59)+
  stat_cor(data=B,aes(B_percent_sat,HIX,col=Generic_Horizon),label.x = 0.5,label.y = 0.58)
HIXpercent

#transect point
ggplot() +
  geom_point(data=O, aes(O_percent_sat,HIX,col=station),size=4)+
  geom_point(data=A, aes(A_percent_sat,HIX,col=station),size=4)+
  geom_point(data=B, aes(B_percent_sat,HIX,col=station),size=4)+
  xlab("% year horizon saturated") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs % of year Horizon is Saturated") + 
  theme_bw()

#4.4.4 HIX Plot ---------------------------------------
figureHIX <- ggarrange( HIXstat, 
                        HIXdur, 
                        HIXn, 
                        HIXpercent,
                         labels = c("A", "B","C","D"),
                         ncol = 2, nrow = 2)
figureHIX

### 4.5 SSR ### -------------------------------------------
#4.5.1 SSR vs Dur --------------------------------------------
SSRdur <- ggplot() +
  geom_point(data=O, aes(O_dur_day,SSR,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_day,SSR,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_day,SSR,col=Generic_Horizon),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_day,SSR,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_day,SSR,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_day,SSR,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_day,SSR,col=Generic_Horizon),label.x.npc = 0.1,label.y = 4.0)+
  stat_regline_equation(data=A,aes(A_dur_day,SSR,col=Generic_Horizon),label.x.npc = 0.1,label.y = 3.9)+
  stat_regline_equation(data=B,aes(B_dur_day,SSR,col=Generic_Horizon),label.x.npc = 0.1,label.y = 3.8)+
  stat_cor(data=O,aes(O_dur_day,SSR,col=Generic_Horizon),label.x.npc = 0.3,label.y = 4.0)+
  stat_cor(data=A,aes(A_dur_day,SSR,col=Generic_Horizon),label.x.npc = 0.3,label.y = 3.9)+
  stat_cor(data=B,aes(B_dur_day,SSR,col=Generic_Horizon),label.x.npc = 0.3,label.y = 3.8)
SSRdur

#4.5.2 SSR vs N events ---------------------------------------
#horizon
SSRn <- ggplot() +
  geom_point(data=O, aes(O_n_events,SSR,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,SSR,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,SSR,col=Generic_Horizon),size=4)+
  xlab("N events in each horizon") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Number of Horizon Specific Saturation Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,SSR,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,SSR,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,SSR,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,SSR,col=Generic_Horizon),label.x.npc = "center",label.y = 4.0)+
  stat_regline_equation(data=A,aes(A_n_events,SSR,col=Generic_Horizon),label.x.npc = "center",label.y = 3.9)+
  stat_regline_equation(data=B,aes(B_n_events,SSR,col=Generic_Horizon),label.x.npc = "center",label.y = 3.8)+
  stat_cor(data=O,aes(O_n_events,SSR,col=Generic_Horizon),label.x.npc = 0.75,label.y = 4.0)+
  stat_cor(data=A,aes(A_n_events,SSR,col=Generic_Horizon),label.x.npc = 0.75,label.y = 3.9)+
  stat_cor(data=B,aes(B_n_events,SSR,col=Generic_Horizon),label.x.npc = 0.75,label.y = 3.8)
SSRn

#transect
SSRstat <- ggplot() +
  geom_point(data=O, aes(O_n_events,SSR,col=station),size=4)+
  geom_point(data=A, aes(A_n_events,SSR,col=station),size=4)+
  geom_point(data=B, aes(B_n_events,SSR,col=station),size=4)+
  xlab("N events in each horizon") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs Number of Horizon Specific Saturation Events") + 
  theme_bw()
SSRstat

#4.5.3 SSR vs % yr Sat ---------------------------------------
SSRpercent <- ggplot() +
  geom_point(data=O, aes(O_percent_sat,SSR,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_percent_sat,SSR,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_percent_sat,SSR,col=Generic_Horizon),size=4)+
  xlab("% year horizon saturated") +
  ylab("SSR") + 
  ggtitle("Wetland SSR vs % of year Horizon is Saturated") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_percent_sat,SSR,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_percent_sat,SSR,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_percent_sat,SSR,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_percent_sat,SSR,col=Generic_Horizon),label.x = 0.25,label.y = 4.0)+
  stat_regline_equation(data=A,aes(A_percent_sat,SSR,col=Generic_Horizon),label.x = 0.25,label.y = 3.9)+
  stat_regline_equation(data=B,aes(B_percent_sat,SSR,col=Generic_Horizon),label.x = 0.25,label.y = 3.8)+
  stat_cor(data=O,aes(O_percent_sat,SSR,col=Generic_Horizon),label.x = 0.5,label.y = 4.0)+
  stat_cor(data=A,aes(A_percent_sat,SSR,col=Generic_Horizon),label.x = 0.5,label.y = 3.9)+
  stat_cor(data=B,aes(B_percent_sat,SSR,col=Generic_Horizon),label.x = 0.5,label.y = 3.8)
SSRpercent

#4.5.4 SSR Plot ---------------------------------------------
figureSSR <- ggarrange( SSRstat, 
                        SSRdur, 
                        SSRn, 
                        SSRpercent,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureSSR

### 4.6 %Protein ###---------------------------------------
#4.6.1 %Protein vs Dur --------------------------------------------
Pdur <- ggplot() +
  geom_point(data=O, aes(O_dur_day,Percent_Protein,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_day,Percent_Protein,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_day,Percent_Protein,col=Generic_Horizon),size=4)+
  xlab("Horizon Saturation Duration (d)") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_day,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_day,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_day,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_day,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.20)+
  stat_regline_equation(data=A,aes(A_dur_day,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.19)+
  stat_regline_equation(data=B,aes(B_dur_day,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.1,label.y = 0.18)+
  stat_cor(data=O,aes(O_dur_day,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.20)+
  stat_cor(data=A,aes(A_dur_day,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.19)+
  stat_cor(data=B,aes(B_dur_day,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.3,label.y = 0.18)
Pdur

#4.6.2 %Protein vs N events ---------------------------------------
#horizon
Pn <- ggplot() +
  geom_point(data=O, aes(O_n_events,Percent_Protein,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,Percent_Protein,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,Percent_Protein,col=Generic_Horizon),size=4)+
  xlab("N events in each horizon") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs Number of Horizon Specific Saturation Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,Percent_Protein,col=Generic_Horizon),label.x.npc = "left",label.y = 0.20)+
  stat_regline_equation(data=A,aes(A_n_events,Percent_Protein,col=Generic_Horizon),label.x.npc = "left",label.y = 0.19)+
  stat_regline_equation(data=B,aes(B_n_events,Percent_Protein,col=Generic_Horizon),label.x.npc = "left",label.y = 0.18)+
  stat_cor(data=O,aes(O_n_events,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.25,label.y = 0.20)+
  stat_cor(data=A,aes(A_n_events,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.25,label.y = 0.19)+
  stat_cor(data=B,aes(B_n_events,Percent_Protein,col=Generic_Horizon),label.x.npc = 0.25,label.y = 0.18)
Pn

#transect
Pstat <- ggplot() +
  geom_point(data=O, aes(O_n_events,Percent_Protein,col=station),size=4)+
  geom_point(data=A, aes(A_n_events,Percent_Protein,col=station),size=4)+
  geom_point(data=B, aes(B_n_events,Percent_Protein,col=station),size=4)+
  xlab("N events in each horizon") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs Number of Horizon Specific Saturation Events") + 
  theme_bw()
Pstat

#4.6.3 %Protein vs % yr Sat ---------------------------------------
Ppercent <- ggplot() +
  geom_point(data=O, aes(O_percent_sat,Percent_Protein,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_percent_sat,Percent_Protein,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_percent_sat,Percent_Protein,col=Generic_Horizon),size=4)+
  xlab("% year horizon saturated") +
  ylab("% Protein") + 
  ggtitle("Wetland % Protein vs % of year Horizon is Saturated") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_percent_sat,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_percent_sat,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_percent_sat,Percent_Protein,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_percent_sat,Percent_Protein,col=Generic_Horizon),label.x = 0,label.y = 0.20)+
  stat_regline_equation(data=A,aes(A_percent_sat,Percent_Protein,col=Generic_Horizon),label.x = 0,label.y = 0.19)+
  stat_regline_equation(data=B,aes(B_percent_sat,Percent_Protein,col=Generic_Horizon),label.x = 0,label.y = 0.18)+
  stat_cor(data=O,aes(O_percent_sat,Percent_Protein,col=Generic_Horizon),label.x = 0.25,label.y = 0.20)+
  stat_cor(data=A,aes(A_percent_sat,Percent_Protein,col=Generic_Horizon),label.x = 0.25,label.y = 0.19)+
  stat_cor(data=B,aes(B_percent_sat,Percent_Protein,col=Generic_Horizon),label.x = 0.25,label.y = 0.18)
Ppercent

#4.6.4 %Protein Plot ---------------------------------------------
figureP <- ggarrange(   Pstat, 
                        Pdur, 
                        Pn, 
                        Ppercent,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureP

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Event duration -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Filter down data by generic horizon
O <- eventdata %>% filter(Generic_Horizon == "1O")
A <- eventdata %>% filter(Generic_Horizon == "2A")
B <- eventdata %>% filter(Generic_Horizon == "3B")

### 5.1 EOC ###---------------------------------------
#5.1.1 EOC vs Mean Event Dur ------------------------------------
#horizon
EOCdurmean <- ggplot() +
  geom_point(data=O, aes(O_dur_mean,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_mean,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_mean,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("Horizon Mean Event Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean Event Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_mean,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_mean,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_mean,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_mean,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 25)+
  stat_regline_equation(data=A,aes(A_dur_mean,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 24)+
  stat_regline_equation(data=B,aes(B_dur_mean,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 23)+
  stat_cor(data=O,aes(O_dur_mean,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 25)+
  stat_cor(data=A,aes(A_dur_mean,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 24)+
  stat_cor(data=B,aes(B_dur_mean,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 23)

#5.1.2 EOC vs Min Event Dur ------------------------------------
EOCdurmin <- ggplot() +
  geom_point(data=O, aes(O_dur_min,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_min,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_min,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("Horizon Mininum Event Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Min Event Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_min,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_min,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_min,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_min,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 25)+
  stat_regline_equation(data=A,aes(A_dur_min,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 24)+
  stat_regline_equation(data=B,aes(B_dur_min,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 23)+
  stat_cor(data=O,aes(O_dur_min,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 25)+
  stat_cor(data=A,aes(A_dur_min,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 24)+
  stat_cor(data=B,aes(B_dur_min,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 23)

#5.1.3 EOC vs Max Event Dur ------------------------------------
EOCdurmax <- ggplot() +
  geom_point(data=O, aes(O_dur_max,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_max,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_max,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("Horizon Maximum Event Duration (d)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Max Event Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_max,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_max,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_max,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_max,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 25)+
  stat_regline_equation(data=A,aes(A_dur_max,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 24)+
  stat_regline_equation(data=B,aes(B_dur_max,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 23)+
  stat_cor(data=O,aes(O_dur_max,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 25)+
  stat_cor(data=A,aes(A_dur_max,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 24)+
  stat_cor(data=B,aes(B_dur_max,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 23)

#5.1.4 EOC vs N Event ------------------------------------
EOCnevent <- ggplot() +
  geom_point(data=O, aes(O_n_events,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,EOC_mgC_L,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,EOC_mgC_L,col=Generic_Horizon),size=4)+
  xlab("Horizon N Events") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs N Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,EOC_mgC_L,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 25)+
  stat_regline_equation(data=A,aes(A_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 24)+
  stat_regline_equation(data=B,aes(B_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "left",label.y = 23)+
  stat_cor(data=O,aes(O_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 25)+
  stat_cor(data=A,aes(A_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 24)+
  stat_cor(data=B,aes(B_n_events,EOC_mgC_L,col=Generic_Horizon),label.x.npc = "center",label.y = 23)

### 5.2 FI ###---------------------------------------
#5.2.1 FI vs Mean Event Dur ------------------------------------
#horizon
FIdurmean <- ggplot() +
  geom_point(data=O, aes(O_dur_mean,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_mean,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_mean,FI,col=Generic_Horizon),size=4)+
  xlab("Horizon Mean Event Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean Event Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_mean,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_mean,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_mean,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_mean,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 2.0)+
  stat_regline_equation(data=A,aes(A_dur_mean,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.9)+
  stat_regline_equation(data=B,aes(B_dur_mean,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.8)+
  stat_cor(data=O,aes(O_dur_mean,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 2.0)+
  stat_cor(data=A,aes(A_dur_mean,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.9)+
  stat_cor(data=B,aes(B_dur_mean,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.8)

#5.2.2 FI vs Min Event Dur ------------------------------------
FIdurmin <- ggplot() +
  geom_point(data=O, aes(O_dur_min,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_min,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_min,FI,col=Generic_Horizon),size=4)+
  xlab("Horizon Mininum Event Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Min Event Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_min,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_min,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_min,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_min,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 2.0)+
  stat_regline_equation(data=A,aes(A_dur_min,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.9)+
  stat_regline_equation(data=B,aes(B_dur_min,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.8)+
  stat_cor(data=O,aes(O_dur_min,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 2.0)+
  stat_cor(data=A,aes(A_dur_min,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.9)+
  stat_cor(data=B,aes(B_dur_min,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.8)

#5.2.3 FI vs Max Event Dur ------------------------------------
FIdurmax <- ggplot() +
  geom_point(data=O, aes(O_dur_max,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_dur_max,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_dur_max,FI,col=Generic_Horizon),size=4)+
  xlab("Horizon Maximum Event Duration (d)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Max Event Duration") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_dur_max,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_dur_max,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_dur_max,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_dur_max,FI,col=Generic_Horizon),label.x.npc = "left",label.y =2.0)+
  stat_regline_equation(data=A,aes(A_dur_max,FI,col=Generic_Horizon),label.x.npc = "left",label.y =1.9)+
  stat_regline_equation(data=B,aes(B_dur_max,FI,col=Generic_Horizon),label.x.npc = "left",label.y =1.8)+
  stat_cor(data=O,aes(O_dur_max,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 2.0)+
  stat_cor(data=A,aes(A_dur_max,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.9)+
  stat_cor(data=B,aes(B_dur_max,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.8)

#5.2.4 FI vs N Event ------------------------------------
FInevent <- ggplot() +
  geom_point(data=O, aes(O_n_events,FI,col=Generic_Horizon),size=4)+
  geom_point(data=A, aes(A_n_events,FI,col=Generic_Horizon),size=4)+
  geom_point(data=B, aes(B_n_events,FI,col=Generic_Horizon),size=4)+
  xlab("Horizon N Events") +
  ylab("FI") + 
  ggtitle("Wetland FI vs N Events") + 
  theme_bw()+
  geom_smooth(data=O,aes(O_n_events,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=A,aes(A_n_events,FI,col=Generic_Horizon), method = 'lm')+
  geom_smooth(data=B,aes(B_n_events,FI,col=Generic_Horizon), method = 'lm')+
  stat_regline_equation(data=O,aes(O_n_events,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 2.0)+
  stat_regline_equation(data=A,aes(A_n_events,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.9)+
  stat_regline_equation(data=B,aes(B_n_events,FI,col=Generic_Horizon),label.x.npc = "left",label.y = 1.8)+
  stat_cor(data=O,aes(O_n_events,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 2.0)+
  stat_cor(data=A,aes(A_n_events,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.9)+
  stat_cor(data=B,aes(B_n_events,FI,col=Generic_Horizon),label.x.npc = "center",label.y = 1.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Variability within transect point -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#separate transect points and get mean of data points (since you have 3 months of samples)
Upland <- data %>% filter(station == "KW-4U") %>% 
  group_by(wetland,station,Generic_Horizon) %>% 
  summarize(meanEOC = mean(EOC_mgC_L),
            meanFI = mean(FI),
            meanSUVA = mean(SUVA254_L_mgm),
            meanHIX=mean(HIX),
            meanSSR = mean(SSR),
            meanWL = mean(mean_waterLevel))
Trans <- data %>% filter(station == "KW-3T")%>% 
  group_by(wetland,station,Generic_Horizon) %>% 
  summarize(meanEOC = mean(EOC_mgC_L),
            meanFI = mean(FI),
            meanSUVA = mean(SUVA254_L_mgm),
            meanHIX=mean(HIX),
            meanSSR = mean(SSR),
            meanWL = mean(mean_waterLevel))
Edge <- data %>% filter(station == "KW-2E")%>% 
  group_by(wetland,station,Generic_Horizon) %>% 
  summarize(meanEOC = mean(EOC_mgC_L),
            meanFI = mean(FI),
            meanSUVA = mean(SUVA254_L_mgm),
            meanHIX=mean(HIX),
            meanSSR = mean(SSR),
            meanWL = mean(mean_waterLevel))
Wet <- data %>% filter(station == "KW-1W")%>% 
  group_by(wetland,station,Generic_Horizon) %>% 
  summarize(meanEOC = mean(EOC_mgC_L),
            meanFI = mean(FI),
            meanSUVA = mean(SUVA254_L_mgm),
            meanHIX=mean(HIX),
            meanSSR = mean(SSR),
            meanWL = mean(mean_waterLevel))

All <- data %>%
  group_by(wetland,station,Generic_Horizon) %>% 
  summarize(meanEOC = mean(EOC_mgC_L),
            meanFI = mean(FI),
            meanSUVA = mean(SUVA254_L_mgm),
            meanHIX=mean(HIX),
            meanSSR = mean(SSR),
            meanWL = mean(mean_waterLevel))

# 6.1 EOC -----------------------------------
#Upland
UpEOC <- ggplot(Upland, aes(meanWL,meanEOC,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Upland EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.1)+
  #stat_cor(label.x = -1.05)
#Transition
TransEOC <- ggplot(Trans, aes(meanWL,meanEOC,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Transition EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.0)+
  #stat_cor(label.x = -0.9)
#Edge
EdgeEOC <- ggplot(Edge, aes(meanWL,meanEOC,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Edge EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.0)#+
  #stat_cor(label.x = -0.9)
#Wetland
WetlandEOC <- ggplot(Wet, aes(meanWL,meanEOC,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)

figureEOC <- ggarrange( UpEOC, 
                        TransEOC, 
                        EdgeEOC, 
                        WetlandEOC,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureEOC

#Don't separate horizons
#regression
ggplot(All, aes(meanWL,meanEOC,col=station)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Mean EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.0)+
  stat_cor(label.x = -0.5)+
    theme(legend.text = element_text(size=16),
      axis.text.y   = element_text(size=16),
      axis.text.x   = element_text(size=16),
      axis.title.y  = element_text(size=16),
      axis.title.x  = element_text(size=16),
      panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#look at wetlands
ggplot(All, aes(meanWL,meanEOC,col=station,shape=wetland)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Mean EOC vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.0)+
  stat_cor(label.x = -0.5)+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


ggplot(All, aes(x=reorder(station,meanWL,FUN=mean),y=meanEOC,fill=wetland)) +
  geom_boxplot() +
  xlab("Station") +
  ylab("EOC (mg/L)") + 
  ggtitle("EOC vs Mean WL") + 
  theme_bw()

# 6.2 FI -------------------------------------
#Upland
UpFI <- ggplot(Upland, aes(meanWL,meanFI,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Upland FI vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.1)+
  #stat_cor(label.x = -1.05)
#Transition
TransFI <- ggplot(Trans, aes(meanWL,meanFI,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI)") + 
  ggtitle("Transition FI vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.0)+
  #stat_cor(label.x = -0.9)
#Edge
EdgeFI <- ggplot(Edge, aes(meanWL,meanFI,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Edge FI vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)
#Wetland
WetlandFI <- ggplot(Wet, aes(meanWL,meanFI,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("Wetland FI vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
  #stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)

figureFI <- ggarrange(  UpFI, 
                        TransFI, 
                        EdgeFI, 
                        WetlandFI,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureFI

#Don't separate horizons
ggplot(All, aes(meanWL,meanFI,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean FI") + 
  ggtitle("Mean FI vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.0)+
  stat_cor(label.x = -0.5)+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(All, aes(meanWL,meanFI,col=station,shape=wetland)) +
  geom_point(size=4) +
  xlab("Mean Water Elev (m)") +
  ylab("FI") + 
  ggtitle("FI vs Mean WL") + 
  theme_bw() 


# 6.3 SUVA -------------------------------------
#Upland
UpSUVA <- ggplot(Upland, aes(meanWL,meanSUVA,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA") + 
  ggtitle("Upland SUVA vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.1)+
#stat_cor(label.x = -1.05)
#Transition
TransSUVA <- ggplot(Trans, aes(meanWL,meanSUVA,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA254") + 
  ggtitle("Transition SUVA vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.0)+
#stat_cor(label.x = -0.9)
#Edge
EdgeSUVA <- ggplot(Edge, aes(meanWL,meanSUVA,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA254") + 
  ggtitle("Edge SUVA vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)
#Wetland
WetlandSUVA <- ggplot(Wet, aes(meanWL,meanSUVA,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA254") + 
  ggtitle("Wetland SUVA vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)

figureSUVA <- ggarrange(  UpSUVA, 
                        TransSUVA, 
                        EdgeSUVA, 
                        WetlandSUVA,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2)
figureSUVA

#Don't separate horizons
ggplot(All, aes(meanWL,meanSUVA,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean SUVA254") + 
  ggtitle("Mean SUVA254 vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.0)+
  stat_cor(label.x = -0.5)+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(All, aes(meanWL,meanSUVA,col=station,shape=wetland)) 
  geom_point(size=4) +
  xlab("Mean Water Elev (m)") +
  ylab("SUVA254") + 
  ggtitle("SUVA254 vs Mean WL") + 
  theme_bw() 

# 6.4 HIX -------------------------------------
#Upland
UpHIX<- ggplot(Upland, aes(meanWL,meanHIX,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Upland HIX vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.1)+
#stat_cor(label.x = -1.05)
#Transition
TransHIX <- ggplot(Trans, aes(meanWL,meanHIX,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Transition HIX vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.0)+
#stat_cor(label.x = -0.9)
#Edge
EdgeHIX <- ggplot(Edge, aes(meanWL,meanHIX,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Edge HIX vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)
#Wetland
WetlandHIX <- ggplot(Wet, aes(meanWL,meanHIX,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("Wetland HIX vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')#+
#stat_regline_equation(label.x = -1.0)#+
#stat_cor(label.x = -0.9)

figureHIX <- ggarrange(  UpHIX, 
                          TransHIX, 
                          EdgeHIX, 
                          WetlandHIX,
                          labels = c("A", "B","C","D"),
                          ncol = 2, nrow = 2)
figureHIX


#dont separate horizons
ggplot(All, aes(meanWL,meanHIX,col=station)) +
  geom_point(size=2.5) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("HIX vs Mean WL") + 
  theme_bw() +
  geom_smooth(method = 'lm')+
  stat_regline_equation(label.x = -1.0)+
  stat_cor(label.x = -0.5)
ggplot(All, aes(meanWL,meanHIX,col=station,shape=wetland)) +
  geom_point(size=4) +
  xlab("Mean Water Elev (m)") +
  ylab("HIX") + 
  ggtitle("HIX vs Mean WL") + 
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#7.0 Variability between wetlands-------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC vs Mean WL colored by wetland
ggplot(All, aes(meanWL,meanEOC,col=Generic_Horizon,shape=wetland)) +
  geom_point(size=4) +
  xlab("Mean Water Elev (m)") +
  ylab("Mean EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


#mean water level along transect colored by wetland
ggplot(annual, aes(station,mean_waterLevel,col=wetland)) +
  geom_point(size=4) +
  xlab("Transect Station") +
  ylab("Mean Water Elev (m)") + 
  ggtitle("Mean Water Level by Wetland") + 
  theme_bw()

#min water level along transect colored by wetland
ggplot(annual, aes(station,min_waterLevel,col=wetland)) +
  geom_point(size=4) +
  xlab("Transect Station") +
  ylab("Min Water Elev (m)") + 
  ggtitle("Min Water Level by Wetland") + 
  theme_bw()

#max water level along transect colored by wetland
ggplot(annual, aes(station,max_waterLevel,col=wetland)) +
  geom_point(size=4) +
  xlab("Transect Station") +
  ylab("Max Water Elev (m)") + 
  ggtitle("Max Water Level by Wetland") + 
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#8.0 Other Plots -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##7.1 CV of Water Level-----------------------------
#CV by transect point
ggplot(data, aes(station,CV_waterLevel,col=station))+
  geom_point(size=4)+
  xlab("Transect Point") +
  ylab("CV") + 
  ggtitle("Coefficient of Variation By Transect Point") + 
  theme_bw()+
  facet_wrap(~wetland)

#7.2 Potential vs realized DOM---------------------------------

subset <- data %>% dplyr::select(wetland,station,Generic_Horizon,EOC_mgC_L,Layer_Thickness_cm, 
                               min_waterLevel,mean_waterLevel,median_waterLevel,max_waterLevel,
                               dur_day,n_events)
subset <- subset %>% mutate(Bulk_EOC = EOC_mgC_L*Layer_Thickness_cm,
                            EOC_mgC_gSoil = (EOC_mgC_L/1000)/30,
                            EOC_Realized = EOC_mgC_L*abs(mean_waterLevel))














