#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Statistical Analyses
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 4/9/2021
#Updated: 
#Purpose: Evaluate statistical significance of ESOM and Water Level Metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(dplyr)
library(psych)
library(pgirmess)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(car)
library(ggpmisc)
library(performance)
library(agricolae)

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv")
threshold_annual <- read_csv("data/annual_metrics_2020_threshold.csv")
soil_annual <- read_csv("data/horizon_annual_metrics_2020.csv")
waterlevel <- read_csv("data/waterLevel_at_sampling_location.csv")
elev <- read_csv("data/xs_survey.csv")

#Filter to just wetlands and no leaf litter
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")

#Join metrics and ESOM data
data <- inner_join(df, threshold_annual, by=c("wetland","station"))

horiz_metrics <- inner_join(df,soil_annual,by=c("wetland","station"))

#Filter water level data to just 2020 water year
waterlevel <- waterlevel %>% filter(Timestamp > "2019-10-01" & Timestamp < "2020-10-01")

#Join elevation data to threshold annual metrics
threshold_annual <- left_join(threshold_annual,elev,by=c("wetland","station"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Exploratory Data Analysis--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Look at distributions/normality/equal variance 

###ESOM DATA###
#Define station vs horizon
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon

#Summarize data
#By horizon
ESOM_Horizon_Summary <- WetlandsNoLL %>% 
        group_by(Generic_Horizon) %>% 
        summarise( MeanEOC = mean(EOC_mgC_L),
                   sdEOC = sd(EOC_mgC_L),
                   MeanFI = mean(FI),
                   sdFI = sd(FI),
                   MeanSUVA = mean(SUVA254_L_mgm),
                   sdSUVA = sd(SUVA254_L_mgm),
                   MeanHIX = mean(HIX),
                   sdHIX = sd(HIX),
                   MeanSSR = mean(SSR,na.rm=T),
                   sdSSR = sd(SSR,na.rm = T))
#By station
ESOM_Station_Summary <- WetlandsNoLL %>% 
  group_by(station) %>% 
  summarise( MeanEOC = mean(EOC_mgC_L),
             sdEOC = sd(EOC_mgC_L),
             MeanFI = mean(FI),
             sdFI = sd(FI),
             MeanSUVA = mean(SUVA254_L_mgm),
             sdSUVA = sd(SUVA254_L_mgm),
             MeanHIX = mean(HIX),
             sdHIX = sd(HIX),
             MeanSSR = mean(SSR,na.rm=T),
             sdSSR = sd(SSR,na.rm = T))

### Water Level Metrics###
#by station
threshold_annual_summary <-threshold_annual %>% 
  #Group by wetland and sampling station
  group_by(station) %>% 
  #Summarise!
  summarise(#elev
    mean_elev =  mean(elevation),
    sd_elev = sd(elevation),
    se_elev = sd_elev/sqrt(4),
    #min
    mean_minWL =  mean(min_waterLevel),
    sd_minWL = sd(min_waterLevel),
    se_minWL = sd_minWL/sqrt(4),
    #mean
    mean_meanWL = mean(mean_waterLevel),
    sd_meanWL = sd(mean_waterLevel),
    se_meanWL = sd_meanWL/sqrt(4),
    #median
    mean_medianWL = mean(median_waterLevel),
    sd_medianWL = sd(median_waterLevel),
    se_medianWL = sd_medianWL/sqrt(4),
    #max
    mean_maxWL = mean(max_waterLevel),
    sd_maxWL = sd(max_waterLevel),
    se_maxWL = sd_maxWL/sqrt(4),
    #dur_day
    mean_durday = mean(dur_day),
    sd_durday = sd(dur_day),
    se_durday = sd_durday/sqrt(4),
    #n_events
    mean_nevents = mean(n_events),
    sd_nevents = sd(n_events),
    se_nevents = sd_nevents/sqrt(4),
    #percent sat
    mean_percentsat = mean(percent_sat),
    sd_percentsat = sd(percent_sat),
    sd_percentsat = sd_percentsat/sqrt(4)
  )

#filter metrics
station <- threshold_annual$station
elev <-    threshold_annual$elevation
minWL <-   threshold_annual$min_waterLevel
meanWL <-  threshold_annual$mean_waterLevel
medianWL <-threshold_annual$median_waterLevel
maxWL <-   threshold_annual$max_waterLevel
durday <-  threshold_annual$dur_day
nevents <- threshold_annual$n_events

#N events + sat duration by horizon
soil_summary <- soil_annual %>% group_by(station) %>% 
  summarise(#O horizon
            O_dur_day_mean = mean(O_dur_day,na.rm=T),
            O_dur_day_sd = sd(O_dur_day,na.rm=T),
            O_nevents_mean = mean(O_n_events,na.rm=T),
            O_nevents_sd = sd(O_n_events,na.rm=T),
            #A horizon
            A_dur_day_mean = mean(A_dur_day,na.rm=T),
            A_dur_day_sd = sd(A_dur_day,na.rm=T),
            A_nevents_mean = mean(A_n_events,na.rm=T),
            A_nevents_sd = sd(A_n_events,na.rm=T),
            #B horizon
            B_dur_day_mean = mean(B_dur_day,na.rm=T),
            B_dur_day_sd = sd(B_dur_day,na.rm=T),
            B_nevents_mean = mean(B_n_events,na.rm=T),
            B_nevents_sd = sd(B_n_events,na.rm=T))

### 2.1 EOC -----------------------------------
hist(WetlandsNoLL$EOC_mgC_L)
qqnorm(WetlandsNoLL$EOC_mgC_L)
shapiro.test(WetlandsNoLL$EOC_mgC_L) #EOC values are not normally distributed

#Log transform data
#EOC
logEOC <- log10(WetlandsNoLL$EOC_mgC_L)
qqnorm(logEOC)

#test for normal distribution of log transformed data
shapiro.test(logEOC) #Log of values is normally distributed

#test for equal variance
bartlett.test(logEOC~Station) #Barlett test doesn't meet equal variance assumptions but barely, Dr G says ok to proceed 
bartlett.test(logEOC~Horizon) 
leveneTest(logEOC,Station,center=median) #Levene test doesn't meet equal variance assumptions
leveneTest(logEOC,Horizon,center=median)


### 2.2 FI -----------------------------------
FI <- WetlandsNoLL$FI 
qqnorm(FI)
hist(FI)
shapiro.test(FI) #not normally distributed
#horizon
bartlett.test(FI~Horizon) #no equal variance
leveneTest(FI,Horizon,center=median) #no equal variance - fail barlett, levene, shapiro - use oneway.test? 
#station
bartlett.test(FI~Station) #equal variance
leveneTest(FI,Station,center=median)

#transform data
logFI <- log10(FI)
hist(logFI)
qqnorm(logFI)
shapiro.test(logFI) #logging FI data is not normally distributed
bartlett.test(logFI~Horizon)

### 2.3 SUVA -----------------------------------
SUVA <- WetlandsNoLL$SUVA254_L_mgm
hist(SUVA)
shapiro.test(SUVA) #not normally distributed
#horizon
bartlett.test(SUVA~Horizon)
leveneTest(SUVA,Horizon,center=median) #passes levene but fails bartlett and normality - use kruskal wallis
#station
bartlett.test(SUVA~Station)
leveneTest(SUVA,Station,center=median) #passes levene/bartlett but not normality - use kruskal wallis


#transform data
sqrtSUVA <- sqrt(SUVA) 
hist(sqrtSUVA)
shapiro.test(sqrtSUVA) #not normally distributed


### 2.4 HIX -----------------------------------
HIX <- WetlandsNoLL$HIX
hist(HIX)
shapiro.test(HIX) #not normally distributed
#horizon
bartlett.test(HIX~Horizon) #no equal variance 
leveneTest(HIX,Horizon,center=median) #no equal variance - fails bartlett/levene and normality - use oneway.test?
#station
bartlett.test(HIX~Station) #just barely fails 
leveneTest(HIX,Station,center=median) # equal variance - use kursal wallis

#transform data
sqrtHIX <- sqrt(HIX)
hist(sqrtHIX) 
shapiro.test(sqrtHIX) #not normally distributed
bartlett.test(sqrtHIX~Horizon)
leveneTest(sqrtHIX,Horizon,center=median)

### 2.5 SSR -----------------------------------
SSR <- WetlandsNoLL$SSR
hist(SSR)
shapiro.test(SSR) #not normally distributed
#horizon
bartlett.test(SSR~Horizon)#no equal variance
leveneTest(SSR,Horizon,center=median) #no equal variance - fails bartlett/levene and normality - use oneway.test?
#station
bartlett.test(SSR~Station)#no equal variance
leveneTest(SSR,Station,center=median) #no equal variance - fails bartlett/levene and normality - use oneway.test?


#transform data
logSSR <- log10(SSR)
hist(logSSR)
shapiro.test(logSSR)
bartlett.test(logSSR~Horizon)#no equal variance
leveneTest(logSSR,Horizon,center=median) 

### 2.6 Water Level Data -----------------------------------
#break out variables
y_n <- waterlevel$y_n
station <- waterlevel$station

#distribution and normality test
hist(y_n) #looks normally distributed overall
qqnorm(y_n) #ends of plot are wonky but overall looks ok
shapiro.test(y_n) #shapiro doesn't work when n>5,000 :(

#test for equal variance
bartlett.test(y_n~station) #big fail p = 9.4x10^-5
leveneTest(y_n,station,center=median) #p = 5.97x10-7

### 2.7 2020 Annual Metrics ----------------------------------
#elev
qqnorm(elev)
shapiro.test(elev) #normal
bartlett.test(elev~station) #equal variance
#min water level
qqnorm(minWL)
shapiro.test(minWL) #normal
bartlett.test(minWL~station) #equal variance
#mean water level
qqnorm(meanWL)
shapiro.test(meanWL) #normal
bartlett.test(meanWL~station) #equal variance
#median water level
qqnorm(medianWL)
shapiro.test(medianWL) #normal
bartlett.test(medianWL~station) #equal variance
#max water level
qqnorm(maxWL)
shapiro.test(maxWL) #normal
bartlett.test(maxWL~station) #equal variance
#duration
qqnorm(durday)
logdurday <- log(durday)
shapiro.test(durday) #not normally distributed
bartlett.test(durday~station) #not equal variance
leveneTest(durday,station,center=median) #no equal variance with levene - use onway
#n events
qqnorm(nevents)
shapiro.test(nevents) #not normally distributed
bartlett.test(nevents~station) #not equal variance
leveneTest(nevents,station,center=median) #no equal variance levene - use oneway

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 ANOVA/TukeyHSD/Kruskal-Wallis/Oneway Test --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 3.1 EOC --------------------------------------------------
#3.1.1 EOC by horizon------------------------------------------
logEOC.horiz.aov <- aov(logEOC~Horizon)
summary(logEOC.horiz.aov)
logEOC.horiz.HSD <- TukeyHSD(logEOC.horiz.aov);logEOC.horiz.HSD
logEOCL.HSD <- HSD.test(logEOC.horiz.aov,"Horizon",group=T);logEOCL.HSD 
#one-way anova test for unequal variance
oneway.test(logEOC~Horizon)

#3.1.2 EOC by station------------------------------------------
logEOC.sta.aov <- aov(logEOC~Station)
summary(logEOC.sta.aov)
logEOC.sta.HSD <- TukeyHSD(logEOC.sta.aov);logEOC.sta.HSD 
logEOC.sta.HSD <- HSD.test(logEOC.sta.aov,"Station",group=T);logEOC.sta.HSD 
#one-way anova test for unequal variance
oneway.test(logEOC~Station)

#3.1.3 EOC by both horizon and transect location (ANOCVA) --------------------
logEOC.both.aov <- aov(logEOC~Horizon+Station)
summary(logEOC.both.aov)
logEOC.both.HSD <- TukeyHSD(logEOC.both.aov);logEOC.both.HSD  
logEOC.both.HSD <- HSD.test(logEOC.both.aov,trt = c("Horizon", "Station"),group=T);logEOC.both.HSD  

### 3.2 FI -------------------------------------------
#3.2.1 FI by horizon------------------------------------------
oneway.test(FI~Horizon)
pairwise.t.test(FI,Horizon,p.adj="bonferroni") #O not diff from A but all other comparisons significant
#3.2.2 FI by station------------------------------------------
oneway.test(FI~Station)
pairwise.t.test(FI,Station,p.adj="bonferroni") 

### 3.3 SUVA ------------------------------------------
#3.3.1 SVUA by horizon------------------------------------------
kruskal.test(SUVA~Horizon)
kruskalmc(SUVA, Horizon, probs=0.05) #O and A not diff, but all others yes
#3.3.1 SVUA by station------------------------------------------
kruskal.test(SUVA~Station)
kruskalmc(SUVA,Station, probs=0.05) 


### 3.4 HIX ------------------------------------------
#3.4.1 HIX by horizon------------------------------------------
oneway.test(HIX~Horizon)
pairwise.t.test(HIX,Horizon,p.adj="bonferroni") #O not diff from A but all other comparisons significant
#3.4.1 HIX by station------------------------------------------
kruskal.test(HIX~Station)
kruskalmc(HIX,Station, probs=0.05) #W and E / T and U not diff, but all others yes

### 3.5 SSR ------------------------------------------
#3.5.1 SSR by horizon------------------------------------------
oneway.test(SSR~Horizon)
pairwise.t.test(SSR,Horizon,p.adj="bonferroni") #O not diff from A but all other comparisons significant
#3.5.1 SSR by station------------------------------------------
kruskal.test(SSR~Station)
kruskalmc(SSR,Station, probs=0.05) #edge and upland only different 


### 3.6 Water Level Data--------------------------------
waterlevel.aov <- aov(y_n~station)
summary(waterlevel.aov)
waterlevel.HSD <- TukeyHSD(waterlevel.aov);waterlevel.HSD 

### 3.7 2020 Water Level Metrics--------------------------------
station <- threshold_annual$station
elev <-    threshold_annual$elevation
minWL <-   threshold_annual$min_waterLevel
meanWL <-  threshold_annual$mean_waterLevel
medianWL <-threshold_annual$median_waterLevel
maxWL <-   threshold_annual$max_waterLevel
durday <-  threshold_annual$dur_day
nevents <- threshold_annual$n_events

#elev
elev.aov <-    aov(elev~station);summary(elev.aov)
elev.HSD <- TukeyHSD(elev.aov);elev.HSD
elev.HSD <- HSD.test(elev.aov,"station",group=T);elev.HSD 
#min water level
minWL.aov <-   aov(minWL~station); summary(minWL.aov)
minWL.HSD <-   TukeyHSD(minWL.aov); minWL.HSD
minWL.HSD <- HSD.test(elev.aov,"station",group=T);minWL.HSD
#mean water level
meanWL.aov <-  aov(meanWL~station); summary(meanWL.aov)
meanWL.HSD <-   TukeyHSD(meanWL.aov); meanWL.HSD
meanWL.HSD <- HSD.test(meanWL.aov,"station",group=T);meanWL.HSD
#median water level
medianWL.aov <-aov(medianWL~station); summary(medianWL.aov)
medianWL.HSD <-   TukeyHSD(medianWL.aov); medianWL.HSD
medianWL.HSD <- HSD.test(medianWL.aov,"station",group=T);medianWL.HSD
#max water level
maxWL.aov <-aov(maxWL~station); summary(medianWL.aov)
maxWL.HSD <-   TukeyHSD(maxWL.aov); maxWL.HSD
maxWL.HSD <- HSD.test(maxWL.aov,"station",group=T);maxWL.HSD
#inundation duration - need onway instead of ANOVA
durday.OW <- oneway.test(durday~station);durday.OW
durday.KW <- kruskal.test(durday~station);durday.KW 
#durday.aov <-  aov(durday~station); summary(durday.aov)
#durday.HSD <-   TukeyHSD(durday.aov); durday.HSD
#durday.HSD <- HSD.test(durday.aov,"station",group=T);durday.HSD
#n events - need onway test instead of ANOVA
nevent.OW <- oneway.test(nevents~station);nevent.OW
#nevent.KW <- kruskal.test(nevents~station);nevent.KW
#nevents.aov <- aov(nevents~station); summary(nevents.aov)
#nevents.HSD <-   TukeyHSD(nevents.aov); nevents.HSD
#nevents.HSD <- HSD.test(nevents.aov,"station",group=T);nevents.HSD



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Simple Linear Regression ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create inidcator variable for station and horizon
data <- data %>% 
  mutate(Horizon_Indicator = case_when(Generic_Horizon == "1O"~ 0,
                                       Generic_Horizon == "2A" ~ 1,
                                       Generic_Horizon == "3B" ~ 2),
         Station_Indicator = case_when(station == "KW-1W"~ 0,
                                       station == "KW-2E" ~ 1,
                                       station == "KW-3T" ~ 2,
                                       station == "KW-4U" ~ 3))

#Separate horizons
O <- data %>% filter(Generic_Horizon == "1O")
A <- data %>% filter(Generic_Horizon == "2A")
B <- data %>% filter(Generic_Horizon == "3B")

### 4.1 Mean WL ------------------------------

# 4.1.1 EOC ------------------------------
lmEOCO <- lm(O$EOC_mgC_L~O$mean_waterLevel)
summary(lmEOCO)
plot(residuals(lmEOCO))
lmEOCA <- lm(A$EOC_mgC_L~A$mean_waterLevel)
summary(lmEOCA)
plot(residuals(lmEOCA))
lmEOCB <- lm(B$EOC_mgC_L~B$mean_waterLevel)
summary(lmEOCB)
plot(residuals(lmEOCB))
lmoverall <- lm(data$EOC_mgC_L~data$mean_waterLevel)
summary(lmoverall)
plot(residuals(lmoverall))

compEOC <- compare_performance(lmEOCO,lmEOCA,lmEOCB)

# 4.1.2 SUVA ------------------------------
lmSUVAO <- lm(O$SUVA254_L_mgm~O$mean_waterLevel)
summary(lmSUVAO)
plot(residuals(lmSUVAO))
lmSUVAA <- lm(A$SUVA254_L_mgm~A$mean_waterLevel)
summary(lmSUVAA)
plot(residuals(lmSUVAA)) #bit of a parabolic trend - indicating something important is missing
lmSUVAB <- lm(B$SUVA254_L_mgm~B$mean_waterLevel)
summary(lmSUVAB)
plot(residuals(lmSUVAB))
lmoverall <- lm(data$SUVA254_L_mgm~data$mean_waterLevel)
summary(lmoverall)
plot(residuals(lmoverall))

# 4.1.3 HIX ------------------------------
lmHIXoverall <- lm(data$HIX~data$mean_waterLevel)
summary(lmoverall)
plot(residuals(lmHIXoverall))

### 4.2 N_events -----------------------------
lmNO <- lm(O$EOC_mgC_L~O$n_events)
summary(lmNO) 
lmNA <- lm(A$EOC_mgC_L~A$n_events)
summary(lmNA)
lmNB <- lm(B$EOC_mgC_L~B$n_events)
summary(lmNB) #non significant 

### 4.3 Duration -----------------------------

lmFIO <- lm(O$FI~O$dur_day)
summary(lmFIO)
plot(residuals(lmFIO))
lmFIA <- lm(A$FI~A$dur_day)
summary(lmFIA)
plot(residuals(lmFIA))
lmFIB <- lm(B$FI~B$dur_day)
summary(lmFIB)
plot(residuals(lmFIB))
lmFIoverall <-(data$FI~data$dur_day)
summary(lmFIoverall)
plot(residuals(lmFIoverall))

### 4.4 Station indicator------------------------
lmstatO <- lm(O$EOC_mgC_L~O$Station_Indicator)
summary(lmstatO) #station signf for O only
lmstatA <- lm(A$EOC_mgC_L~A$Station_Indicator)
summary(lmstatA)
lmstatB <- lm(B$EOC_mgC_L~B$Station_Indicator)
summary(lmstatB)

### 4.5 Keep horizons together----------------------
#4.5.1 EOC ------------------------------------
#EOC ~ Horizon
lmEOChoriz <- lm(data$EOC_mgC_L~data$Horizon_Indicator)
summary(lmEOChoriz)
plot(residuals(lmEOChoriz)) #horiz is significant

#EOC ~ station
lmEOCstat <- lm(data$EOC_mgC_L~data$Station_Indicator)
summary(lmEOCstat)
plot(residuals(lmEOCstat)) #station not significant by itself

#EOC ~ mean_waterlevel
lmmeanwl <- lm(data$EOC_mgC_L~data$mean_waterLevel)
summary(lmmeanwl) #mean not significant
plot(residuals(lmmeanwl))

# 4.5.2 FI --------------------------------------
#FI ~ Horizon
lmFIhoriz <- lm(data$FI~data$Horizon_Indicator)
summary(lmFIhoriz)
plot(residuals(lmFIhoriz)) #horiz is significant

#FI ~ station
lmFIstat <- lm(data$EOC_mgC_L~data$Station_Indicator)
summary(lmFIstat)
plot(residuals(lmFIstat)) #station not significant by itself

#FI ~ mean_waterlevel
lmmeanwl <- lm(data$FI~data$mean_waterLevel)
summary(lmmeanwl) #mean not significant
plot(residuals(lmmeanwl))

# 4.5.3 SUVA ----------------------------------
#SUVA ~ Horizon
lmSUVAhoriz <- lm(data$SUVA254_L_mgm~data$Horizon_Indicator)
summary(lmSUVAhoriz)
plot(residuals(lmSUVAhoriz)) #horiz is significant
qqnorm(resid(lmSUVAhoriz))
shapiro.test(resid(lmSUVAhoriz))

#SUVA ~ station
lmSUVAstat <- lm(data$SUVA254_L_mgm~data$Station_Indicator)
summary(lmSUVAstat)
plot(residuals(lmSUVAstat)) #station is significant 

#SUVA ~ mean_waterlevel
lmmeanwl <- lm(data$SUVA254_L_mgm~data$mean_waterLevel)
summary(lmmeanwl) #mean not significant (just barely)
plot(residuals(lmmeanwl))
qqnorm(resid(lmmeanwl))
shapiro.test(resid(lmmeanwl))

# 4.5.4 HIX -----------------------------
#HIX ~ Horizon
lmHIXhoriz <- lm(data$HIX~data$Horizon_Indicator)
summary(lmHIXhoriz)
plot(residuals(lmHIXhoriz)) #horiz is significant by itself
qqnorm(resid(lmHIXhoriz))
shapiro.test(resid(lmHIXhoriz))

#HIX ~ station
lmHIXstat <- lm(data$HIX~data$Station_Indicator)
summary(lmHIXstat)
plot(residuals(lmHIXstat)) #station is significant 

#HIX ~ mean_waterlevel
lmmeanwl <- lm(data$HIX~data$mean_waterLevel)
summary(lmmeanwl) #mean water level is significant by itself
plot(residuals(lmmeanwl))
qqnorm(resid(lmmeanwl))
shapiro.test(resid(lmmeanwl))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Multiple Linear Regression --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#keep horizons grouped together
#create indicator variables for horizon and transect station 
data <- data %>% 
  mutate(Horizon_Indicator = case_when(Generic_Horizon == "1O"~ 0,
                                       Generic_Horizon == "2A" ~ 1,
                                       Generic_Horizon == "3B" ~ 2),
         Station_Indicator = case_when(station == "KW-1W"~ 0,
                                       station == "KW-2E" ~ 1,
                                       station == "KW-3T" ~ 2,
                                       station == "KW-4U" ~ 3))

#5.1 EOC -----------------------------

#EOC ~ All variables
lmall <- lm(data$EOC_mgC_L~data$Horizon_Indicator+
              data$Station_Indicator+
              data$Percent_Clay+
              data$min_waterLevel+
              data$mean_waterLevel+
              data$max_waterLevel+
              data$dur_day)
summary(lmall) #none of the water level variables are significant
plot(residuals(lmall))
qqnorm(resid(lmall))
shapiro.test(resid(lmall))

#5.2 FI ------------------------------
#FI ~ All variables
lmall <- lm(data$FI~data$Horizon_Indicator+
              data$Station_Indicator+
              data$Percent_Clay+
              data$min_waterLevel+
              data$mean_waterLevel+
              data$max_waterLevel+
              data$dur_day)
summary(lmall) #horiz, clay significant
plot(residuals(lmall))
qqnorm(resid(lmall))
shapiro.test(resid(lmall))

#5.3 SUVA -----------------------------
#SUVA ~ All variables
lmall <- lm(data$SUVA254_L_mgm~data$Horizon_Indicator+
              data$Station_Indicator+
              data$Percent_Clay+
              data$min_waterLevel+
              data$mean_waterLevel+
              data$max_waterLevel+
              data$dur_day)
summary(lmall) #horizon, clay, min water level significant
plot(residuals(lmall))
qqnorm(resid(lmall))
shapiro.test(resid(lmall))


#5.4 HIX -----------------------------
#HIX ~ All variables
lmall <- lm(data$HIX~data$Horizon_Indicator+
              data$Station_Indicator+
              data$Percent_Clay+
              data$min_waterLevel+
              data$mean_waterLevel+
              data$max_waterLevel+
              data$dur_day)
summary(lmall) #station, clay, (horizon just barely not significant)
plot(residuals(lmall))
qqnorm(resid(lmall))
shapiro.test(resid(lmall))

#5.5 Now separate horizons and look at multiple variable influence--------------
#5.5.1 EOC ------------------------------
lmOall <- lm(O$EOC_mgC_L~
               O$Station_Indicator+
               O$Percent_Clay+
               O$min_waterLevel+
               O$mean_waterLevel+
               O$max_waterLevel+
               O$dur_day)
summary(lmOall) #nothing significant, overall model is
plot(residuals(lmOall))
qqnorm(resid(lmOall))
shapiro.test(resid(lmOall))

lmAall <- lm(A$EOC_mgC_L~
               A$Station_Indicator+
               A$Percent_Clay+
               A$min_waterLevel+
               A$mean_waterLevel+
               A$max_waterLevel+
               A$dur_day)
summary(lmAall) #station signficiant, nothing else, overall model is
plot(residuals(lmAall))
qqnorm(resid(lmAall))
shapiro.test(resid(lmAall))

lmBall <- lm(B$EOC_mgC_L~
               B$Station_Indicator+
               B$Percent_Clay+
               B$min_waterLevel+
               B$mean_waterLevel+
               B$max_waterLevel+
               B$dur_day)
summary(lmBall) #nothing significant, overall model isn't either
plot(residuals(lmBall))
qqnorm(resid(lmBall))
shapiro.test(resid(lmBall))

#5.5.2 FI ------------------------------

lmFIOall <- lm(O$FI~
               O$Station_Indicator+
               O$Percent_Clay+
               O$min_waterLevel+
               O$mean_waterLevel+
               O$max_waterLevel+
               O$dur_day)
summary(lmFIOall) #nothing significant, overall model isn't
plot(residuals(lmFIOall))
qqnorm(resid(lmFIOall))
shapiro.test(resid(lmFIOall))

lmFIAall <- lm(A$FI~
               A$Station_Indicator+
               A$Percent_Clay+
               A$min_waterLevel+
               A$mean_waterLevel+
               A$max_waterLevel+
               A$dur_day)
summary(lmFIAall) #nothing significant, overall model isn't
plot(residuals(lmFIAall))
qqnorm(resid(lmFIAall))
shapiro.test(resid(lmFIAall))

lmFIBall <- lm(B$FI~
               B$Station_Indicator+
               B$Percent_Clay+
               B$min_waterLevel+
               B$mean_waterLevel+
               B$max_waterLevel+
               B$dur_day)
summary(lmFIBall) #nothing significant, overall model isn't either
plot(residuals(lmFIBall))
qqnorm(resid(lmFIBall))
shapiro.test(resid(lmFIBall))

#5.5.3 SUVA ------------------------------
lmSUVAOall <- lm(O$SUVA254_L_mgm~
                 O$Station_Indicator+
                 O$Percent_Clay+
                 O$min_waterLevel+
                 O$mean_waterLevel+
                 O$max_waterLevel+
                 O$dur_day)
summary(lmSUVAOall) #nothing significant, overall model isn't
plot(residuals(lmSUVAOall))
qqnorm(resid(lmSUVAOall))
shapiro.test(resid(lmSUVAOall))

lmSUVAAall <- lm(A$SUVA254_L_mgm~
                 A$Station_Indicator+
                 A$Percent_Clay+
                 A$min_waterLevel+
                 A$mean_waterLevel+
                 A$max_waterLevel+
                 A$dur_day)
summary(lmSUVAAall) #min water level is significant, dur day barely not, overall model is
plot(residuals(lmSUVAAall))
qqnorm(resid(lmSUVAAall))
shapiro.test(resid(lmSUVAAall))

lmSUVABall <- lm(B$SUVA254_L_mgm~
                 B$Station_Indicator+
                 B$Percent_Clay+
                 B$min_waterLevel+
                 B$mean_waterLevel+
                 B$max_waterLevel+
                 B$dur_day)
summary(lmSUVABall) #percent clay significant
plot(residuals(lmSUVABall))
qqnorm(resid(lmSUVABall))
shapiro.test(resid(lmSUVABall))

#5.5.4 HIX ------------------------------
lmHIXOall <- lm(O$HIX~
                   O$Station_Indicator+
                   O$Percent_Clay+
                   O$min_waterLevel+
                   O$mean_waterLevel+
                   O$max_waterLevel+
                   O$dur_day)
summary(lmHIXOall) #nothing significant, overall model isn't
plot(residuals(lmHIXOall))
qqnorm(resid(lmHIXOall))
shapiro.test(resid(lmHIXOall))

lmHIXAall <- lm(A$HIX~
                   A$Station_Indicator+
                   A$Percent_Clay+
                   A$min_waterLevel+
                   A$mean_waterLevel+
                   A$max_waterLevel+
                   A$dur_day)
summary(lmHIXAall)  #nothing significant, overall model isn't
plot(residuals(lmHIXAall))
qqnorm(resid(lmHIXAall))
shapiro.test(resid(lmHIXAall))

lmHIXBall <- lm(B$HIX~
                   B$Station_Indicator+
                   B$Percent_Clay+
                   B$min_waterLevel+
                   B$mean_waterLevel+
                   B$max_waterLevel+
                   B$dur_day)
summary(lmHIXBall) #percent clay significant, model overal is
plot(residuals(lmHIXBall))
qqnorm(resid(lmHIXBall))
shapiro.test(resid(lmHIXBall))
