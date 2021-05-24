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
threshold_annual <- read_csv("data/annual_metrics_threshold_0.3.csv")
annual <- read_csv("data/annual_metrics_2020.csv")
waterlevel <- read_csv("data/waterLevel_at_sampling_location.csv")
elev <- read_csv("data/xs_survey.csv")

#Filter to just wetlands and no leaf litter
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")

#Join metrics and ESOM data
data <- inner_join(df, annual, by=c("wetland","station"))

#Filter water level data to just 2020 water year
waterlevel <- waterlevel %>% filter(Timestamp > "2019-10-01" & Timestamp < "2020-10-01")

#Join elevation data to threshold annual metrics
threshold_annual <- left_join(threshold_annual,elev,by=c("wetland","station"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Exploratory Data Analysis--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Look at distributions/normality/equal variance 

#Define station vs horizon
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon

###ESOM DATA###

#Summarize data
ESOM_Summary <- WetlandsNoLL %>% 
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
bartlett.test(logEOC~Station) #Barlett test doesn't meet equal variance assumptions
bartlett.test(logEOC~Horizon) 
leveneTest(logEOC,Station,center=median) #Levene test doesn't meet equal variance assumptions
leveneTest(logEOC,Horizon,center=median)


### 2.2 FI -----------------------------------
FI <- WetlandsNoLL$FI 
qqnorm(FI)
hist(FI)
shapiro.test(FI) #not normally distributed
bartlett.test(logFI~Horizon) #no equal variance

logFI <- log10(FI)
hist(logFI)
qqnorm(logFI)
shapiro.test(logFI) #logging FI data is not normally distributed
bartlett.test(logFI~Horizon)

### 2.3 SUVA -----------------------------------
SUVA <- WetlandsNoLL$SUVA254_L_mgm
hist(SUVA)
shapiro.test(SUVA) #not normally distributed
bartlett.test(SUVA~Horizon)
sqrtSUVA <- sqrt(SUVA) 
hist(sqrtSUVA)
shapiro.test(sqrtSUVA) #not normally distributed

### 2.4 HIX -----------------------------------
HIX <- WetlandsNoLL$HIX
hist(HIX)
shapiro.test(HIX) #not normally distributed
bartlett.test(HIX~Station)

sqrtHIX <- sqrt(HIX)
hist(sqrtHIX) 
shapiro.test(sqrtHIX) #not normally distributed

### 2.5 SSR -----------------------------------
SSR <- WetlandsNoLL$SSR
hist(SSR)
shapiro.test(SSR) #not normally distributed
bartlett.test(SSR~Station)#no equal variance

logSSR <- log10(SSR)
hist(logSSR)
shapiro.test(logSSR)

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

station <- threshold_annual$station
elev <-    threshold_annual$elevation
minWL <-   threshold_annual$min_waterLevel
meanWL <-  threshold_annual$mean_waterLevel
medianWL <-threshold_annual$median_waterLevel
maxWL <-   threshold_annual$max_waterLevel
durday <-  threshold_annual$dur_day
nevents <- threshold_annual$n_events

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
shapiro.test(durday) #not normally distributed
bartlett.test(durday~station) #not equal variance
#n events
qqnorm(nevents)
shapiro.test(nevents) #not normally distributed
bartlett.test(nevents~station) #not equal variance


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 ANOVA/TukeyHSD --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 3.1 EOC --------------------------------------------------
#3.1.1 EOC by horizon------------------------------------------
logEOC.horiz.aov <- aov(logEOC~Horizon)
summary(logEOC.horiz.aov)
logEOC.horiz.HSD <- TukeyHSD(logEOC.horiz.aov);logEOC.horiz.HSD 
#one-way anova test for unequal variance
oneway.test(logEOC~Horizon)

#3.1.2 EOC by station------------------------------------------
logEOC.sta.aov <- aov(logEOC~Station)
summary(logEOC.sta.aov)
logEOC.sta.HSD <- TukeyHSD(logEOC.sta.aov);logEOC.sta.HSD 
#one-way anova test for unequal variance
oneway.test(logEOC~Station)

#3.1.3 EOC by both horizon and transect location (ANOCVA) --------------------
logEOC.both.aov <- aov(logEOC~Horizon+Station)
summary(logEOC.both.aov)
logEOC.both.HSD <- TukeyHSD(logEOC.both.aov);logEOC.both.HSD  

### 3.2 FI -------------------------------------------
#3.2.1 FI by horizon------------------------------------------
#3.2.2 FI by station------------------------------------------
#3.2.3 FI by both horizon and transect location (ANOCVA) --------------------

### 3.3 SUVA ------------------------------------------
### 3.4 HIX ------------------------------------------
### 3.5 SSR ------------------------------------------

### 3.6 Water Level Data--------------------------------
waterlevel.aov <- aov(y_n~station)
summary(waterlevel.aov)
waterlevel.HSD <- TukeyHSD(waterlevel.aov);waterlevel.HSD 

### 3.7 2020 Water Level Metrics
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
#inundation duration
durday.aov <-  aov(durday~station); summary(durday.aov)
durday.HSD <-   TukeyHSD(durday.aov); durday.HSD
durday.HSD <- HSD.test(durday.aov,"station",group=T);durday.HSD
#n events
nevents.aov <- aov(nevents~station); summary(nevents.aov)
nevents.HSD <-   TukeyHSD(nevents.aov); nevents.HSD
nevents.HSD <- HSD.test(nevents.aov,"station",group=T);nevents.HSD



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Kruskal-Wallis --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###4.1 EOC --------------------------------------------------
#EOC by horizon
kruskal.test(logEOC~Horizon)
kruskalmc(logEOC,Horizon,probs=0.05)
#EOC by station
kruskal.test(logEOC~Station)
kruskalmc(logEOC,Station,probs=0.05)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Linear Regression --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Separate horizons
O <- data %>% filter(Generic_Horizon == "1O")
A <- data %>% filter(Generic_Horizon == "2A")
B <- data %>% filter(Generic_Horizon == "3B")

### 5.1 Mean WL ------------------------------

# 5.1.1 EOC ------------------------------
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

# 5.1.2 SUVA ------------------------------
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

# 5.1.3 HIX ------------------------------
lmHIXoverall <- lm(data$HIX~data$mean_waterLevel)
summary(lmoverall)
plot(residuals(lmHIXoverall))

### 5.2 N_events -----------------------------


### 5.3 Duration -----------------------------

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
