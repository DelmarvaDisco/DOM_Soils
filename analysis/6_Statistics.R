#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Statistical Anlyses
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

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv")
annual <- read_csv("data/annual_metrics_2020.csv")
waterlevel <- read_csv("data/waterLevel_at_sampling_location.csv")

#Filter to just wetlands and no leaf litter
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")

#Join metrics and ESOM data
data <- inner_join(df, annual, by=c("wetland","station"))

#Filter water level data to just 2020 water year
waterlevel <- waterlevel %>% filter(Timestamp > "2019-10-01" & Timestamp < "2020-10-01")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Exploratory Data Analysis--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Look at distributions/normality/equal variance 

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
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon

#test for equal variance
bartlett.test(logEOC~Station) #Barlett test doesn't meet equal variance assumptions
bartlett.test(logEOC~Horizon) 
leveneTest(logEOC,Station,center=median) #Levene test doesn't meet equal variance assumptions
leveneTest(logEOC,Horizon,center=median)


### 2.2 FI -----------------------------------
qqnorm(df$FI)
shapiro.test(df$FI)
logFI <- log(df$FI)
qqnorm(logFI)
shapiro.test(logFI) #logging FI data is not normally distributed

### 2.3 Water Level Data -----------------------------------
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 ANOVA/TukeyHSD --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 3.1 EOC --------------------------------------------------
#4.1.1 EOC by horizon------------------------------------------
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

### 3.2 Water Level Data--------------------------------
waterlevel.aov <- aov(y_n~station)
summary(waterlevel.aov)
waterlevel.HSD <- TukeyHSD(waterlevel.aov);waterlevel.HSD 


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
