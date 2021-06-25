#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Statistical Analyses
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 4/9/2021
#Updated: 6/20/2021
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

#Filter to specific months
JanMar <- WetlandsNoLL %>% filter(Month %in% c('2020-01','2020-03'))
Sept <- WetlandsNoLL %>% filter(Month == '2020-09')
Nov <- WetlandsNoLL %>% filter(Month == "2020-11")

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

###ESOM DATA###
#Summarize data
#By horizon all data
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

Spring_ESOM_Horizon_Summary <- JanMar %>% 
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
             sdSSR = sd(SSR,na.rm = T),
             nobserv = length(EOC_mgC_L))

Autumn_ESOM_Horizon_Summary <- Sept %>% 
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
             sdSSR = sd(SSR,na.rm = T),
             nobserv = length(EOC_mgC_L))

#By station - all data
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

Spring_ESOM_Station_Summary <- JanMar %>% 
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
             sdSSR = sd(SSR,na.rm = T),
             nobserv = length(EOC_mgC_L))

Autumn_ESOM_Station_Summary <- Sept %>% 
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
             sdSSR = sd(SSR,na.rm = T),
             nobserv = length(EOC_mgC_L))

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
            O_dur_day_se = O_dur_day_sd/sqrt(4),
            O_nevents_mean = mean(O_n_events,na.rm=T),
            O_nevents_sd = sd(O_n_events,na.rm=T),
            O_nevents_se = O_nevents_sd/sqrt(4),
            #A horizon
            A_dur_day_mean = mean(A_dur_day,na.rm=T),
            A_dur_day_sd = sd(A_dur_day,na.rm=T),
            A_dur_day_se = A_dur_day_sd/sqrt(4),
            A_nevents_mean = mean(A_n_events,na.rm=T),
            A_nevents_sd = sd(A_n_events,na.rm=T),
            A_nevents_se = A_nevents_sd/sqrt(4),
            #B horizon
            B_dur_day_mean = mean(B_dur_day,na.rm=T),
            B_dur_day_sd = sd(B_dur_day,na.rm=T),
            B_dur_day_se = B_dur_day_sd/sqrt(4),
            B_nevents_mean = mean(B_n_events,na.rm=T),
            B_nevents_sd = sd(B_n_events,na.rm=T),
            B_nevents_se = B_nevents_sd/sqrt(4))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 ANOVA/TukeyHSD/Kruskal-Wallis/Oneway Test --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1 Check equal variance
#Step 2 Run ANOVA/TukeyHSD or other tests
#Step 3 Check normality of residuals 

### 3.1.1 ALL EOC --------------------------------------------------
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon
logEOC <- log10(WetlandsNoLL$EOC_mgC_L)

#Step 1 Check equal variance
bartlett.test(logEOC~Station) #Barlett test doesn't meet equal variance assumptions but barely, Dr G says ok to proceed 
bartlett.test(logEOC~Horizon) 
leveneTest(logEOC,Station,center=median) #Levene test doesn't meet equal variance assumptions
leveneTest(logEOC,Horizon,center=median)

#Step 2 ANOVA
#EOC by horizon 
logEOC.horiz.aov <- aov(logEOC~Horizon);summary(logEOC.horiz.aov)
logEOC.horiz.HSD <- TukeyHSD(logEOC.horiz.aov);logEOC.horiz.HSD
logEOCL.HSD <- HSD.test(logEOC.horiz.aov,"Horizon",group=T);logEOCL.HSD 

#EOC by station
logEOC.sta.aov <- aov(logEOC~Station);summary(logEOC.sta.aov)
logEOC.sta.HSD <- TukeyHSD(logEOC.sta.aov);logEOC.sta.HSD 
logEOC.sta.HSD <- HSD.test(logEOC.sta.aov,"Station",group=T);logEOC.sta.HSD 

#ANCOVA - interaction of horizon and station
logEOC.both.aov <- aov(logEOC~Horizon*Station);summary(logEOC.both.aov)
logEOC.both.HSD <- TukeyHSD(logEOC.both.aov);logEOC.both.HSD  
logEOC.both.HSD <- HSD.test(logEOC.both.aov,trt = c("Horizon", "Station"),group=T);logEOC.both.HSD

#Step 3 Check normality of residuals 
qqnorm(logEOC)
shapiro.test(logEOC) #Log of values is normally distributed
shapiro.test(resid(logEOC.horiz.aov)) #pass
shapiro.test(resid(logEOC.sta.aov)) #pass


#3.1.2 Spring EOC ------------------------------------------
Station <- JanMar$station
Horizon <- JanMar$Generic_Horizon
SpEOC <- JanMar$EOC_mgC_L
logSpEOC <- log10(SpEOC)

#Step 1 Check equal variance
bartlett.test(logSpEOC~Station) #Barlett works for both horz and stat on log transformed EOC
bartlett.test(logSpEOC~Horizon) 
leveneTest(logSpEOC,Station,center=median) #Levene test works 
leveneTest(logSpEOC,Horizon,center=median)

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
logSpEOC.horiz.aov <- aov(logSpEOC~Horizon);summary(logSpEOC.horiz.aov)
logSpEOC.horiz.HSD <- TukeyHSD(logSpEOC.horiz.aov);logSpEOC.horiz.HSD
logSpEOC.HSD <- HSD.test(logSpEOC.horiz.aov,"Horizon",group=T);logSpEOC.HSD 

#EOC by station
logSpEOC.sta.aov <- aov(logSpEOC~Station);summary(logSpEOC.sta.aov)
logSpEOC.sta.HSD <- TukeyHSD(logSpEOC.sta.aov);logSpEOC.sta.HSD 
logSpEOC.sta.HSD <- HSD.test(logSpEOC.sta.aov,"Station",group=T);logSpEOC.sta.HSD 

#ANCOVA
logSpEOC.both.aov <- aov(logSpEOC~Horizon*Station);summary(logSpEOC.both.aov)
logSpEOC.both.HSD <- TukeyHSD(logSpEOC.both.aov);logSpEOC.both.HSD  
logSpEOC.both.HSD <- HSD.test(logSpEOC.both.aov,trt = c("Horizon", "Station"),group=T);logSpEOC.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(logSpEOC)
shapiro.test(logSpEOC) #Log of values is normally distributed
shapiro.test(resid(logSpEOC.horiz.aov))
shapiro.test(resid(logSpEOC.sta.aov))

#3.1.3 Autumn EOC------------------------------------------
Station <- Sept$station
Horizon <- Sept$Generic_Horizon
AuEOC <- Sept$EOC_mgC_L
logAuEOC <- log10(AuEOC)

#Step 1 Check equal variance
bartlett.test(logAuEOC~Station) #Barlett works for both horz and stat on log transformed EOC
bartlett.test(logAuEOC~Horizon) 
leveneTest(logAuEOC,Station,center=median) #Levene test works 
leveneTest(logAuEOC,Horizon,center=median)

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
logAuEOC.horiz.aov <- aov(logAuEOC~Horizon);summary(logAuEOC.horiz.aov)
logAuEOC.horiz.HSD <- TukeyHSD(logAuEOC.horiz.aov);logAuEOC.horiz.HSD
logAuEOC.HSD <- HSD.test(logAuEOC.horiz.aov,"Horizon",group=T);logAuEOC.HSD 

#EOC by station
logAuEOC.sta.aov <- aov(logAuEOC~Station);summary(logAuEOC.sta.aov)
logAuEOC.sta.HSD <- TukeyHSD(logAuEOC.sta.aov);logAuEOC.sta.HSD 
logAuEOC.sta.HSD <- HSD.test(logAuEOC.sta.aov,"Station",group=T);logAuEOC.sta.HSD 

#ANCOVA
logAuEOC.both.aov <- aov(logAuEOC~Horizon*Station);summary(logAuEOC.both.aov)
logAuEOC.both.HSD <- TukeyHSD(logAuEOC.both.aov);logAuEOC.both.HSD  
logAuEOC.both.HSD <- HSD.test(logAuEOC.both.aov,trt = c("Horizon", "Station"),group=T);logAuEOC.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(logAuEOC)
shapiro.test(logAuEOC) #Log of values is normally distributed
shapiro.test(resid(logAuEOC.horiz.aov))
shapiro.test(resid(logAuEOC.sta.aov))

### 3.2 FI -----------------------------------
#3.2.1 All FI data ------------------------------
Horizon <- WetlandsNoLL$Generic_Horizon
Station <- WetlandsNoLL$station
FI <- WetlandsNoLL$FI
logFI <- log10(FI)

#Step 1 Check equal variance
#horizon
bartlett.test(FI~Horizon) #no equal variance
leveneTest(FI,Horizon,center=median) #no equal variance - fail barlett, levene, shapiro - use oneway.test? 
#station
bartlett.test(FI~Station) #equal variance
leveneTest(FI,Station,center=median)

#Step 2 ANOVA
#FI by horizon
oneway.test(FI~Horizon)
pairwise.t.test(FI,Horizon,p.adj="bonferroni") #O not diff from A but all other comparisons significant
 
FI.horiz.aov <- aov(FI~Horizon);summary(FI.horiz.aov)
FI.horiz.HSD <- TukeyHSD(FI.horiz.aov);FI.horiz.HSD
FI.HSD <- HSD.test(FI.horiz.aov,"Horizon",group=T);FI.HSD 

#FI by station
oneway.test(FI~Station)
pairwise.t.test(FI,Station,p.adj="bonferroni") 
#I think I can get away with Kruskal-Wallis here
kruskal.test(FI~Station)
kruskalmc(FI,Station, probs=0.05) 

FI.sta.aov <- aov(FI~Station);summary(FI.sta.aov)
FI.sta.HSD <- TukeyHSD(FI.sta.aov);FI.sta.HSD 
FI.sta.HSD <- HSD.test(FI.sta.aov,"Station",group=T);FI.sta.HSD 

#ANCOVA - interaction of horizon and station
FI.both.aov <- aov(FI~Horizon*Station);summary(FI.both.aov)
FI.both.HSD <- TukeyHSD(FI.both.aov);FI.both.HSD  
FI.both.HSD <- HSD.test(FI.both.aov,trt = c("Horizon", "Station"),group=T);FI.both.HSD

#Step 3 Normality of residuals
qqnorm(FI)
hist(FI)
shapiro.test(FI) #not normally distributed
shapiro.test(resid(FI.horiz.aov))
shapiro.test(resid(FI.sta.aov))

#3.2.2 Spring FI ------------------------------------------
Station <- JanMar$station
Horizon <- JanMar$Generic_Horizon
SpFI <- JanMar$FI
logSpFI <- log10(SpFI)

#Step 1 Check equal variance
bartlett.test(logSpFI~Station) #Works for stat
bartlett.test(logSpFI~Horizon) #misses but sd within 2x of eachother - allow it
leveneTest(logSpFI,Station,center=median) #Levene test works 
leveneTest(logSpFI,Horizon,center=median)

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
logSpFI.horiz.aov <- aov(logSpFI~Horizon);summary(logSpFI.horiz.aov)
logSpFI.horiz.HSD <- TukeyHSD(logSpFI.horiz.aov);logSpFI.horiz.HSD
logSpFI.HSD <- HSD.test(logSpFI.horiz.aov,"Horizon",group=T);logSpFI.HSD 

#EOC by station
logSpFI.sta.aov <- aov(logSpFI~Station);summary(logSpFI.sta.aov)
logSpFI.sta.HSD <- TukeyHSD(logSpFI.sta.aov);logSpFI.sta.HSD 
logSpFI.sta.HSD <- HSD.test(logSpFI.sta.aov,"Station",group=T);logSpFI.sta.HSD 

#ANCOVA
logSpFI.both.aov <- aov(logSpFI~Horizon*Station);summary(logSpFI.both.aov)
logSpFI.both.HSD <- TukeyHSD(logSpFI.both.aov);logSpFI.both.HSD  
logSpFI.both.HSD <- HSD.test(logSpFI.both.aov,trt = c("Horizon", "Station"),group=T);logSpFI.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(logSpFI)
shapiro.test(logSpFI) #fail
shapiro.test(resid(logSpFI.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(logSpFI.sta.aov)) #only slightly under - let it slide?

#3.2.2 Autumn FI ------------------------------------------
Station <- Sept$station
Horizon <- Sept$Generic_Horizon
AuFI <- Sept$FI
logAuFI <- log10(AuFI)

#Step 1 Check equal variance
bartlett.test(logAuFI~Station) #Works for stat
bartlett.test(logAuFI~Horizon) #misses but sd within 2x of eachother - allow it
leveneTest(logAuFI,Station,center=median) #Levene test works 
leveneTest(logAuFI,Horizon,center=median)

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
logAuFI.horiz.aov <- aov(logAuFI~Horizon);summary(logAuFI.horiz.aov)
logAuFI.horiz.HSD <- TukeyHSD(logAuFI.horiz.aov);logAuFI.horiz.HSD
logAuFI.HSD <- HSD.test(logAuFI.horiz.aov,"Horizon",group=T);logAuFI.HSD 

#EOC by station
logAuFI.sta.aov <- aov(logAuFI~Station);summary(logAuFI.sta.aov)
logAuFI.sta.HSD <- TukeyHSD(logAuFI.sta.aov);logAuFI.sta.HSD 
logAuFI.sta.HSD <- HSD.test(logAuFI.sta.aov,"Station",group=T);logAuFI.sta.HSD 

#ANCOVA
logAuFI.both.aov <- aov(logAuFI~Horizon*Station);summary(logAuFI.both.aov)
logAuFI.both.HSD <- TukeyHSD(logAuFI.both.aov);logAuFI.both.HSD  
logAuFI.both.HSD <- HSD.test(logAuFI.both.aov,trt = c("Horizon", "Station"),group=T);logAuFI.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(logAuFI)
shapiro.test(logAuFI) #fail
shapiro.test(resid(logAuFI.horiz.aov)) #passes
shapiro.test(resid(logAuFI.sta.aov)) #only slightly under - let it slide?

### 3.3 SUVA -----------------------------------
#3.3.1 All SUVA Data------------------------------------------
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon
SUVA <- WetlandsNoLL$SUVA254_L_mgm

#Step 1 Check equal variance
#horizon
bartlett.test(SUVA~Horizon)
leveneTest(SUVA,Horizon,center=median) #passes levene but fails bartlett and normality - use kruskal wallis
#station
bartlett.test(SUVA~Station)
leveneTest(SUVA,Station,center=median) #passes levene/bartlett but not normality - use kruskal wallis

#Step 2 ANOVA 
#By horizon
kruskal.test(SUVA~Horizon)
kruskalmc(SUVA, Horizon, probs=0.05) #O and A not diff, but all others yes
SUVA.horiz.aov <- aov(SUVA~Horizon);summary(SUVA.horiz.aov)
SUVA.horiz.HSD <- TukeyHSD(SUVA.horiz.aov);SUVA.horiz.HSD
SUVA.HSD <- HSD.test(SUVA.horiz.aov,"Horizon",group=T);SUVA.HSD 

#By station
kruskal.test(SUVA~Station)
kruskalmc(SUVA,Station, probs=0.05)
SUVA.sta.aov <- aov(SUVA~Station);summary(SUVA.sta.aov)
SUVA.sta.HSD <- TukeyHSD(SUVA.sta.aov);SUVA.sta.HSD 
SUVA.sta.HSD <- HSD.test(SUVA.sta.aov,"Station",group=T);SUVA.sta.HSD 

#ANCOVA - interaction of horizon and station
SUVA.both.aov <- aov(SUVA~Horizon*Station);summary(SUVA.both.aov)
SUVA.both.HSD <- TukeyHSD(SUVA.both.aov);SUVA.both.HSD  
SUVA.both.HSD <- HSD.test(SUVA.both.aov,trt = c("Horizon", "Station"),group=T);SUVA.both.HSD


#Step 3 Check normality of residuals
hist(SUVA)
shapiro.test(SUVA) #not normally distributed
shapiro.test(resid(SUVA.horiz.aov))
shapiro.test(resid(SUVA.sta.aov))

#3.3.2 Spring SUVA ------------------------------------------
Station <- JanMar$station
Horizon <- JanMar$Generic_Horizon
SpSUVA <- JanMar$SUVA254_L_mgm
#logSpSUVA <- log10(SpSUVA)

#Step 1 Check equal variance
bartlett.test(SpSUVA~Station) #pass
bartlett.test(SpSUVA~Horizon) #pass
leveneTest(SpSUVA,Station,center=median) #pass 
leveneTest(SpSUVA,Horizon,center=median) #pass

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
SpSUVA.horiz.aov <- aov(SpSUVA~Horizon);summary(SpSUVA.horiz.aov)
SpSUVA.horiz.HSD <- TukeyHSD(SpSUVA.horiz.aov);SpSUVA.horiz.HSD
SpSUVA.HSD <- HSD.test(SpSUVA.horiz.aov,"Horizon",group=T);SpSUVA.HSD 

#EOC by station
SpSUVA.sta.aov <- aov(SpSUVA~Station);summary(SpSUVA.sta.aov)
SpSUVA.sta.HSD <- TukeyHSD(SpSUVA.sta.aov);SpSUVA.sta.HSD 
SpSUVA.sta.HSD <- HSD.test(SpSUVA.sta.aov,"Station",group=T);SpSUVA.sta.HSD 

#ANCOVA
SpSUVA.both.aov <- aov(SpSUVA~Horizon*Station);summary(SpSUVA.both.aov)
SpSUVA.both.HSD <- TukeyHSD(SpSUVA.both.aov);SpSUVA.both.HSD  
SpSUVA.both.HSD <- HSD.test(SpSUVA.both.aov,trt = c("Horizon", "Station"),group=T);SpSUVA.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(SpSUVA)
shapiro.test(SpSUVA) #fail
shapiro.test(resid(SpSUVA.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(SpSUVA.sta.aov)) #only slightly under - let it slide?

#3.3.3 Autumn SUVA ------------------------------------------
Station <- Sept$station
Horizon <- Sept$Generic_Horizon
AuSUVA <- Sept$SUVA254_L_mgm
#logAuSUVA <- log10(AuSUVA)

#Step 1 Check equal variance
bartlett.test(AuSUVA~Station) #pass
bartlett.test(AuSUVA~Horizon) #pass
leveneTest(AuSUVA,Station,center=median) #pass 
leveneTest(AuSUVA,Horizon,center=median) #pass

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
AuSUVA.horiz.aov <- aov(AuSUVA~Horizon);summary(AuSUVA.horiz.aov)
AuSUVA.horiz.HSD <- TukeyHSD(AuSUVA.horiz.aov);AuSUVA.horiz.HSD
AuSUVA.HSD <- HSD.test(AuSUVA.horiz.aov,"Horizon",group=T);AuSUVA.HSD 

#EOC by station
AuSUVA.sta.aov <- aov(AuSUVA~Station);summary(AuSUVA.sta.aov)
AuSUVA.sta.HSD <- TukeyHSD(AuSUVA.sta.aov);AuSUVA.sta.HSD 
AuSUVA.sta.HSD <- HSD.test(AuSUVA.sta.aov,"Station",group=T);AuSUVA.sta.HSD 

#ANCOVA
AuSUVA.both.aov <- aov(AuSUVA~Horizon*Station);summary(AuSUVA.both.aov)
AuSUVA.both.HSD <- TukeyHSD(AuSUVA.both.aov);AuSUVA.both.HSD  
AuSUVA.both.HSD <- HSD.test(AuSUVA.both.aov,trt = c("Horizon", "Station"),group=T);AuSUVA.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(AuSUVA)
shapiro.test(AuSUVA) #fail
shapiro.test(resid(AuSUVA.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(AuSUVA.sta.aov)) #only slightly under - let it slide?

### 3.4 HIX -----------------------------------
#3.4.1 All HIX ------------------------------------------
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon
HIX <- WetlandsNoLL$HIX

#Step 1 check equal variance 
#horizon
bartlett.test(HIX~Horizon) #no equal variance 
leveneTest(HIX,Horizon,center=median) #no equal variance - fails bartlett/levene and normality - use oneway.test?
#station
bartlett.test(HIX~Station) #just barely fails 
leveneTest(HIX,Station,center=median) # equal variance - use kursal wallis

#Step 2 ANOVA
#By horizon
oneway.test(HIX~Horizon)
pairwise.t.test(HIX,Horizon,p.adj="bonferroni") #O not diff from A but all other comparisons significant
HIX.horiz.aov <- aov(HIX~Horizon);summary(HIX.horiz.aov)
HIX.horiz.HSD <- TukeyHSD(HIX.horiz.aov);HIX.horiz.HSD
HIX.HSD <- HSD.test(HIX.horiz.aov,"Horizon",group=T);HIX.HSD 

#By station
kruskal.test(HIX~Station)
kruskalmc(HIX,Station, probs=0.05) #W and E / T and U not diff, but all others yes
HIX.sta.aov <- aov(HIX~Station);summary(HIX.sta.aov)
HIX.sta.HSD <- TukeyHSD(HIX.sta.aov);HIX.sta.HSD 
HIX.sta.HSD <- HSD.test(HIX.sta.aov,"Station",group=T);HIX.sta.HSD 

#ANCOVA - interaction of horizon and station
HIX.both.aov <- aov(HIX~Horizon*Station);summary(HIX.both.aov)
HIX.both.HSD <- TukeyHSD(HIX.both.aov);HIX.both.HSD  
HIX.both.HSD <- HSD.test(HIX.both.aov,trt = c("Horizon", "Station"),group=T);HIX.both.HSD

#Step 3 Normality of residuals
hist(HIX)
shapiro.test(HIX) #not normally distributed
#transform data
sqrtHIX <- sqrt(HIX)
hist(sqrtHIX) 
shapiro.test(sqrtHIX) #not normally distributed

shapiro.test(resid(HIX.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(HIX.sta.aov)) #only slightly under - let it slide?

#3.4.2 Spring HIX ------------------------------------------
Station <- JanMar$station
Horizon <- JanMar$Generic_Horizon
SpHIX <- JanMar$HIX
#logSpSUVA <- log10(SpSUVA)

#Step 1 Check equal variance
bartlett.test(SpHIX~Station) #pass
bartlett.test(SpHIX~Horizon) #pass
leveneTest(SpHIX,Station,center=median) #pass 
leveneTest(SpHIX,Horizon,center=median) #pass

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
SpHIX.horiz.aov <- aov(SpHIX~Horizon);summary(SpHIX.horiz.aov)
SpHIX.horiz.HSD <- TukeyHSD(SpHIX.horiz.aov);SpHIX.horiz.HSD
SpHIX.HSD <- HSD.test(SpHIX.horiz.aov,"Horizon",group=T);SpHIX.HSD 

#EOC by station
SpHIX.sta.aov <- aov(SpHIX~Station);summary(SpHIX.sta.aov)
SpHIX.sta.HSD <- TukeyHSD(SpHIX.sta.aov);SpHIX.sta.HSD 
SpHIX.sta.HSD <- HSD.test(SpHIX.sta.aov,"Station",group=T);SpHIX.sta.HSD 

#ANCOVA
SpHIX.both.aov <- aov(SpHIX~Horizon*Station);summary(SpHIX.both.aov)
SpHIX.both.HSD <- TukeyHSD(SpHIX.both.aov);SpHIX.both.HSD  
SpHIX.both.HSD <- HSD.test(SpHIX.both.aov,trt = c("Horizon", "Station"),group=T);SpHIX.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(SpHIX)
shapiro.test(SpHIX) #fail
shapiro.test(resid(SpHIX.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(SpHIX.sta.aov)) #only slightly under - let it slide?

#3.4.3 Autumn HIX ------------------------------------------
Station <- Sept$station
Horizon <- Sept$Generic_Horizon
AuHIX <- Sept$HIX
#logAuSUVA <- log10(AuSUVA)

#Step 1 Check equal variance
bartlett.test(AuHIX~Station) #pass
bartlett.test(AuHIX~Horizon) #pass
leveneTest(AuHIX,Station,center=median) #pass 
leveneTest(AuHIX,Horizon,center=median) #pass

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
AuHIX.horiz.aov <- aov(AuHIX~Horizon);summary(AuHIX.horiz.aov)
AuHIX.horiz.HSD <- TukeyHSD(AuHIX.horiz.aov);AuHIX.horiz.HSD
AuHIX.HSD <- HSD.test(AuHIX.horiz.aov,"Horizon",group=T);AuHIX.HSD 

#EOC by station
AuHIX.sta.aov <- aov(AuHIX~Station);summary(AuHIX.sta.aov)
AuHIX.sta.HSD <- TukeyHSD(AuHIX.sta.aov);AuHIX.sta.HSD 
AuHIX.sta.HSD <- HSD.test(AuHIX.sta.aov,"Station",group=T);AuHIX.sta.HSD 

#ANCOVA
AuHIX.both.aov <- aov(AuHIX~Horizon*Station);summary(AuHIX.both.aov)
AuHIX.both.HSD <- TukeyHSD(AuHIX.both.aov);AuHIX.both.HSD  
AuHIX.both.HSD <- HSD.test(AuHIX.both.aov,trt = c("Horizon", "Station"),group=T);AuHIX.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(AuHIX)
shapiro.test(AuHIX) #fail
shapiro.test(resid(AuHIX.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(AuHIX.sta.aov)) #only slightly under - let it slide?

### 3.5 SSR -----------------------------------
#3.5.1 All SSR data------------------------------------------
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon
SSR <- WetlandsNoLL$SSR
logSSR <- log10(SSR)

#Step 1 Equal variance
#horizon
bartlett.test(SSR~Horizon)#no equal variance
leveneTest(SSR,Horizon,center=median) #no equal variance - fails bartlett/levene and normality - use oneway.test?
#station
bartlett.test(SSR~Station)#no equal variance
leveneTest(SSR,Station,center=median) #no equal variance - fails bartlett/levene and normality - use oneway.test?

#Step 2 ANOVA
#by horizon
oneway.test(SSR~Horizon)
pairwise.t.test(SSR,Horizon,p.adj="bonferroni") #O not diff from A but all other comparisons significant
SSR.horiz.aov <- aov(SSR~Horizon);summary(SSR.horiz.aov)
SSR.horiz.HSD <- TukeyHSD(SSR.horiz.aov);SSR.horiz.HSD
SSR.HSD <- HSD.test(SSR.horiz.aov,"Horizon",group=T);SSR.HSD 

#by station
kruskal.test(SSR~Station)
kruskalmc(SSR,Station, probs=0.05) #edge and upland only different
SSR.sta.aov <- aov(SSR~Station);summary(SSR.sta.aov)
SSR.sta.HSD <- TukeyHSD(SSR.sta.aov);SSR.sta.HSD 
SSR.sta.HSD <- HSD.test(SSR.sta.aov,"Station",group=T);SSR.sta.HSD

#ANCOVA - interaction of horizon and station
SSR.both.aov <- aov(SSR~Horizon*Station);summary(SSR.both.aov)
SSR.both.HSD <- TukeyHSD(SSR.both.aov);SSR.both.HSD  
SSR.both.HSD <- HSD.test(SSR.both.aov,trt = c("Horizon", "Station"),group=T);SSR.both.HSD

#Step 3 Normality of residuals
hist(SSR)
shapiro.test(SSR) #not normally distributed
hist(logSSR)
shapiro.test(logSSR)

shapiro.test(resid(SSR.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(SSR.sta.aov)) #only slightly under - let it slide?

#3.5.2 Spring SSR ------------------------------------------
Station <- JanMar$station
Horizon <- JanMar$Generic_Horizon
SpSSR <- JanMar$SSR

#Step 1 Check equal variance
bartlett.test(SpSSR~Station) #pass
bartlett.test(SpSSR~Horizon) #pass
leveneTest(SpSSR,Station,center=median) #pass 
leveneTest(SpSSR,Horizon,center=median) #pass

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
SpSSR.horiz.aov <- aov(SpSSR~Horizon);summary(SpSSR.horiz.aov)
SpSSR.horiz.HSD <- TukeyHSD(SpSSR.horiz.aov);SpSSR.horiz.HSD
SpSSR.HSD <- HSD.test(SpSSR.horiz.aov,"Horizon",group=T);SpSSR.HSD 

#EOC by station
SpSSR.sta.aov <- aov(SpSSR~Station);summary(SpSSR.sta.aov)
SpSSR.sta.HSD <- TukeyHSD(SpSSR.sta.aov);SpSSR.sta.HSD 
SpSSR.sta.HSD <- HSD.test(SpSSR.sta.aov,"Station",group=T);SpSSR.sta.HSD 

#ANCOVA
SpSSR.both.aov <- aov(SpSSR~Horizon*Station);summary(SpSSR.both.aov)
SpSSR.both.HSD <- TukeyHSD(SpSSR.both.aov);SpSSR.both.HSD  
SpSSR.both.HSD <- HSD.test(SpSSR.both.aov,trt = c("Horizon", "Station"),group=T);SpSSR.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(SpSSR)
shapiro.test(SpSSR) #fail
shapiro.test(resid(SpSSR.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(SpSSR.sta.aov)) #only slightly under - let it slide?


#3.5.3 Autumn SSR ------------------------------------------ 
Station <- Sept$station
Horizon <- Sept$Generic_Horizon
AuSSR <- Sept$SSR

#Step 1 Check equal variance
bartlett.test(AuSSR~Station) #pass
bartlett.test(AuSSR~Horizon) #pass
leveneTest(AuSSR,Station,center=median) #pass 
leveneTest(AuSSR,Horizon,center=median) #pass

#Step 2 Run ANOVA/TukeyHSD or other tests
#EOC by horizon 
AuSSR.horiz.aov <- aov(AuSSR~Horizon);summary(AuSSR.horiz.aov)
AuSSR.horiz.HSD <- TukeyHSD(AuSSR.horiz.aov);AuSSR.horiz.HSD
AuSSR.HSD <- HSD.test(AuSSR.horiz.aov,"Horizon",group=T);AuSSR.HSD 

#EOC by station
AuSSR.sta.aov <- aov(AuSSR~Station);summary(AuSSR.sta.aov)
AuSSR.sta.HSD <- TukeyHSD(AuSSR.sta.aov);AuSSR.sta.HSD 
AuSSR.sta.HSD <- HSD.test(AuSSR.sta.aov,"Station",group=T);AuSSR.sta.HSD 

#ANCOVA
AuSSR.both.aov <- aov(AuSSR~Horizon*Station);summary(AuSSR.both.aov)
AuSSR.both.HSD <- TukeyHSD(AuSSR.both.aov);AuSSR.both.HSD  
AuSSR.both.HSD <- HSD.test(AuSSR.both.aov,trt = c("Horizon", "Station"),group=T);AuSSR.both.HSD 

#Step 3 Check normality of data and residuals 
qqnorm(AuSSR)
shapiro.test(AuSSR) #fail
shapiro.test(resid(AuSSR.horiz.aov)) #only slightly under - let it slide?
shapiro.test(resid(AuSSR.sta.aov)) #only slightly under - let it slide?

### 3.6 Water Level Data -----------------------------------
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

waterlevel.aov <- aov(y_n~station)
summary(waterlevel.aov)
waterlevel.HSD <- TukeyHSD(waterlevel.aov);waterlevel.HSD 

### 3.7 2020 Annual Metrics ----------------------------------
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

#3.7.1 Theshold metrics -----------------------------------
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
#inundation duration - need onway instead of ANOVA?
durday.OW <- oneway.test(durday~station);durday.OW
durday.KW <- kruskal.test(durday~station);durday.KW 
durday.aov <-  aov(durday~station); summary(durday.aov)
durday.HSD <-   TukeyHSD(durday.aov); durday.HSD
durday.HSD <- HSD.test(durday.aov,"station",group=T);durday.HSD
#n events - need onway test instead of ANOVA?
nevent.OW <- oneway.test(nevents~station);nevent.OW
nevent.KW <- kruskal.test(nevents~station);nevent.KW
nevents.aov <- aov(nevents~station); summary(nevents.aov)
nevents.HSD <-   TukeyHSD(nevents.aov); nevents.HSD
nevents.HSD <- HSD.test(nevents.aov,"station",group=T);nevents.HSD

#3.7.2 Soil horizon events and duration of saturation -------------------------
Odur <- soil_annual$O_dur_day
Oev <- soil_annual$O_n_events
Adur <- soil_annual$A_dur_day
Aev <- soil_annual$A_n_events
Bdur <- soil_annual$B_dur_day
Bev <- soil_annual$B_n_events
station <- soil_annual$station

#O
#N events
qqnorm(Oev)
shapiro.test(Oev) #fail
bartlett.test(Oev~station) #fail
Oev.aov <-    aov(Oev~station);summary(Oev.aov)
Oev.HSD <- TukeyHSD(Oev.aov);Oev.HSD
Oev.HSD <- HSD.test(Oev.aov,"station",group=T);Oev.HSD
#Duration
qqnorm(Odur)
shapiro.test(Odur) #fail
bartlett.test(Odur~station) #fail
Odur.aov <-  aov(Odur~station);summary(Odur.aov)
Odur.HSD <- TukeyHSD(Odur.aov);Odur.HSD
Odur.HSD <- HSD.test(Odur.aov,"station",group=T);Odur.HSD

#A
#N events
qqnorm(Aev)
shapiro.test(Aev) #fail
bartlett.test(Aev~station) #fail
Aev.aov <-    aov(Aev~station);summary(Aev.aov)
Aev.HSD <- TukeyHSD(Aev.aov);Aev.HSD
Aev.HSD <- HSD.test(Aev.aov,"station",group=T);Aev.HSD
#Duration
qqnorm(Adur)
shapiro.test(Adur) #fail
bartlett.test(Adur~station) #fail
Adur.aov <-  aov(Adur~station);summary(Adur.aov)
Adur.HSD <- TukeyHSD(Adur.aov);Adur.HSD
Adur.HSD <- HSD.test(Adur.aov,"station",group=T);Adur.HSD

#B
#N events
qqnorm(Bev)
shapiro.test(Bev) #fail
bartlett.test(Bev~station) #fail
Bev.aov <-    aov(Bev~station);summary(Bev.aov)
Bev.HSD <- TukeyHSD(Bev.aov);Bev.HSD
Bev.HSD <- HSD.test(Bev.aov,"station",group=T);Bev.HSD
#Duration
qqnorm(Bdur)
shapiro.test(Bdur) #fail
bartlett.test(Bdur~station) #fail
Bdur.aov <-  aov(Bdur~station);summary(Bdur.aov)
Bdur.HSD <- TukeyHSD(Bdur.aov);Bdur.HSD
Bdur.HSD <- HSD.test(Bdur.aov,"station",group=T);Bdur.HSD

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Cluster Analysis --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Packages 
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#Step 1 scale data
#using extraction results: Wetlands no leaf litter (WetlandsNoLL)

#trial run using all the variables (I think this leads to PCA)
#select only data you want in cluster analysis, ensure no NA's
data <- WetlandsNoLL %>% select(#Month,wetland,station,Generic_Horizon,
                                 EOC_mgC_L,FI,HIX,SUVA254_L_mgm)
data <- drop_na(data)

#scale data (only columns with numeric values)
data[c(5,9)] <- scale(data[c(5,9)])

#scale using data only (no wetland or horizon name)
data <- scale(data)

#Step 2 K-means algorithm
#specify number of clusters, number of configurations (usually start with 25)
k3 <- kmeans(data,centers=3,nstart=25);k3
k4 <- kmeans(data,centers=4,nstart=25);k4
k5 <- kmeans(data,centers=5,nstart=25);k5
k6 <- kmeans(data,centers=6,nstart=25);k6

#Step 3 view results
p3 <- fviz_cluster(k3,data=data)
p4 <- fviz_cluster(k4,data=data)
p5 <- fviz_cluster(k5,data=data)
p6 <- fviz_cluster(k6,data=data)

library(gridExtra)
grid.arrange(p3, p4, p5, p6, nrow = 2)

#Step 4 determine optical number of clusters
#want average silhouette width to be high for determining optimal number
fviz_nbclust(data, kmeans, method = "silhouette")
#look for bend in plot to determine optimal number
fviz_nbclust(data, kmeans, method = "wss")
#two is best, 4 second best for optimizing number of clusters

