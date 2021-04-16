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

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv")
annual <- read_csv("data/annual_metrics.csv")

#Filter to just wetlands and no leaf litter
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Exploratory Data Analysis--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Look at distribution
#EOC
hist(WetlandsNoLL$EOC_mgC_L)
qqnorm(WetlandsNoLL$EOC_mgC_L)
shapiro.test(WetlandsNoLL$EOC_mgC_L) #EOC values are not normally distributed

#FI
qqnorm(df$FI)
shapiro.test(df$FI)

#Log transform data
#EOC
logEOC <- log(WetlandsNoLL$EOC_mgC_L)
qqnorm(logEOC)
shapiro.test(logEOC) #Log of values is normally distributed
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon
bartlett.test(logEOC~Station)
bartlett.test(logEOC~Horizon)

#FI
logFI <- log(df$FI)
qqnorm(logFI)
shapiro.test(logFI) #logging FI data is not normally distributed

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Linear Regression --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 ANOVA --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC
logEOC.aov <- aov(logEOC~Horizon)
summary(logEOC.aov)
logEOC.HSD <- TukeyHSD(logEOC.aov);logEOC.HSD 



