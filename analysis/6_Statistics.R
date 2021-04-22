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
annual <- read_csv("data/katie_annual_metrics_practice.csv")

#Filter to just wetlands and no leaf litter
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")

#Join data
data <- inner_join(df, annual, by=c("wetland","station"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Exploratory Data Analysis--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Look at distribution
#EOC
hist(WetlandsNoLL$EOC_mgC_L)
qqnorm(WetlandsNoLL$EOC_mgC_L)
shapiro.test(WetlandsNoLL$EOC_mgC_L) #EOC values are not normally distributed

#Log transform data
#EOC
logEOC <- log(WetlandsNoLL$EOC_mgC_L)
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


#FI
qqnorm(df$FI)
shapiro.test(df$FI)
logFI <- log(df$FI)
qqnorm(logFI)
shapiro.test(logFI) #logging FI data is not normally distributed


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Linear Regression --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using fake water level data for practice
#Mean WL
#EOC by horizon
my.formula <- data$EOC_mgC_L~data$mean_depth_m
ggplot(data, aes(mean_depth_m,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Depth to Water Table (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = "lm",formula=my.formula,se=FALSE)
  #stat_poly_eq(formula=my.formula,
               #aes(label=paste(..eq.label..,..rr.label..,sep=="~~~")), parse=TRUE )
#log data
ggplot(data, aes(mean_depth_m,log(EOC_mgC_L),col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Depth to Water Table (m)") +
  ylab("log(EOC) (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#all data
ggplot(data, aes(mean_depth_m,EOC_mgC_L)) +
  geom_point(size=2.5) +
  xlab("Mean Depth to Water Table (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')
#log data
ggplot(data, aes(mean_depth_m,log(EOC_mgC_L))) +
  geom_point(size=2.5) +
  xlab("Mean Depth to Water Table (m)") +
  ylab("log(EOC) (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

O <- data %>% filter(Generic_Horizon == "1O")
A <- data %>% filter(Generic_Horizon == "2A")
B <- data %>% filter(Generic_Horizon == "3B")

#untransformed
lmEOCO <- lm(O$EOC_mgC_L~O$mean_depth_m)
summary(lmEOCO)
lmEOCA <- lm(A$EOC_mgC_L~A$mean_depth_m)
summary(lmEOCA)
lmEOCB <- lm(B$EOC_mgC_L~B$mean_depth_m)
summary(lmEOCB)
lmoverall <- lm(data$EOC_mgC_L~data$mean_depth_m)
summary(lmoverall)

#plot using base R
plot(O$EOC_mgC_L~O$mean_depth_m)
points(A$EOC_mgC_L~A$mean_depth_m,col="red")
points(B$EOC_mgC_L~B$mean_depth_m,col="blue")
abline(lmEOCO)
abline(lmEOCA,col="red")
abline(lmEOCB,col="blue")

#logtransformed
lmlogEOCO <- lm(O$mean_depth_m~log(O$EOC_mgC_L))
summary(lmlogEOCO)
lmlogEOCA <- lm(A$mean_depth_m~log(A$EOC_mgC_L))
summary(lmlogEOCA)
lmlogEOCB <- lm(B$mean_depth_m~log(B$EOC_mgC_L))
summary(lmlogEOCB)
lmlogoverall <- lm(data$mean_depth_m~log(data$EOC_mgC_L))
summary(lmlogoverall)

#N_events
#FI
ggplot(data, aes(n_events,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Number of saturation events") +
  ylab("FI") + 
  theme_bw()+
  geom_smooth(method = 'lm')

ggplot(data, aes(n_events,FI)) +
  geom_point(size=2.5) +
  xlab("Number of saturation events") +
  ylab("FI") + 
  theme_bw()+
  geom_smooth(method = 'lm')

lmFIO <- lm(O$n_events~O$FI)
summary(lmFIO)
lmFIA <- lm(A$n_events~A$FI)
summary(lmFIA)
lmFIB <- lm(B$n_events~B$FI)
summary(lmFIB)
lmFIoverall <-(data$n_events~data$FI)
summary(lmFIoverall)

#min water level
#EOC by horizon
ggplot(data, aes(min_depth_m,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Minimum Depth to Water Table (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 ANOVA --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC by horizon
logEOC.horiz.aov <- aov(logEOC~Horizon)
summary(logEOC.horiz.aov)
logEOC.horiz.HSD <- TukeyHSD(logEOC.horiz.aov);logEOC.horiz.HSD 

#one-way anova test for unequal variance
oneway.test(logEOC~Horizon)

#EOC by station
logEOC.sta.aov <- aov(logEOC~Station)
summary(logEOC.sta.aov)
logEOC.sta.HSD <- TukeyHSD(logEOC.sta.aov);logEOC.sta.HSD 

#one-way anova test for unequal variance
oneway.test(logEOC~Station)

#EOC by both horizon and transect location
logEOC.both.aov <- aov(logEOC~Horizon+Station)
summary(logEOC.both.aov)
logEOC.both.HSD <- TukeyHSD(logEOC.both.aov);logEOC.both.HSD  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Kruskal-Wallis --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC by horizon
kruskal.test(logEOC~Horizon)
kruskalmc(logEOC,Horizon,probs=0.05)
#EOC by station
kruskal.test(logEOC~Station)
kruskalmc(logEOC,Station,probs=0.05)

