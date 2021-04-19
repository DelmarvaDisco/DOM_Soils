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
shapiro.test(logEOC) #Log of values is normally distributed
Station <- WetlandsNoLL$station
Horizon <- WetlandsNoLL$Generic_Horizon
bartlett.test(logEOC~Station)
bartlett.test(logEOC~Horizon)

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
#EOC
ggplot(data, aes(mean_depth_m,EOC_mgC_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("Mean Depth to Water Table (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')

ggplot(data, aes(mean_depth_m,EOC_mgC_L)) +
  geom_point(size=2.5) +
  xlab("Mean Depth to Water Table (m)") +
  ylab("EOC (mg/L)") + 
  ggtitle("Wetland EOC vs Mean WL") + 
  theme_bw()+
  geom_smooth(method = 'lm')


O <- data %>% filter(Generic_Horizon == "1O")
A <- data %>% filter(Generic_Horizon == "2A")
B <- data %>% filter(Generic_Horizon == "3B")

lmEOCO <- lm(O$mean_depth_m~O$EOC_mgC_L)
summary(lmEOCO)
lmEOCA <- lm(A$mean_depth_m~A$EOC_mgC_L)
summary(lmEOCA)
lmEOCB <- lm(B$mean_depth_m~B$EOC_mgC_L)
summary(lmEOCB)
lmoverall <- lm(data$mean_depth_m~data$EOC_mgC_L)
summary(lmoverall)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 ANOVA --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC by horizon
logEOC.horiz.aov <- aov(logEOC~Horizon)
summary(logEOC.horiz.aov)
logEOC.horiz.HSD <- TukeyHSD(logEOC.horiz.aov);logEOC.horiz.HSD 

#EOC by station
logEOC.sta.aov <- aov(logEOC~Station)
summary(logEOC.sta.aov)
logEOC.sta.HSD <- TukeyHSD(logEOC.sta.aov);logEOC.sta.HSD 

#EOC by both horizon and transect location
logEOC.both.aov <- aov(logEOC~Horizon+Station)
summary(logEOC.both.aov)
logEOC.both.HSD <- TukeyHSD(logEOC.both.aov);logEOC.both.HSD  




