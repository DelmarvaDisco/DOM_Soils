#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: ESOM Data
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 4/1/2021
#Updated: 4/9/2021
#Purpose: Explore ESOM results by horizon and transect location
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
library(dplyr)
library(ggpubr)

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv") #extraction data
synoptic <- read_csv("data/R_Extraction_Synoptic_Combined.csv") #extraction combined with synoptic

#Filter out Channel and Forested Flat
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetSynoptic <- synoptic %>% filter(Wetland_ID %in% c("QB","TB","DB","ND")) 
#Filter out Leaf Litter
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")
WetSynopticNoLL <- WetSynoptic %>% filter(Point != "6 LL")

#Separate months
#ESOM only
JanMar <- WetlandsNoLL %>% filter(Month %in% c('2020-01','2020-03'))
Sept <- WetlandsNoLL %>% filter(Month == '2020-09')
Nov <- WetlandsNoLL %>% filter(Month == "2020-11")

#Synoptic
JanMar <- WetSynopticNoLL  %>% filter(Month %in% c('2020-01','2020-03'))
Sept <- WetSynopticNoLL  %>% filter(Month == '2020-09')
Nov <- WetSynopticNoLL  %>% filter(Month == "2020-11")

#Separate wetland sites
QB <- WetlandsNoLL %>% filter(wetland == "QB")
TB <- WetlandsNoLL %>% filter(wetland == "TB")
DB <- WetlandsNoLL %>% filter(wetland == "DB")
ND <- WetlandsNoLL %>% filter(wetland == "ND")

#General summary of EOC
EOC_Summary <- WetlandsNoLL %>% group_by(station,Generic_Horizon) %>% 
                summarise(mean_EOC_mgC_L = mean(EOC_mgC_L),
                          mean_EOC_mgC_gsoil = mean(EOC_mgC_gsoil),
                          meanlayerthickness_m = mean(Layer_Thickness_cm/100))

write_csv(EOC_Summary,"data//EOC_Summary.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Plots-----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.1  EOC ---------------------------------------

#EOC Boxplot along Transect Points
#Including leaf litter
ggplot(Wetlands,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  xlab("EOC (mg/L)") +
  ylab("Transect Point and Soil Horizon") + 
  ggtitle("EOC by Horizon and Transect Spot")
#No leaf litter
ggplot(WetlandsNoLL,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  xlab("EOC (mg/L)") +
  ylab("Transect Point and Soil Horizon") + 
  ggtitle("EOC by Horizon and Transect Spot")
#no leaf litter by month
#spring
ggplot(JanMar,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  xlab("mg EOC/L") + 
  xlim(0,25)+
  ylab("Transect Point and Soil Horizon") + 
  ggtitle("Spring EOC by Horizon and Transect Spot")+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#autumn
ggplot(Sept,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  xlab("mg EOC/L") + 
  xlim(0,25)+
  ylab("Transect Point and Soil Horizon") + 
  ggtitle("Autumn EOC by Horizon and Transect Spot")+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



#2.2 FI------------------------------------------
#FI by Horizon - All wetland sites, no LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ggtitle("All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#spring
ggplot(JanMar, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ylim(1.3,1.9)+
  xlim(0,26)+ 
  ggtitle("Spring FI vs EOC")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#autumn
ggplot(Sept, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("FI") +
  ylim(1.3,1.9)+
  xlim(0,26)+
  ggtitle("Autumn FI vs EOC")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#FI by point - All wetland sites, no LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,FI,col=Point)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ggtitle("All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#FI by point and horizon
ggplot(WetlandsNoLL, aes(EOC_mgC_L,FI,col=Number_Name)) +
  geom_point(size=2.5) +
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ggtitle("All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#FI boxplot by transect point
ggplot(WetlandsNoLL, aes(Point,FI,fill=Point)) +
  geom_boxplot()+
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ggtitle("All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#FI boxplot by transect point and horion
ggplot(WetlandsNoLL, aes(Point,FI,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ggtitle("All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#spring
ggplot(JanMar, aes(Point,FI,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Transect Point") +
  ylab("FI") + 
  ylim(1.3,1.9)+
  ggtitle("Spring FI - All Sites")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#autumn
ggplot(Sept, aes(Point,FI,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Transect Point") +
  ylab("FI") + 
  ylim(1.3,1.9)+
  ggtitle("Autumn FI - All Sites")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#FI boxplot by transect point and horizon, wrap by wetland
#Note: DB and ND show more variability in values
ggplot(WetlandsNoLL, aes(Point,FI,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("EOC (mg/L)") +
  ylab("FI") + 
  ggtitle("All Sites FI")+ 
  theme_bw() +
  facet_wrap(~wetland)+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#2.3 SUVA254 -------------------------------------------
#SUVA by Horizon - All wetland sites, No LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  ggtitle("All Sites SUVA254 vs EOC") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#spring
ggplot(JanMar, aes(EOC_mgC_L,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  xlim(0,26)+
  ylim(0,4)+
  ggtitle("Spring SUVA254 vs EOC") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#autumn
ggplot(Sept, aes(EOC_mgC_L,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  xlim(0,26)+
  ylim(0,4)+
  ggtitle("Autumn SUVA254 vs EOC") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA vs FI
ggplot(WetlandsNoLL, aes(FI,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("FI") +
  ylab("SUVA254") +
  ylim(0,4)+
  ggtitle("All Sites FI vs SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#spring
ggplot(JanMar, aes(FI,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("FI") +
  ylab("SUVA254") +
  ylim(0,4)+
  xlim(1.3,1.9)+
  ggtitle("Spring FI vs SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#autumn
ggplot(Sept, aes(FI,SUVA254_L_mgm,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("FI") +
  ylab("SUVA254") + 
  ylim(0,4)+
  xlim(1.3,1.9)+
  ggtitle("Autumn FI vs SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA by Point - All wetland sites, No LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,SUVA254_L_mgm,shape=Point,col=Point)) +
  geom_point(size=2.5) +
  #stat_ellipse()+
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  ggtitle("All Sites SUVA254 vs EOC") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA by point and horizon - no LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,SUVA254_L_mgm,col=Number_Name)) +
  geom_point(size=2.5) +
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  ggtitle("Wetlands No LL SUVA")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA boxplot by Point - All wetalnd sites, No LL
ggplot(WetlandsNoLL, aes(Point,SUVA254_L_mgm,fill=Point)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SUVA254") + 
  ggtitle("All Sites SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA boxplot by Point - All wetalnd sites, No LL
#Spring
ggplot(JanMar, aes(Point,SUVA254_L_mgm,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SUVA254") + 
  ylim(0,3.5)+
  ggtitle("Spring SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#Autumn
ggplot(Sept, aes(Point,SUVA254_L_mgm,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SUVA254") + 
  ggtitle("Autumn SUVA254") + 
  theme_bw() +
  ylim(0,3.5)+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA boxplot by Horizon - All wetalnd sites, No LL
ggplot(WetlandsNoLL, aes(Generic_Horizon,SUVA254_L_mgm,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Horizon") +
  ylab("SUVA254") + 
  ggtitle("All Sites SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA boxplot by transect point and horizon
#Note: DB and ND show more variability in values (when wrap by wetland)
ggplot(WetlandsNoLL, aes(Point,SUVA254_L_mgm,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SUVA254") + 
  ggtitle("SUVA254")+ 
  theme_bw() +
  facet_wrap(~wetland)+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

# 2.4 Random ------------------------------------
#EOC vs ETDN
ggplot(WetlandsNoLL, aes(EOC_mgC_L,ETDN_mgN_L,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("EOC (mg C/L)") +
  ylab("ETDN (mg N/L)") + 
  ggtitle("Wetland Soils")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#EOC vs T
ggplot(WetlandsNoLL, aes(EOC_mgC_L,T,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("EOC (mg C/L)") +
  ylab("Peak T") + 
  ggtitle("Wetland Soils")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#EOC vs A
ggplot(WetlandsNoLL, aes(EOC_mgC_L,A,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("EOC (mg C/L)") +
  ylab("Peak A") + 
  ggtitle("Wetland Soils")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#EOC vs C
ggplot(WetlandsNoLL, aes(EOC_mgC_L,C,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("EOC (mg C/L)") +
  ylab("Peak C") + 
  ggtitle("Wetland Soils")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#EOC vs M
ggplot(WetlandsNoLL, aes(EOC_mgC_L,M,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  xlab("EOC (mg C/L)") +
  ylab("Peak M") + 
  ggtitle("Wetland Soils")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

# 2.5 HIX ----------------------------------------
#HIX boxplot by Point - All wetland sites, No LL
ggplot(WetlandsNoLL, aes(Point,HIX,fill=Point)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("HIX") + 
  ggtitle("All Sites HIX") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#spring
ggplot(JanMar, aes(Point,HIX,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("HIX") + 
  ylim(0.35,0.6)+
  ggtitle("Spring HIX") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#autumn
ggplot(Sept, aes(Point,HIX,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("HIX") + 
  ylim(0.35,0.6)+
  ggtitle("Autumn HIX") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#HIX vs FI
#spring
ggplot(JanMar, aes(FI,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("FI") +
  ylab("HIX") + 
  xlim(1.35,1.95)+
  ylim(0.3,0.65)+
  ggtitle("Spring FI vs HIX") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#autumn
ggplot(Sept, aes(FI,HIX,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  stat_ellipse()+
  xlab("FI") +
  ylab("HIX") + 
  xlim(1.35,1.95)+
  ylim(0.3,0.65)+
  ggtitle("Autumn FI vs HIX") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

# 2.6 SSR -------------------------------------
ggplot(WetlandsNoLL, aes(Point,SSR,fill=Point)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SSR") + 
  ggtitle("All Sites SSR") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ggplot(WetlandsNoLL, aes(Point,SSR,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SSR") + 
  ylim(1,5)+
  ggtitle("All Sites SSR") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#spring
ggplot(JanMar, aes(Point,SSR,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SSR") + 
  ylim(1,4.5)+
  ggtitle("Spring SSR") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#autumn
ggplot(Sept, aes(Point,SSR,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("Point") +
  ylab("SSR") +
  ylim(1,4.5) +
  ggtitle("Autumn SSR") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


#2.7 Compare months --------------------------------------------------

#2.7.1 QB only -------------------------
#QB EOC over the 3 sampling campaigns
ggplot(data=QB,aes(x=Generic_Horizon,y=EOC_mgC_L,fill=Generic_Horizon)) + 
  geom_boxplot()+
  theme_bw()+
  ylab("EOC (mg/L)") +
  xlab("Soil Horizon")+
  ggtitle("QB EOC vs Month") + 
  facet_wrap(~Month)

#FI by horizon and month - shape is point on transect
ggplot(data=QB,aes(x=Generic_Horizon,y=FI,fill=Generic_Horizon)) + 
  geom_boxplot() +
  ylab("FI") +
  xlab("Soil Horizon")+
  theme_bw() +
  facet_wrap(~Month)

#FI vs Month colored by point
ggplot(data=QB,aes(x=Point,y=FI,fill=Point)) + 
  geom_boxplot() +
  ylab("FI") +
  ggtitle("QB FI vs Month") +
  theme_bw()+
  facet_wrap(~Month)

#SUVA vs Month colored by horizon
ggplot(data=QB,aes(x=Generic_Horizon,y=SUVA254_L_mgm,fill=Generic_Horizon)) + 
  geom_boxplot() +
  ylab("SUVA254") +
  ggtitle("QB SUVA vs Month") +
  theme_bw()+
  facet_wrap(~Month)

#SUVA vs Month colored by point
ggplot(data=QB,aes(x=Point,y=SUVA254_L_mgm,fill=Point)) + 
  geom_boxplot() +
  ylab("SUVA254") +
  ggtitle("QB SUVA vs Month") +
  theme_bw()+
  facet_wrap(~Month)

#SUVA by horizon and month - shape is point on transect
ggplot(data=QB,aes(x=Generic_Horizon,y=SUVA254_L_mgm,shape=Point,size=0.25)) + 
  geom_point() +
  ylab("SUVA254") +
  xlab("Soil Horizon")+
  theme_bw() +
  facet_wrap(~Month)

#HIX vs Month colored by point
ggplot(data=QB,aes(x=Point,y=HIX,fill=Point)) + 
  geom_boxplot() +
  ylab("HIX") +
  ggtitle("QB HIX vs Month") +
  theme_bw()+
  facet_wrap(~Month)

#Moisture by horizon and month - shape is point on transect
ggplot(data=QB,aes(x=Point,y=Percent_Soil_Moisture_notin,color=Generic_Horizon)) + 
  geom_point(size=4) +
  ylab("Soil Moisture Content (%)")+
  theme_bw() + 
  facet_wrap(~Month)

#2.7.2 All sites ---------------------------
#SUVA
ND_SUVA <- ggplot(data=ND,aes(x=Point,y=SUVA254_L_mgm,col=Generic_Horizon,shape=Generic_Horizon)) + 
  geom_point(size=3) +
  ylab("SUVA254") +
  xlab("Point")+
  ggtitle("ND") +
  theme_bw() +
  facet_wrap(~Month)

DB_SUVA <- ggplot(data=DB,aes(x=Point,y=SUVA254_L_mgm,col=Generic_Horizon,shape=Generic_Horizon)) + 
  geom_point(size=3) +
  ylab("SUVA254") +
  xlab("Point")+
  ggtitle("DB") +
  theme_bw() +
  facet_wrap(~Month)

QB_SUVA <- ggplot(data=QB,aes(x=Point,y=SUVA254_L_mgm,col=Generic_Horizon,shape=Generic_Horizon)) + 
  geom_point(size=3) +
  ylab("SUVA254") +
  xlab("Point")+
  ggtitle("QB") +
  theme_bw() +
  facet_wrap(~Month)

TB_SUVA <- ggplot(data=TB,aes(x=Point,y=SUVA254_L_mgm,col=Generic_Horizon,shape=Generic_Horizon)) + 
  geom_point(size=3) +
  ylab("SUVA254") +
  xlab("Point")+
  ggtitle("TB") +
  theme_bw() +
  facet_wrap(~Month)

SUVA_Wetland <- ggarrange( ND_SUVA, 
                           DB_SUVA, 
                           QB_SUVA, 
                           TB_SUVA,
                         ncol = 2, nrow = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 PARAFAC Results--------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.1 Cory and McKnight Model ----------------------------------------
#loadings across all sites
boxplot(WetlandsNoLL$C1, 
        WetlandsNoLL$C2_Q2, 
        WetlandsNoLL$C3, 
        WetlandsNoLL$C4_HQ, 
        WetlandsNoLL$C5_SQ1, 
        WetlandsNoLL$C6,
        WetlandsNoLL$C7_SQ2, 
        WetlandsNoLL$C8_Trypto, 
        WetlandsNoLL$C9_SQ3, 
        WetlandsNoLL$C10,
        WetlandsNoLL$C11_Q1,
        WetlandsNoLL$C12_Q3,
        WetlandsNoLL$C13_Tyrosine,
        main="Percent Loading of Each Component",
        names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13"),
        ylab="Loading (%)",
        xlab="Component")

#by sampling period
#loadings across all sites
#spring
boxplot(JanMar$C1, 
        JanMar$C2_Q2, 
        JanMar$C3, 
        JanMar$C4_HQ, 
        JanMar$C5_SQ1, 
        JanMar$C6,
        JanMar$C7_SQ2, 
        JanMar$C8_Trypto, 
        JanMar$C9_SQ3, 
        JanMar$C10,
        JanMar$C11_Q1,
        JanMar$C12_Q3,
        JanMar$C13_Tyrosine,
        ylim=c(0,0.6),
        main="Spring Percent Loading of Each Component",
        names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13"),
        ylab="Loading (%)",
        xlab="Component")

#autumn
boxplot(Sept$C1, 
        Sept$C2_Q2, 
        Sept$C3, 
        Sept$C4_HQ, 
        Sept$C5_SQ1, 
        Sept$C6,
        Sept$C7_SQ2, 
        Sept$C8_Trypto, 
        Sept$C9_SQ3, 
        Sept$C10,
        Sept$C11_Q1,
        Sept$C12_Q3,
        Sept$C13_Tyrosine,
        ylim=c(0,0.6),
        main="Autumn Percent Loading of Each Component",
        names=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13"),
        ylab="Loading (%)",
        xlab="Component")



#boxplot of most notable components broken out by horizon
CM_C2 <- ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C2_Q2,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()
CM_C4 <- ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C4_HQ,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()
CM_C8 <- ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C8_Trypto,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C8")+
  ggtitle("%C8 by Point")+
  theme_bw()
CM_C12 <- ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C12_Q3,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C12")+
  ggtitle("%C12 by Point")+
  theme_bw()
CM_C13 <- ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C13_Tyrosine,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C13")+
  ggtitle("%C13 by Point")+
  theme_bw()
  
CM_Boxplot <- ggarrange( CM_C2, 
                         CM_C4, 
                         CM_C8, 
                         CM_C12,
                         CM_C13,
                         ncol = 2, nrow = 3)

#boxplot of most notable components broken out by horizon - Spring
CM_C2 <- ggplot(data=JanMar) +
  geom_boxplot(aes(x=Point,y=C2_Q2,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()
CM_C4 <- ggplot(data=JanMar) +
  geom_boxplot(aes(x=Point,y=C4_HQ,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()
CM_C8 <- ggplot(data=JanMar) +
  geom_boxplot(aes(x=Point,y=C8_Trypto,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C8")+
  ggtitle("%C8 by Point")+
  theme_bw()
CM_C12 <- ggplot(data=JanMar) +
  geom_boxplot(aes(x=Point,y=C12_Q3,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C12")+
  ggtitle("%C12 by Point")+
  theme_bw()
CM_C13 <- ggplot(data=JanMar) +
  geom_boxplot(aes(x=Point,y=C13_Tyrosine,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C13")+
  ggtitle("%C13 by Point")+
  theme_bw()

CM_Boxplot <- ggarrange( CM_C2, 
                         CM_C4, 
                         CM_C8, 
                         CM_C12,
                         CM_C13,
                         ncol = 2, nrow = 3)

#boxplot of most notable components broken out by horizon - Fall
CM_C2 <- ggplot(data=Sept) +
  geom_boxplot(aes(x=Point,y=C2_Q2,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()
CM_C4 <- ggplot(data=Sept) +
  geom_boxplot(aes(x=Point,y=C4_HQ,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()
CM_C8 <- ggplot(data=Sept) +
  geom_boxplot(aes(x=Point,y=C8_Trypto,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C8")+
  ggtitle("%C8 by Point")+
  theme_bw()
CM_C12 <- ggplot(data=Sept) +
  geom_boxplot(aes(x=Point,y=C12_Q3,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C12")+
  ggtitle("%C12 by Point")+
  theme_bw()
CM_C13 <- ggplot(data=Sept) +
  geom_boxplot(aes(x=Point,y=C13_Tyrosine,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C13")+
  ggtitle("%C13 by Point")+
  theme_bw()

CM_Boxplot <- ggarrange( CM_C2, 
                         CM_C4, 
                         CM_C8, 
                         CM_C12,
                         CM_C13,
                         ncol = 2, nrow = 3)
##C2##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C2_Q2,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C2")+
  ggtitle("%C2 vs EOC")+
  theme_bw()
#SUVA
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=SUVA254_L_mgm,y=C2_Q2,col=Generic_Horizon),size=3) +
  xlab("SUVA254 (L/mg-m)") +
  ylab("%C2")+
  ggtitle("%C2 vs SUVA254")+
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

##C4##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C4_HQ,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C4")+
  ggtitle("%C4 vs EOC")+
  theme_bw()
#SUVA
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=SUVA254_L_mgm,y=C4_HQ,col=Generic_Horizon)) +
  xlab("SUVA254 (L/mg-m)") +
  ylab("%C4")+
  ggtitle("%C4 vs SUVA254")+
  theme_bw()

##C8##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C8_Trypto,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C8")+
  ggtitle("%C8 vs EOC")
#FI
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=FI,y=C8_Trypto,col=Generic_Horizon),size=3) +
  xlab("FI") +
  ylab("%C8")+
  ggtitle("%C8 vs FI")+
  theme_bw()
#Transect Location
ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C8_Trypto,fill=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C8")+
  ggtitle("%C8 by Transect Location")+
  theme_bw()

##C10##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C10,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C10")+
  ggtitle("%C10 vs EOC")
#FI
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=FI,y=C10,col=Generic_Horizon)) +
  xlab("FI") +
  ylab("%C10")+
  ggtitle("%C10 vs FI")

##C11##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C11_Q1,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C11")+
  ggtitle("%C11 vs EOC")
#FI
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=FI,y=C11_Q1,col=Generic_Horizon)) +
  xlab("FI") +
  ylab("%C11")+
  ggtitle("%C11 vs FI")

##C12##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C12_Q3,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C12")+
  ggtitle("%C12 vs EOC")
#FI
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=FI,y=C12_Q3,col=Generic_Horizon)) +
  xlab("FI") +
  ylab("%C12")+
  ggtitle("%C12 vs FI")
#SUVA
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=SUVA254_L_mgm,y=C12_Q3,col=Generic_Horizon)) +
  xlab("SUVA254 (L/mg-m)") +
  ylab("%C12")+
  ggtitle("%C12 vs SUVA254")
#Transect Location
ggplot(data=WetlandsNoLL) +
  geom_boxplot(aes(x=Point,y=C12_Q3,col=Generic_Horizon)) +
  xlab("Point") +
  ylab("%C12")+
  ggtitle("%C12 by Transect Location")+
  theme_bw()

##C13##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C13_Tyrosine,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C13")+
  ggtitle("%C13 vs EOC")
#FI
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=FI,y=C13_Tyrosine,col=Generic_Horizon)) +
  xlab("FI") +
  ylab("%C13")+
  ggtitle("%C13 vs FI")

#3.2 Delmarva Synoptic Model -----------------------------------------
#boxplot of loadings across all samples - no leaf litter
boxplot(WetlandsNoLL$DMV_C1, 
        WetlandsNoLL$DMV_C2, 
        WetlandsNoLL$DMV_C3, 
        WetlandsNoLL$DMV_C4,
        main="Percent Loading of Each Component",
        names=c("C1","C2","C3","C4"),
        ylab="Loading (%)",
        xlab="Component")
#spring
boxplot(JanMar$DMV_C1, 
        JanMar$DMV_C2, 
        JanMar$DMV_C3, 
        JanMar$DMV_C4,
        ylim=c(0,0.75),
        main="Spring Percent Loading of Each Component",
        names=c("C1","C2","C3","C4"),
        ylab="Loading (%)",
        xlab="Component")
#autumn
boxplot(Sept$DMV_C1, 
        Sept$DMV_C2, 
        Sept$DMV_C3, 
        Sept$DMV_C4,
        ylim=c(0,0.75),
        main="Autumn Percent Loading of Each Component",
        names=c("C1","C2","C3","C4"),
        ylab="Loading (%)",
        xlab="Component")


#boxplot of loadings across all samples - including leaf litter
boxplot(Wetlands$DMV_C1, 
        Wetlands$DMV_C2, 
        Wetlands$DMV_C3, 
        Wetlands$DMV_C4,
        main="Percent Loading of Each Component",
        names=c("C1","C2","C3","C4"),
        ylab="Loading (%)",
        xlab="Component")

#loading by horizon
Box_C1 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C1,fill=Generic_Horizon))+ 
  xlab("Transect Point") +
  ylab("%C1")+
  ggtitle("%C1 by Point")+
  theme_bw()
Box_C2 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C2,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()
Box_C3 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C3,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C3")+
  ggtitle("%C3 by Point")+
  theme_bw()
Box_C4 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C4,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()

DMV_Boxplot <- ggarrange(Box_C1, 
                         Box_C2, 
                         Box_C3, 
                         Box_C4,
                         labels = c("1", "2","3","4"),
                         ncol = 2, nrow = 2)

#loading by horizon - spring
Box_C1 <- ggplot(data=JanMar)+
  geom_boxplot(aes(x=Point,y=DMV_C1,fill=Generic_Horizon))+ 
  xlab("Transect Point") +
  ylab("%C1")+
  ggtitle("%C1 by Point")+
  theme_bw()
Box_C2 <- ggplot(data=JanMar)+
  geom_boxplot(aes(x=Point,y=DMV_C2,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()
Box_C3 <- ggplot(data=JanMar)+
  geom_boxplot(aes(x=Point,y=DMV_C3,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C3")+
  ggtitle("%C3 by Point")+
  theme_bw()
Box_C4 <- ggplot(data=JanMar)+
  geom_boxplot(aes(x=Point,y=DMV_C4,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()

DMV_Boxplot <- ggarrange(Box_C1, 
                         Box_C2, 
                         Box_C3, 
                         Box_C4,
                         labels = c("1", "2","3","4"),
                         ncol = 2, nrow = 2)

#component by wetland
DMV_C1 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C1,fill=Generic_Horizon))+ 
  xlab("Transect Point") +
  ylab("%C1")+
  ggtitle("%C1 by Point")+
  theme_bw()+
  facet_wrap(~wetland)
DMV_C2 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C2,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()+
  facet_wrap(~wetland)
DMV_C3 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C3,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C3")+
  ggtitle("%C3 by Point")+
  theme_bw()+
  facet_wrap(~wetland)
DMV_C4 <- ggplot(data=WetlandsNoLL)+
  geom_boxplot(aes(x=Point,y=DMV_C4,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()+
  facet_wrap(~wetland)

DMV_Boxplot <- ggarrange(DMV_C1, 
                         DMV_C2, 
                         DMV_C3, 
                         DMV_C4,
                         labels = c("C1", "C2","C3","C4"),
                         ncol = 2, nrow = 2)

#loading by horizon - spring
Box_C1 <- ggplot(data=Sept)+
  geom_boxplot(aes(x=Point,y=DMV_C1,fill=Generic_Horizon))+ 
  xlab("Transect Point") +
  ylab("%C1")+
  ggtitle("%C1 by Point")+
  theme_bw()
Box_C2 <- ggplot(data=Sept)+
  geom_boxplot(aes(x=Point,y=DMV_C2,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C2")+
  ggtitle("%C2 by Point")+
  theme_bw()
Box_C3 <- ggplot(data=Sept)+
  geom_boxplot(aes(x=Point,y=DMV_C3,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C3")+
  ggtitle("%C3 by Point")+
  theme_bw()
Box_C4 <- ggplot(data=Sept)+
  geom_boxplot(aes(x=Point,y=DMV_C4,fill=Generic_Horizon))+
  xlab("Transect Point") +
  ylab("%C4")+
  ggtitle("%C4 by Point")+
  theme_bw()

DMV_Boxplot <- ggarrange(Box_C1, 
                         Box_C2, 
                         Box_C3, 
                         Box_C4,
                         labels = c("1", "2","3","4"),
                         ncol = 2, nrow = 2)

#Components vs fluorescence metrics
ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=SUVA254_L_mgm,y=DMV_C2,col=Generic_Horizon,shape=Point,size=2)) +
  xlab("SUVA254 (L/mg-m)") +
  ylab("%C2")+
  ggtitle("%C2 vs SUVA254")+
  theme_bw()

ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=FI,y=DMV_C4,col=Generic_Horizon,shape=Point,size=2)) +
  xlab("FI") +
  ylab("%C4")+
  ggtitle("%C4 vs FI")+
  theme_bw()

ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=HIX,y=DMV_C1,col=Generic_Horizon,shape=Point,size=2)) +
  xlab("HIX") +
  ylab("%C1")+
  ggtitle("%C1 vs HIX")+
  theme_bw()

ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=SSR,y=DMV_C1,col=Generic_Horizon,shape=Point)) +
  xlab("SSR") +
  ylab("%C1")+
  ggtitle("%C1 vs SSR")+
  theme_bw()

#Components vs EOC
#C1
ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=EOC_mgC_L,y=DMV_C1,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C1")+
  ggtitle("%C1 vs EOC")+
  theme_bw()

ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=EOC_mgC_L,y=DMV_C1,col=Point)) +
  xlab("EOC (mg/L)") +
  ylab("%C1")+
  ggtitle("%C1 vs EOC")+
  theme_bw()

#C2
ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=EOC_mgC_L,y=DMV_C2,col=Generic_Horizon)) +
  xlab("EOC (mg C/L)") +
  ylab("%C2")+
  ggtitle("%C2 vs EOC")+
  theme_bw()

#C3
ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=EOC_mgC_L,y=DMV_C3,col=Generic_Horizon,shape=Point)) +
  xlab("EOC (mg C/L)") +
  ylab("%C3")+
  ggtitle("%C3 vs EOC")+
  theme_bw()

#C4
ggplot(data=WetlandsNoLL)+
  geom_point(aes(x=EOC_mgC_L,y=DMV_C4,col=Generic_Horizon,shape=Point)) +
  xlab("EOC (mg C/L)") +
  ylab("%C4")+
  ggtitle("%C4 vs EOC")+
  theme_bw()

#4.0 ESOM and Synoptic Data Together-----------------------------------

#EOC Boxplot along Transect Points
ggplot(WetSynoptic,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("EOC by Horizon and Transect Spot")

#spring
ggplot(JanMar,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  ylab("Transect Point and Horizon")+
  xlab("EOC / DOC (mg C/L)")+
  xlim(0,40)+
  theme_bw()+
  ggtitle("Spring EOC and DOC")
#autumn
ggplot(Sept,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
    geom_boxplot()+
    ylab("Transect Point and Horizon")+
    xlab("EOC / DOC (mg C/L)")+
    xlim(0,40)+
    theme_bw()+
    ggtitle("Autumn EOC and DOC")
  
#EOC by Month
ggplot(WetSynoptic,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("EOC by Horizon and Transect Spot")+
  facet_wrap(~Month)

#FI by Horizon - All wetland sites
ggplot(WetSynoptic, aes(EOC_mgC_L,FI,col=Generic_Horizon,shape=Wetland_ID)) +
  geom_point(size=3.5) +
  #stat_ellipse()+
  xlab("EOC / DOC (mg/L)") +
  ylab("FI") + 
  #stat_ellipse()+
  ggtitle("All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#Spring
ggplot(JanMar, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  #stat_ellipse()+
  xlab("EOC / DOC (mg/L)") +
  ylab("FI") +  
  xlim(0,40)+
  ylim(1.4,2.0)+
  ggtitle("Spring All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#Aumtun
ggplot(Sept, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=3.5) +
  #stat_ellipse()+
  xlab("EOC / DOC (mg/L)") +
  ylab("FI") + 
  xlim(0,40)+
  ylim(1.4,2.0)+
  #stat_ellipse()+
  ggtitle("Autumn All Sites FI")+ 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


#FI vs Location
ggplot(WetSynoptic,aes(x=Point,y=FI,fill=Generic_Horizon)) + 
  geom_boxplot() +
  xlab("Transect Point") +
  ylab("FI")+
  ggtitle("FI vs Horizon") +
  theme_bw()

#Spring
ggplot(JanMar,aes(x=Point,y=FI,fill=Generic_Horizon)) + 
  geom_boxplot() +
  xlab("Transect Point") +
  ylab("FI")+
  ylim(1.4,2.1)+
  ggtitle("Spring FI") +
  theme_bw()
#Autumn
ggplot(Sept,aes(x=Point,y=FI,fill=Generic_Horizon)) + 
  geom_boxplot() +
  xlab("Transect Point") +
  ylab("FI")+
  ylim(1.4,2.1)+
  ggtitle("Autumn FI") +
  theme_bw()

#FI vs Month boxplot by transect point
ggplot(WetSynoptic,aes(x=Point,y=FI,fill=Generic_Horizon)) + 
  geom_boxplot()+
  theme_bw()+
  ylab("FI") +
  xlab("Point")+
  facet_wrap(~Month)

ggplot(WetSynoptic,aes(x=Point,y=FI)) + 
  geom_boxplot()+
  theme_bw()+
  ylab("FI") +
  xlab("Point")+
  facet_wrap(~Month)

#SSR vs FI colored by horizon
ggplot(WetSynoptic,aes(x=FI,y=SSR,col=Generic_Horizon)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("SSR") +
  ggtitle("SSR vs FI") +
  theme_bw()

#5.0 Other variables-----------------------------------

#FI vs SSR
ggplot(WetlandsNoLL,aes(x=FI,y=SSR,col=Generic_Horizon)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("SSR") +
  ggtitle("SSR vs FI") +
  theme_bw()
ggplot(WetlandsNoLL,aes(x=FI,y=SSR,col=Point)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("SSR") +
  ggtitle("SSR vs FI") +
  theme_bw()

#FI vs SUVA
ggplot(WetlandsNoLL,aes(x=FI,y=SUVA254_L_mgm,col=Generic_Horizon)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("SUVA254") +
  ggtitle("SUVA254 vs FI") +
  theme_bw()
ggplot(WetlandsNoLL,aes(x=FI,y=SUVA254_L_mgm,col=Point)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("SUVA254") +
  ggtitle("SUVA254 vs FI") +
  theme_bw()

#FI vs HIX
ggplot(WetlandsNoLL,aes(x=FI,y=HIX,col=Generic_Horizon)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("HIX") +
  ggtitle("HIX vs FI") +
  theme_bw()+
  stat_ellipse()
ggplot(WetlandsNoLL,aes(x=FI,y=HIX,col=Point)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("HIX") +
  ggtitle("HIX vs FI") +
  theme_bw()

#FI vs %Protein
ggplot(WetlandsNoLL,aes(x=FI,y=Percent_Protein,col=Generic_Horizon)) + 
  geom_point(size=3) +
  xlab("FI")+
  ylab("%Protein") +
  ggtitle("%Protein vs FI") +
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
ggplot(WetlandsNoLL,aes(x=FI,y=Percent_Protein,col=Point)) + 
  geom_point(size=2) +
  xlab("FI")+
  ylab("%Protein") +
  ggtitle("%Protein vs FI") +
  theme_bw()

##HIX##
#Break out horizon at each point
ggplot(WetlandsNoLL,aes(x=Point,y=HIX,fill=Generic_Horizon)) + 
  geom_boxplot() +
  xlab("Transect Point")+
  ylab("HIX") +
  ggtitle("HIX by Point and Horizon") +
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#Point only
ggplot(WetlandsNoLL,aes(x=Point,y=HIX,fill=Point)) + 
  geom_boxplot() +
  xlab("Point")+
  ylab("HIX") +
  ggtitle("HIX by Transect Point") +
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

## %Clay ##
#EOC vs clay
ggplot(WetlandsNoLL,aes(x=Percent_Clay,y=EOC_mgC_L,color=Generic_Horizon)) + 
  geom_point() +
  xlab("%Clay")+
  ylab("EOC (mgC/L)") +
  ggtitle("EOC vs %Clay") +
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#FI vs %Clay
ggplot(WetlandsNoLL,aes(x=Percent_Clay,y=FI,color=Generic_Horizon)) + 
  geom_point() +
  xlab("%Clay")+
  ylab("FI") +
  ggtitle("FI vs %Clay") +
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
#SUVA254 vs %Clay
ggplot(WetlandsNoLL,aes(x=Percent_Clay,y=SUVA254_L_mgm,color=Generic_Horizon)) + 
  geom_point() +
  xlab("%Clay")+
  ylab("SUVA254") +
  ggtitle("SUVA254 vs %Clay") +
  theme_bw()+
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Correlation Exploration ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(corrplot)
library(Hmisc)

#cor
glimpse(Wetlands)
wetlandcor <- Wetlands %>% select(where(is.numeric))
wetlandcor2 <- wetlandcor[,10:55]
spearman <- cor(wetlandcor2,method="spearman",use="complete.obs")
corrplot(spearman,type="upper",tl.col = "black", tl.srt = 45,tl.cex=0.5)

#rcorr - which shows if correlations are signficant within the cor plot
trial <- rcorr(as.matrix(wetlandcor2))
# Extract the correlation coefficients
trial$r
# Extract p-values
trial$P
corrplot(trial$r, type="upper", 
         p.mat = trial$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45,tl.cex=0.5)
