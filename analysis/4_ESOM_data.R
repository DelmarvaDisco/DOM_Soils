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

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv") #extraction data
synoptic <- read_csv("data/R_Extraction_Synoptic_Combined.csv") #extraction combined with synoptic

#Separate months
JanMar <- df %>% filter(Month %in% c('2020-01','2020-03'))
Sept <- df %>% filter(Month == '2020-09')
Nov <- df %>% filter(Month == "2020-11")

#Separate wetland sites
QB <- df %>% filter(wetland == "QB")
TB <- df %>% filter(wetland == "TB")
DB <- df %>% filter(wetland == "DB")
ND <- df %>% filter(wetland == "ND")

#Filter out Channel and Forested Flat
Wetlands <- df %>% filter(wetland %in% c("QB","TB","DB","ND"))
WetSynoptic <- synoptic %>% filter(Wetland_ID %in% c("QB","TB","DB","ND")) 
#Filter out Leaf Litter
WetlandsNoLL <- Wetlands %>% filter(Point != "5 LL")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Plots-----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2.1 Plots of EOC vs other variables---------------------------------------

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

##FI##
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

##SUVA254##
#SUVA by Horizon - All wetalnd sites, No LL
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

#SUVA by Point - All wetalnd sites, No LL
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
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  ggtitle("All Sites SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#SUVA boxplot by Horizon - All wetalnd sites, No LL
ggplot(WetlandsNoLL, aes(Generic_Horizon,SUVA254_L_mgm,fill=Generic_Horizon)) +
  geom_boxplot()+
  xlab("EOC (mg/L)") +
  ylab("SUVA254") + 
  ggtitle("All Sites SUVA254") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

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

##HIX##
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

##SSR##
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
  ggtitle("All Sites SSR") + 
  theme_bw() +
  theme(legend.text = element_text(size=16),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.title.x  = element_text(size=16),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#2.2 Compare months --------------------------------------------------

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

#2.3 Cory and McKnight PARAFAC Results--------------------------------

#Cory and McKnight Component Loadings across all sites
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

##C2##
#EOC
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=EOC_mgC_L,y=C2_Q2,col=Generic_Horizon)) +
  xlab("EOC (mg/L)") +
  ylab("%C2")+
  ggtitle("%C2 vs EOC")
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
  ggtitle("%C4 vs EOC")
#SUVA
ggplot(data=WetlandsNoLL) +
  geom_point(aes(x=SUVA254_L_mgm,y=C4_HQ,col=Generic_Horizon)) +
  xlab("SUVA254 (L/mg-m)") +
  ylab("%C4")+
  ggtitle("%C4 vs SUVA254")

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

#2.4 ESOM and Synoptic Data Together-----------------------------------

#EOC Boxplot along Transect Points
ggplot(WetSynoptic,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("EOC by Horizon and Transect Spot")

#EOC by Month
ggplot(WetSynoptic,aes(EOC_mgC_L,Number_Name,fill=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("EOC by Horizon and Transect Spot")+
  facet_wrap(~Month)

#FI by Horizon - All wetland sites
ggplot(WetSynoptic, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  #stat_ellipse()+
  xlab("EOC (mg/L)") +
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

#FI vs Location
ggplot(WetSynoptic,aes(x=Point,y=FI,fill=Generic_Horizon)) + 
  geom_boxplot() +
  xlab("Transect Point") +
  ylab("FI")+
  ggtitle("FI vs Horizon") +
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

#2.5 Other variables-----------------------------------

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

#%Clay
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
#3.0 Correlation Exploration ---------------------------------------------------
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
