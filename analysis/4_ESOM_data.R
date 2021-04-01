#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: ESOM Data
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Date: 4/1/2021
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

#Read data
df<-read_csv("data/R_Extraction_Results_All.csv")

#Separate months
JanMar <- df %>% filter(Month %in% c('2020-01','2020-03'))
Sept <- df %>% filter(Month == '2020-09')
Nov <- df %>% filter(Month == "2020-11")

#Separate wetland sites
QB <- df %>% filter(Wetland_ID == "QB")
TB <- df %>% filter(Wetland_ID == "TB")
DB <- df %>% filter(Wetland_ID == "DB")
ND <- df %>% filter(Wetland_ID == "ND")

#Filter out Channel and Forested Flat
Wetlands <- df %>% filter(Wetland_ID %in% c("QB","TB","DB","ND"))
#Filter out Leaf Litter
WetlandsNoLL <- Wetlands %>% filter(Point != "LL")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Plots-----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#EOC Boxplot along Transect Points
ggplot(Wetlands,aes(EOC_mgC_L,Generic_Name,col=Generic_Horizon))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("EOC by Horizon and Transect Spot")

#FI by Horizon - All wetland sites, No LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,FI,col=Generic_Horizon)) +
  geom_point(size=2.5) +
  #stat_ellipse()+
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

#SUVA by Horizon - All wetalnd sites, No LL
ggplot(WetlandsNoLL, aes(EOC_mgC_L,SUVA254_L_mgm,col=Generic_Horizon)) +
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

#QB EOC over the 3 sampling campaigns
ggplot(data=QB,aes(x=Generic_Horizon,y=EOC_mgC_L)) + 
  geom_boxplot()+
  theme_bw()+
  ylab("EOC (mg/L)") +
  xlab("Soil Horizon")+
  ggtitle("QB EOC vs Month") + 
  facet_wrap(~Month)

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
