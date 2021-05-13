#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Troubleshooting
#Coder: C. Nate Jones (cnjones7@ua.edu)
#Date: 5/13/2021
#Purpose: Troubleshoot data issues!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Setup Env ---------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Run waterLevel code
source('analysis/1_waterLevel_Calc.R')

#load extra libs
library(patchwork)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Viz ---------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quintesential Bay
QB<-df %>% 
  filter(wetland =="QB") %>% 
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
    geom_line(aes(y=`KW-1W`), col='blue') +
    #geom_line(aes(y=`KW-2E`)) +
    #geom_line(aes(y=`KW-3T`)) +
    geom_line(aes(y=`KW-4U`), col='brown') +
    ggtitle("QB Wetland") +
  theme_bw()
    
#Tiger B
TB<-df %>% 
  filter(wetland =="TB") %>% 
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='blue') +
  #geom_line(aes(y=`KW-2E`)) +
  #geom_line(aes(y=`KW-3T`)) +
  geom_line(aes(y=`KW-4U`), col='brown') +
  ggtitle("TB Wetland") +
  theme_bw()

#Dark Bay
DB<-df %>% 
  filter(wetland =="DB") %>% 
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='blue') +
  #geom_line(aes(y=`KW-2E`)) +
  #geom_line(aes(y=`KW-3T`)) +
  geom_line(aes(y=`KW-4U`), col='brown') +
  ggtitle("DB Wetland") +
  theme_bw()

#North Dogbone
ND<-df %>% 
  filter(wetland =="ND") %>% 
  pivot_wider(names_from = station, values_from = y_n) %>% 
  ggplot(aes(x=Timestamp)) + 
  geom_line(aes(y=`KW-1W`), col='blue') +
  #geom_line(aes(y=`KW-2E`)) +
  #geom_line(aes(y=`KW-3T`)) +
  geom_line(aes(y=`KW-4U`), col='brown') +
  ggtitle("ND Wetland") +
  theme_bw()

#plot
QB + TB + DB + ND 
