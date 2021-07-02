#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Plots
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 6/23/2020
#Purpose: Plot water level data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Workspace---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#load appropriate packages
library(patchwork)
library(lubridate)
library(tidyverse)
library(dplyr)

#load data
#2017-2020
depth<-read_csv("data//waterLevel_at_sampling_location.csv") 
depth<-depth %>% drop_na(y_n)
metrics<-read_csv("data//annual_metrics_2017-2020_threshold.csv")
#2020 water year only
#depth <- read_csv("data//waterLevel_2020only.csv") 
#metrics<-read_csv("data//annual_metrics_2020_threshold.csv")

#Remove SC-A because it was inundaded and not samples
#depth<-depth %>% filter(!(station == 'KW-4U' & wetland == 'QB'))
#metrics<-metrics %>% filter(!(station == 'KW-4U' & wetland == 'QB'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Hydrologic Regime Plots-------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Hydrograph---------------------------------------------
df<-depth %>% 
  #Convert to cm
  mutate(waterLevel= y_n*100) %>% 
  #seleect wetland well data
  filter(str_detect(station, 'KW')) %>% 
  #create Hydrologic Zone col
  mutate(loc = substr(station, 4,4)) %>% 
  #Group by date & loc
  group_by(Timestamp, loc) %>% 
  #Summarise water level data
  summarise(
    mean = mean(waterLevel, na.rm = T),
    lwr    = mean - sd(waterLevel, na.rm = T)/sqrt(n()), 
    upr    = mean + sd(waterLevel, na.rm = T)/sqrt(n()))

#Subset by Hydrologic Zone
Wetland<-df %>% filter(loc=='1')
Edge<-df %>% filter(loc=='2')
Transition<-df %>% filter(loc=='3')
Upland<-df %>% filter(loc=='4')
#e<-df %>% filter(loc=="E")

#Define ribbon tranparency
ribbon_alpha<-0.90

#Define colors
cols<-c(
  'Wetland' = '#045a8d', 
  'Edge' = '#2b8cbe', 
  'Transition' = '#74a9cf', 
  'Upland' = '#bdc9e1') #,
  #'E' = '#f1eef6')

line_col<-"grey50"

#Start ggplot
hyd<-ggplot() + 
  geom_rect(
    aes(
      xmin = as.Date('2017-09-30'), 
      xmax = as.Date('2020-10-18'), 
      ymin = -50, 
      ymax = 0), 
    fill='grey70', alpha = 0.9) +
 
  #E
  #geom_ribbon(aes(ymin = e$lwr, ymax = e$upr, x = e$day, fill='E'), 
  #col='grey90', lwd=0.25) +
  #geom_line(aes(x=e$day, y=e$mean), 
  #col=line_col) +
   
  #Upland
  geom_ribbon(aes(ymin = Upland$lwr, ymax = Upland$upr, x = Upland$Timestamp, fill='Upland'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=Upland$Timestamp, y=Upland$mean), 
            col=line_col) +
  
  #Transition
  geom_ribbon(aes(ymin = Transition$lwr, ymax = Transition$upr, x = Transition$Timestamp, fill='Transition'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=Transition$Timestamp, y=Transition$mean), 
            col=line_col) +
  #Edge
  geom_ribbon(aes(ymin = Edge$lwr, ymax = Edge$upr, x = Edge$Timestamp, fill='Edge'), 
              alpha=ribbon_alpha) +
  geom_line(aes(x=Edge$Timestamp, y=Edge$mean), 
            col=line_col) +
  #Wetland
  geom_ribbon(aes(ymin = Wetland$lwr, ymax = Wetland$upr, x = Wetland$Timestamp, fill='Wetland'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=Wetland$Timestamp, y=Wetland$mean), 
            col=line_col) +
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  #Clip to water year
  coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  #coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  #theme options
  theme_bw() + 
  ylab("Water Level [cm]") + 
  xlab(NULL) + 
  scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = c("bottom"), 
        legend.margin = margin(t=-2, unit="lines")) +
  plot_annotation(tag_levels = 'A', tag_suffix = ".  ")

hyd

#2.2 Water Level Plot---------------------------------------
dep<-depth %>%
  #Convert depth-to-water-table to water level (ignore so I keep water level)
  #mutate(d_n = -1*y_n) %>% 
  #Select SC stations
  filter(str_detect(station, 'KW')) %>% 
  #Summarise duration by transect and station
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(y_n = mean(y_n, na.rm=T), na.rm=T) %>% 
  #summarise(d_n = mean(d_n, na.rm=T), na.rm=T) %>% 
  #Summarise by transect
  group_by(transect) %>% 
  summarise(mean = mean(y_n, na.rm=T)*100, 
            upr    = (mean(y_n, na.rm=T) + sd(y_n, na.rm = T)/sqrt(n()))*100,
            lwr    = (mean(y_n, na.rm=T) - sd(y_n, na.rm = T)/sqrt(n()))*100) %>% 
  #plot!
  ggplot() + 
  geom_hline(aes(yintercept=0), lty=2,lwd=1.25, col="grey30")+
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = mean), 
             pch=c(21,22,23,24),
             cex=4,
             col = 'grey30', 
             fill = c('#e41a1c', '#377eb8','#4daf4a','#984ea3'), 
             alpha = 70) + 
  theme_bw() + 
  ylab("Water Elevation [cm]") + 
  xlab("Hydrologic Zone") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

dep
#2.3 Duration ----------------------------------------------
#Duration plot
dur<-metrics %>% 
  #Select KW stations
  filter(str_detect(station, 'KW')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(dur_day = mean(dur_day, na.rm=T), na.rm=T) %>% 
  group_by(transect) %>% 
  summarise(mean = mean(dur_day, na.rm=T), 
            upr    = mean(dur_day, na.rm=T) + sd(dur_day, na.rm=T)/sqrt(n()),
            lwr    = mean(dur_day, na.rm=T) - sd(dur_day, na.rm=T)/sqrt(n())) %>% 
  #plot!
  ggplot() + 
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = mean), 
             pch=c(21,22,23,24),
             cex=4,
             col = 'grey30', 
             fill = c('#e41a1c', '#377eb8','#4daf4a','#984ea3'),
             alpha = 70) + 
  theme_bw() +
  ylab("Saturation Duration [Days]") + 
  xlab('Hydrologic Zone') + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

dur

#2.4 Frequency----------------------------------------------
freq<-metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'KW')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(n_events = mean(n_events, na.rm=T), na.rm=T) %>% 
  group_by(transect) %>% 
  summarise(mean = mean(n_events, na.rm=T), 
            upr    = mean(n_events, na.rm=T)+sd(n_events, na.rm=T)/sqrt(n()),
            lwr    = mean(n_events, na.rm=T)-sd(n_events, na.rm=T)/sqrt(n())) %>% 
  #plot!
  ggplot() + 
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = mean), 
             pch=c(21,22,23,24),
             cex=4,
             col = 'grey30', 
             fill = c('#e41a1c', '#377eb8','#4daf4a','#984ea3'),
             alpha = 70) + 
  theme_bw() + 
  ylab("Saturation Frequency [Events]") + 
  xlab("Hydrologic Zone") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 
freq

#2.5 Print plot---------------------------------------------
tiff("docs/hydro_regime.tif", res=285, width = 7, height = 6, units = 'in')
hyd + dep + dur + freq + plot_layout(ncol=2)
dev.off()

pdf("docs/hydro_regime.pdf", width = 7, height = 6)
hyd + dep + dur + freq + plot_layout(ncol=2)
dev.off()

hyd + dep + dur + freq

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Hydrologic Regime Plots by transect point --------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#3.1.1 Bring in soil horiz elev to calculate mean depth (convert to cm)
elev <- read_csv("data//20210516_KW_SoilHorizElev.csv")
mean_elev <- elev %>% 
                group_by(station) %>% 
                summarize(O_mean_elev = mean(O_lower,na.rm=T)*100,
                          A_mean_elev = mean(A_lower,na.rm=T)*100,
                          B_mean_elev = mean(B_lower,na.rm=T)*100)
    
KWW <- mean_elev %>% filter(station=='KW-1W')
KWE <- mean_elev %>% filter(station=='KW-2E')
KWT <- mean_elev %>% filter(station=='KW-3T')
KWU <- mean_elev %>% filter(station=='KW-4U')

#3.1.2 Set up water level data
df<-depth %>% 
  #Convert to cm
  mutate(waterLevel= y_n*100) %>% 
  #seleect wetland well data
  filter(str_detect(station, 'KW')) %>% 
  #create Hydrologic Zone col
  mutate(loc = substr(station, 4,4)) %>% 
  #Group by date & loc
  group_by(Timestamp, loc) %>% 
  #Summarise water level data
  summarise(
    mean = mean(waterLevel, na.rm = T),
    lwr    = mean - sd(waterLevel, na.rm = T)/sqrt(n()), 
    upr    = mean + sd(waterLevel, na.rm = T)/sqrt(n()))

#Subset by Hydrologic Zone
Wetland<-df %>% filter(loc=='1')
Edge<-df %>% filter(loc=='2')
Transition<-df %>% filter(loc=='3')
Upland<-df %>% filter(loc=='4')
#e<-df %>% filter(loc=="E")

#Define ribbon tranparency
ribbon_alpha<-0.90

#Define colors
cols<-c('Wetland' = '#045a8d', 
        'Edge' = '#2b8cbe', 
        'Transition' = '#74a9cf', 
        'Upland' = '#bdc9e1')

line_col<-"grey50"

#3.2 Wetland ---------------------------------------------

#Start ggplot
wet <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWW$O_mean_elev, 
        ymax = 0), 
        fill='tan4') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWW$A_mean_elev, 
        ymax = KWW$O_mean_elev), 
    fill='tan3') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWW$B_mean_elev, 
        ymax = KWW$A_mean_elev), 
    fill='tan2') +

  #Wetland
  geom_ribbon(aes(ymin = Wetland$lwr, ymax = Wetland$upr, x = Wetland$Timestamp, fill='Wetland'),
              alpha=ribbon_alpha,col='#045a8d') +
  geom_line(aes(x=Wetland$Timestamp, y=Wetland$mean), col='black') +
  
  #Legend/color
  #scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [cm]") + 
  xlab(NULL) +
  ggtitle("Wetland")+
  scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")
        #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = '1', tag_suffix = ".  ")

wet

#3.3 Edge -------------------------------------------
edge <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWE$O_mean_elev, 
        ymax = 0), 
    fill='tan4') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWE$A_mean_elev, 
        ymax = KWE$O_mean_elev), 
    fill='tan3') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWE$B_mean_elev, 
        ymax = KWE$A_mean_elev), 
    fill='tan2') +
  
 
  #Edge
  geom_ribbon(aes(ymin = Edge$lwr, ymax = Edge$upr, x = Edge$Timestamp, fill='Edge'), 
              alpha=ribbon_alpha,col='#045a8d') +
  geom_line(aes(x=Edge$Timestamp, y=Edge$mean),col='grey4') +

  #Legend/color
  #scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [cm]") + 
  xlab(NULL) + 
  ggtitle("Edge")+
  scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")
        #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = '2', tag_suffix = ".  ")

edge


#3.4 Transition -------------------------------------------
trans <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWT$O_mean_elev, 
        ymax = 0), 
    fill='tan4') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWT$A_mean_elev, 
        ymax = KWT$O_mean_elev), 
    fill='tan3') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWT$B_mean_elev, 
        ymax = KWT$A_mean_elev), 
    fill='tan2') +
  
  #Transition
  geom_ribbon(aes(ymin = Transition$lwr, ymax = Transition$upr, x = Transition$Timestamp, fill='Transition'),
              alpha=ribbon_alpha,col='#045a8d') +
  geom_line(aes(x=Transition$Timestamp, y=Transition$mean), col='grey4') +
  
  #Legend/color
  #scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [cm]") + 
  xlab(NULL) + 
  ggtitle("Transition")+
  scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")
  #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = 'C', tag_suffix = ".  ")

trans

#3.5 Upland ------------------------------------------------------

upland <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWU$O_mean_elev, 
        ymax = 0), 
    fill='tan4') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWU$A_mean_elev, 
        ymax = KWU$O_mean_elev), 
    fill='tan3') +
  
  geom_rect(
    aes(xmin = as.Date('2017-09-30'), 
        xmax = as.Date('2020-10-18'), 
        ymin = KWU$B_mean_elev, 
        ymax = KWU$A_mean_elev), 
    fill='tan2') +
  
  #Upland
  geom_ribbon(aes(ymin = Upland$lwr, ymax = Upland$upr, x = Upland$Timestamp,fill='Upland'),
              alpha=ribbon_alpha,col='#045a8d') +
  geom_line(aes(x=Upland$Timestamp, y=Upland$mean), col='grey4') +
  
  #Legend/color
  #scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [cm]") + 
  xlab(NULL) + 
  ggtitle("Upland")+
  scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")
        #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = '4', tag_suffix = ".  ")

upland
    

#3.6 Plot together --------------------------------------------------

wet + edge + trans + upland
