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
depth <- read_csv("data//waterLevel_2020only.csv") 
depth<-depth %>% drop_na(y_n)
metrics<-read_csv("data//annual_metrics_2020_threshold.csv")

#Remove SC-A because it was inundaded and not samples
#depth<-depth %>% filter(!(station == 'KW-4U' & wetland == 'QB'))
#metrics<-metrics %>% filter(!(station == 'KW-4U' & wetland == 'QB'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Hydrologic Regime Plots-------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Hydrograph---------------------------------------------
df<-depth %>% 
  #Convert to cm
  #mutate(waterLevel= y_n*100) %>% 
  mutate(waterLevel= y_n) %>% 
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
Wetland<-df %>% filter(loc=='1') %>% drop_na(lwr)
Edge<-df %>% filter(loc=='2') %>% drop_na(lwr)
Transition<-df %>% filter(loc=='3') %>% drop_na(lwr)
Upland<-df %>% filter(loc=='4') %>% drop_na(lwr)

#Define ribbon tranparency
ribbon_alpha<-0.90

#Define colors
cols<-c(
  'Wetland' = '#045a8d', 
  'Edge' = '#2b8cbe', 
  'Transition' = '#74a9cf', 
  'Upland' = '#bdc9e1')

line_col<-"grey50"

#Start ggplot
hyd<-ggplot() + 
  geom_rect(
    aes(
      xmin = as.Date('2019-09-30'), 
      xmax = as.Date('2020-10-01'), 
      ymin = -0.5, 
      ymax = 0), 
      fill='grey80', 
      alpha = 0.9) +
   
  #Upland
  geom_ribbon(aes(ymin = Upland$lwr, 
                  ymax = Upland$upr, 
                  x = Upland$Timestamp, 
                  fill='Upland'),
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Upland$Timestamp, 
                y=Upland$mean), 
                col=line_col) +
  
  #Transition
  geom_ribbon(aes(ymin = Transition$lwr, 
                  ymax = Transition$upr, 
                  x = Transition$Timestamp, 
                  fill='Transition'),
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Transition$Timestamp, 
                y=Transition$mean), 
                col=line_col) +
  #Edge
  geom_ribbon(aes(ymin = Edge$lwr, 
                  ymax = Edge$upr, 
                  x = Edge$Timestamp, fill='Edge'), 
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Edge$Timestamp, 
                y=Edge$mean), 
                col=line_col) +
  #Wetland
  geom_ribbon(aes(ymin = Wetland$lwr, 
                  ymax = Wetland$upr, 
                  x = Wetland$Timestamp, 
                  fill='Wetland'),
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Wetland$Timestamp, 
                y=Wetland$mean), 
                col=line_col) +
  
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [m]") + 
  xlab("Date") + 
  ylim(-2,1)+
  #scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = c("bottom"), 
        legend.margin = margin(t=-0.5, unit="lines"),
        legend.text = element_text(size=12)) +
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
  summarise(mean = mean(y_n, na.rm=T), 
            upr    = (mean(y_n, na.rm=T) + sd(y_n, na.rm = T)/sqrt(n())),
            lwr    = (mean(y_n, na.rm=T) - sd(y_n, na.rm = T)/sqrt(n()))) %>% 
  #plot!
  ggplot() + 
  geom_hline(aes(yintercept=0), lty=2,lwd=1.25, col="grey30")+
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = mean), 
             pch=c(21,22,23,24),
             cex=6,
             col = 'grey30', 
             fill = c('#045a8d', '#2b8cbe','#74a9cf','#bdc9e1'), 
             alpha = 70) + 
  theme_bw() + 
  ylab("Mean Water Elevation [m]") + 
  xlab("Transect Point") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) 

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
             cex=6,
             col = 'grey30', 
             fill = c('#045a8d', '#2b8cbe','#74a9cf','#bdc9e1'),
             alpha = 70) + 
  theme_bw() +
  ylab("Mean Saturation Duration [Days]") + 
  xlab('Transect Point') + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) 

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
             cex=6,
             col = 'grey30', 
             fill = c('#045a8d', '#2b8cbe','#74a9cf','#bdc9e1'),
             alpha = 70) + 
  theme_bw() + 
  ylab("Mean Number of Saturation Events") + 
  xlab("Transect Point") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) 
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

#3.1.1 Bring in soil horiz elev to calculate mean depth 
elev <- read_csv("data//20210516_KW_SoilHorizElev.csv")
mean_elev <- elev %>% 
                group_by(station) %>% 
                summarize(O_mean_elev = mean(O_lower,na.rm=T),
                          A_mean_elev = mean(A_lower,na.rm=T),
                          B_mean_elev = mean(B_lower,na.rm=T))
    
KWW <- mean_elev %>% filter(station=='KW-1W')
KWE <- mean_elev %>% filter(station=='KW-2E')
KWT <- mean_elev %>% filter(station=='KW-3T')
KWU <- mean_elev %>% filter(station=='KW-4U')

#3.1.2 Set up water level data
df<-depth %>% 
  #Leave as m
  mutate(waterLevel= y_n) %>% 
  #select wetland well data
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
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWW$O_mean_elev, 
        ymax = 0), 
        fill='#3D2F21FF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWW$A_mean_elev, 
        ymax = KWW$O_mean_elev), 
    fill='#38302AFF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWW$B_mean_elev, 
        ymax = KWW$A_mean_elev), 
    fill='#686057FF') +

  #Wetland
  geom_ribbon(aes(ymin = Wetland$lwr, 
                  ymax = Wetland$upr, 
                  x = Wetland$Timestamp, 
                  fill='Wetland'),
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Wetland$Timestamp, 
                y=Wetland$mean), 
                col='black',
                size=1.25) +
  
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [m]") + 
  xlab("Date") +
  ggtitle("Wetland")+
  #scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none")
        #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = '1', tag_suffix = ".  ")

wet

#3.3 Edge -------------------------------------------
edge <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWE$O_mean_elev, 
        ymax = 0), 
    fill='#3D2F21FF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWE$A_mean_elev, 
        ymax = KWE$O_mean_elev), 
    fill='#38302AFF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWE$B_mean_elev, 
        ymax = KWE$A_mean_elev), 
    fill='#50473FFF') +
  
 
  #Edge
  geom_ribbon(aes(ymin = Edge$lwr, 
                  ymax = Edge$upr,
                  x = Edge$Timestamp, 
                  fill='Edge'), 
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Edge$Timestamp, 
                y=Edge$mean),
                col='black',
                size=1.25) +

  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [m]") + 
  xlab("Date") + 
  ggtitle("Edge")+
  #scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none")
        #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = '2', tag_suffix = ".  ")

edge


#3.4 Transition -------------------------------------------
trans <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWT$O_mean_elev, 
        ymax = 0), 
    fill='#3F2E23FF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWT$A_mean_elev, 
        ymax = KWT$O_mean_elev), 
    fill='#38302AFF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWT$B_mean_elev, 
        ymax = KWT$A_mean_elev), 
    fill='#947650FF') +
  
  #Transition
  geom_ribbon(aes(ymin = Transition$lwr, 
                  ymax = Transition$upr, 
                  x = Transition$Timestamp, 
                  fill='Transition'),
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Transition$Timestamp, 
                y=Transition$mean), 
                col='black',
                size=1.25) +
  
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [m]") + 
  xlab("Date") + 
  ggtitle("Transition")+
  #scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none")
  #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = 'C', tag_suffix = ".  ")

trans

#3.5 Upland ------------------------------------------------------

upland <-ggplot() + 
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWU$O_mean_elev, 
        ymax = 0), 
    fill='#3F2E23FF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWU$A_mean_elev, 
        ymax = KWU$O_mean_elev), 
    fill='#39302BFF') +
  
  geom_rect(
    aes(xmin = as.Date('2019-09-30'), 
        xmax = as.Date('2020-10-01'), 
        ymin = KWU$B_mean_elev, 
        ymax = KWU$A_mean_elev), 
    fill='#A99264FF') +
  
  #Upland
  geom_ribbon(aes(ymin = Upland$lwr, 
                  ymax = Upland$upr, 
                  x = Upland$Timestamp,fill='Upland'),
                  alpha=ribbon_alpha) +
  geom_line(aes(x=Upland$Timestamp,
                y=Upland$mean),
                col='black',
                size = 1.25) +
  
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  
  #Clip to water year
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
  
  #theme options
  theme_bw() + 
  ylab("Water Level [m]") + 
  xlab("Date") + 
  ggtitle("Upland")+
  #scale_x_date(date_labels = "%b") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none")
        #legend.margin = margin(t=-2, unit="lines")) +
  #plot_annotation(tag_levels = '4', tag_suffix = ".  ")

upland
    

#3.6 Plot together --------------------------------------------------

wet + edge + trans + upland
