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

#load data
#depth<-read_csv("data//waterLevel_at_sampling_location.csv") #all data
depth <- read_csv("data//2020wateryear.csv") #2020 water year
metrics<-read_csv("data//annual_metrics_2020.csv")

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
  #coord_cartesian(xlim=as.Date(c("2017-09-30", "2020-10-18"))) +
  coord_cartesian(xlim=as.Date(c("2019-09-30", "2020-10-01"))) +
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
  #Convert depth-to-water-table to water level
  mutate(d_n = -1*y_n) %>% 
  #Select SC stations
  filter(str_detect(station, 'KW')) %>% 
  #Summarise duration by transect and station
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(d_n = mean(d_n, na.rm=T), na.rm=T) %>% 
  #Summarise by transect
  group_by(transect) %>% 
  summarise(mean = mean(d_n, na.rm=T)*100, 
            upr    = (mean(d_n, na.rm=T) + sd(d_n, na.rm = T)/sqrt(n()))*100,
            lwr    = (mean(d_n, na.rm=T) - sd(d_n, na.rm = T)/sqrt(n()))*100) %>% 
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
  ylab("Depth to Water Table [cm]") + 
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


