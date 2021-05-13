#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Plot Data!
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 2/28/2021
#Purpose: Just an initial script to show off the data!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next steps: 
#    ) Figure out level issue
#    ) Standardize output
#    ) Complete water level analysis along transect
#    ) Add standardized QAQC data
#    ) Redo 2018-2019 data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup Workspace-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace 
remove(list=ls())

#Gather libraries of interest
library(dygraphs)
library(xts)
library(lubridate)
library(stringr)
library(readxl)
library(tidyverse)

#load repo functions
source('R//dygraph_ts_fun.R')

#read waterLevel data
dt<-read_csv("data//waterLevel.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Plots!!! Plots!!! Plots!!!--------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#list sites
dt %>% select(Site_Name) %>% unique()

#Plot QB
dt %>% 
  filter(Site_Name=='QB Wetland Well Shallow') %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  dygraph_ts_fun(.)

#Plot TB
dt %>% 
  filter(Site_Name=='TB Wetland Well Shallow') %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  dygraph_ts_fun(.)

#Plot DB
dt %>% 
  filter(Site_Name=='DB Wetland Well Shallow') %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  dygraph_ts_fun(.)


#Plot NB
dt %>% 
  filter(Site_Name=='ND Wetland Well Shallow') %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  dygraph_ts_fun(.)

#test
