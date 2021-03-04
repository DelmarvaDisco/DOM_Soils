#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Depth to water table calclulation
#Coder: C. Nate Jones (cnjones7@ua.edu)
#Date: 3/5/2020
#Purpose: Estimate depth to water table at wetland sampling locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1.0 Setup workspace------------------------------------------------------------
#Clear workspace
remove(list=ls())

#download relevant libraries
library(lubridate)
library(tidyverse)

#download data
df<-read_csv("data/waterLevel.csv")
survey<-read_csv("data/xs_survey.csv")

#3.0 Estimate Deth to Water Table-----------------------------------------------
#3.1 Depth to water table function----------------------------------------------
depth_fun<-function(wetland_code){
  
  #Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create wide df of wetland and upland water level data
  temp<-df %>%
    #Select time series of interest
    filter(site == paste0(wetland_code, ' Wetland Well Shallow') |
             site == paste0(wetland_code, ' Upland Well 1')) %>%
    #Create wide dataframe
    spread(site, -day) %>% 
    rename(y_sw = paste0(wetland_code, ' Wetland Well Shallow'),
           y_gw = paste0(wetland_code, ' Upland Well 1'))
  
  #Cross section data
  xs<-survey %>% filter(wetland == wetland_code) %>% select(distance, elevation)
  
  #Define points of interest
  sample<- survey %>% 
    filter(wetland == wetland_code) %>% 
    filter(str_detect(station,"AIK") |
             str_detect(station, 'SC')) %>% 
    select(station, distance, elevation)
  
  #Define Wells [for now, assume wetladn well x = 0]
  well<- survey %>% 
    filter(wetland == wetland_code) %>% 
    filter(str_detect(station,"Upland Well")) %>% 
    select(distance, elevation)
  
  #Estimate depth to water table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Start function to estimate depth to water table for each sampling point
  inner_fun<-function(n){
    
    #Define sample
    sample<-sample[n,]
    
    #Create interpolation to estimate inundation extent
    inundation_fun<-approxfun(xs$elevation, xs$distance, yleft = 0)
    
    #Define horizontal distances 
    temp <- temp %>%
      mutate(
        #Distance from well to edge of water
        x_sw = inundation_fun(y_sw), 
        #distance from wetland to upland well
        x_gw = well$distance,
        #distance from wetland well to sampling location
        x_n = sample$distance)
    
    #Define water level at sample location
    temp <- temp %>%
      mutate(y_n = 
               #If sampling the sampling location is beyond upland well
               if_else(x_n>=x_gw, 
                       y_gw,
                       #If sampling location is inundated
                       if_else(x_n<=x_sw, 
                               y_sw, 
                               #If sampling location between inunation and upland well
                               (y_gw-y_sw)*(x_n-x_sw)/(x_gw-x_sw) + y_sw)))
    
    #Estimate depth
    temp<-temp %>% mutate(d_n = sample$elevation - y_n, 
                          station = sample$station)
    
    #Organize and export
    temp %>% select(day, d_n, station)
  }
  
  #Apply function to all sites                   
  output<-lapply(seq(1, nrow(sample)),inner_fun) %>% bind_rows(.)
  
  #Add wetland code
  output$wetland<-wetland_code
  
  #Export output
  output
}

#3.2 Apply Functions------------------------------------------------------------
sites<-c("BB", "DB", "DK", "GN", "ND","QB", "TB")
df<-lapply(sites, depth_fun) %>% bind_rows(.)

#3.3 Export file----------------------------------------------------------------
write.csv(df, "data/DepthToWaterTable.csv")