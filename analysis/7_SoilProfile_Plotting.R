#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Soil Profile Plotting
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 5/26/2021
#Purpose: Plot soil profiles for each transect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load package for filtering
library(tidyverse)

#load ESOM data
soil <- read.csv("data//Soil_Profile_Plot.csv") 

#Specify wetland
QB <- soil %>% filter(wetland == "QB")
TB <- soil %>% filter(wetland == "TB")
DB <- soil %>% filter(wetland == "DB")
ND <- soil %>% filter(wetland == "ND")

#now load these soil profile specific packages
#note that these mask dplyr abilities so do all filtering before using
library(aqp)
#library(soilDB)
library(sharpshootR)
library(RColorBrewer)
library(reshape)
library(latticeExtra)
library(lattice)
library(Hmisc)
library(MASS)
library(rgdal)

#now work on plots
ND$soil_color <- with(ND,munsell2rgb(ND$Hue,ND$Value,ND$Chroma))
QB$soil_color <- with(QB,munsell2rgb(QB$Hue,QB$Value,QB$Chroma))
TB$soil_color <- with(TB,munsell2rgb(TB$Hue,TB$Value,TB$Chroma))
DB$soil_color <- with(DB,munsell2rgb(DB$Hue,DB$Value,DB$Chroma))

#Follow example from https://ncss-tech.github.io/AQP/aqp/aqp-intro.html
str(ND)
# id = column containing profile ID
# top = column containing horizon upper boundaries
# bottom = column containing horizon lower boundaries 
depths(ND) <- station ~ Horizon_Start_cm + Horizon_End_cm
#horizon designation column
hzdesgnname(ND) <- 'Horizon'
class(ND)
print(ND)
plot(ND,name='Horizon',color='soil_color',id.style='side')
title(main="ND")

#trying to plot along transect
#need datum, UTM zone 
coordinates(ND) <- ~ station_distance + ground_elevation
proj4string(ND) <- '+proj=longlat +datum=NAD83'
plotTransect(ND,grad.var.name = 'station_distance',crs=CRS('+proj=utm +zone=18S +datum=NAD83'))

#QB
str(QB)
depths(QB) <- station ~ Horizon_Start_cm + Horizon_End_cm
#horizon designation column
plot(QB,name='Horizon',color='soil_color',id.style='side')
title(main="QB")

#DB
str(DB)
depths(DB) <- station ~ Horizon_Start_cm + Horizon_End_cm
#horizon designation column
plot(DB,name='Horizon',color='soil_color',id.style='side')
title(main="DB")

#TB
str(TB)
depths(TB) <- station ~ Horizon_Start_cm + Horizon_End_cm
#horizon designation column
plot(TB,name='Horizon',color='soil_color',id.style='side')
title(main="TB")
