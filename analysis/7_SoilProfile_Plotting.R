#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Soil Profile Plotting
#Coder: Katie Wardinski (wardinskik@vt.edu)
#Created: 5/26/2021
#Purpose: Plot soil profiles for each transect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

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
library(soilDB)
library(sharpshootR)
library(RColorBrewer)
library(reshape)
library(latticeExtra)
library(lattice)
library(Hmisc)
library(MASS)
library(rgdal)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Basic profile plots--------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get soil color
ND$soil_color <- with(ND,munsell2rgb(ND$Hue,ND$Value,ND$Chroma))
QB$soil_color <- with(QB,munsell2rgb(QB$Hue,QB$Value,QB$Chroma))
TB$soil_color <- with(TB,munsell2rgb(TB$Hue,TB$Value,TB$Chroma))
DB$soil_color <- with(DB,munsell2rgb(DB$Hue,DB$Value,DB$Chroma))

write.csv(ND, "data/ND_SoilColor.csv")
write.csv(QB, "data/QB_SoilColor.csv")
write.csv(TB, "data/TB_SoilColor.csv")
write.csv(DB, "data/DB_SoilColor.csv")

#Online examples
#https://ncss-tech.github.io/AQP/aqp/aqp-intro.html
#http://rstudio-pubs-static.s3.amazonaws.com/63186_f5856ca97f7e4956bbf93e2eaa39412f.html

str(ND)
# id = column containing profile ID
# top = column containing horizon upper boundaries
# bottom = column containing horizon lower boundaries 
depths(ND) <- station ~ Horizon_Start_cm + Horizon_End_cm
#horizon designation column
hzdesgnname(ND) <- 'Horizon'
class(ND)
print(ND)
par(mar=c(5,5,1,3))
plot(ND,name='Horizon',color='soil_color',
     id.style='top',cex.names=1,
     cex.id = 1, axis.line.offset = -2,
     max.depth=70)
axis(1,at=1:length(ND),label=c(10.8,16.3,22.7,30.3),cex.axis=1)
mtext(1,line=2.25,text='Distance from Wetland Center Well (m)',cex=1)
title(main="ND",line=-3)


#QB
str(QB)
depths(QB) <- station ~ Horizon_Start_cm + Horizon_End_cm
plot(QB,name='Horizon',color='soil_color',
     id.style='top',cex.names=1,
     cex.id = 1, axis.line.offset = -2,
     max.depth=70)
axis(1,at=1:length(QB),label=c(19.0,25.8,35.3,44.9),cex.axis=1)
mtext(1,line=2.25,text='Distance from Wetland Center Well (m)',cex=1)
title(main="QB",line=-3)

#DB
str(DB)
depths(DB) <- station ~ Horizon_Start_cm + Horizon_End_cm
plot(DB,name='Horizon',color='soil_color',
     id.style='top',cex.names=1,
     cex.id = 1, axis.line.offset = -2,
     max.depth=70)
axis(1,at=1:length(DB),label=c(8.8,13.4,19.2,25.8),cex.axis=1)
mtext(1,line=2.25,text='Distance from Wetland Center Well (m)',cex=1)
title(main="DB",line=-3)

#TB
str(TB)
depths(TB) <- station ~ Horizon_Start_cm + Horizon_End_cm
plot(TB,name='Horizon',color='soil_color',
     id.style='top',cex.names=1,
     cex.id = 1, axis.line.offset = -2,
     max.depth=70)
axis(1,at=1:length(TB),label=c(32.6,36.3,40.7,47.3),cex.axis=1)
mtext(1,line=2.25,text='Distance from Wetland Center Well (m)',cex=1)
title(main="TB",line=-3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Plot profiles along transect ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#https://cran.r-project.org/web/packages/sharpshootR/sharpshootR.pdf   (pg 49)

#trying to plot along transect
#need datum, UTM zone?
coordinates(ND) <- ~ station_distance + ground_elevation
proj4string(ND) <- '+proj=longlat +datum=NAD83'
crs.utm <- CRS('+proj=utm +zone=18S +datum=NAD83')
plotTransect(ND,grad.var.name = 'station_distance',crs=crs.utm)


#an example
data("mineralKing",package="soilDB")
par(mar=c(1,1,2,1))
groupedProfilePlot(mineralKing, groups='taxonname', print.id=FALSE)
coordinates(mineralKing) <- ~ x_std + y_std
proj4string(mineralKing) <- '+proj=longlat +datum=NAD83'
crs.utm <- CRS('+proj=utm +zone=11 +datum=NAD83')
par(mar=c(4.5,4,4,1))
plotTransect(mineralKing, grad.var.name='elev_field', crs=crs.utm,
             grad.axis.title='Elevation (m)', label='pedon_id', name='hzname')

