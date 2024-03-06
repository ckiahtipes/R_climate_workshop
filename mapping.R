#Mapping tools script.

#We need to locate some global climate and physiographic datasets.

#worldclim...https://www.worldclim.org/
#srtm mission...

geodata_path("data/")

###Necessary Packages

library(raster)
library(maps)
library(grDevices)
library(sf)
library(rnaturalearth)
library(geodata)

#Testing

srtm <- elevation_3s(lon = -82, lat = 27)

#Use climate data to plot avg temp.

#Pull and extract spatial data.

tavg.files=list.files("worldclim/wc2.1_10m_tavg/",".tif",full.names=TRUE)
tavg=stack(tavg.files)

prec.files=list.files("worldclim/wc2.1_10m_prec/",".tif",full.names=TRUE)
prec=stack(prec.files)

month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

names(tavg)=month
names(prec)=month

#Set a color palette

tempcol=colorRampPalette(c("purple","blue","skyblue","green","lightgreen","yellow","orange","red","darkred"), alpha = FALSE) #This is a cool means of constructing gradient colors

#Basic plotting

plot(tavg, col = tempcol(100))

plot(tavg[[1]], col = tempcol(100))

#Limiting search area.

map_LAT = c(25, 30)
map_LON = c(-85, -80)

project.area=extent(map_LON[1],map_LON[2],map_LAT[1],map_LAT[2])
proj.prec=crop(prec,project.area)
proj.tavg=crop(tavg,project.area)
proj.isohyets=rasterToContour(proj.prec$Apr,maxpixels = 1e4, nlevels=20)

#Getting seasonal data

DJF.prec=sum(proj.prec$Dec,proj.prec$Jan,proj.prec$Feb)
MAM.prec=sum(proj.prec$Mar,proj.prec$Apr,proj.prec$May)
JJA.prec=sum(proj.prec$Jun,proj.prec$Jul,proj.prec$Aug)
SON.prec=sum(proj.prec$Sep,proj.prec$Oct,proj.prec$Nov)

seasonal=array(c(DJF.prec,MAM.prec,JJA.prec,SON.prec))

#Naming seasonal objects

names(seasonal)=c("DJF.prec","MAM.prec","JJA.prec","SON.prec")
seas_names=c("DJF","MAM","JJA","SON")

#Creating annual values

prec.annual=sum(proj.prec[[1:12]])
ann.iso=rasterToContour(prec.annual, nlevels = 20)

#Basic plotting

plot(0,0, 
     xlim = map_LON, 
     ylim= map_LAT, 
     pch=NA, 
     axes = FALSE, 
     ann = FALSE)

plot(prec.annual,col = tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE, legend = TRUE)

plot(ann.iso, add=TRUE, lty=1, lwd = 0.5)

axis(1, at = seq(-85,-80,1))
axis(2, at = seq(25, 30, 1))
title(main = "Florida Annual Precipitation via Worldclim")