#Coders against the Climate Crisis! 

#We need to locate some global climate and physiographic datasets.

#worldclim...
#srtm mission...


###Necessary Packages

library(raster)
library(maps)
library(grDevices)
library(sf)
library(rnaturalearth)

#Need to define lats/longs of some locations to plot and extract data.

#Pull and extract spatial data.

tavg.files=list.files("worldclim/wc2.1_30s_tavg/",".tif",full.names=TRUE)
tavg=stack(tavg.files)

prec.files=list.files("worldclim/wc2.1_30s_prec/",".tif",full.names=TRUE)
prec=stack(prec.files)

month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

names(tavg)=month
names(prec)=month

#Get rivers, lakes, and ocean

# download if needed
if(!file.exists("ne_maps/ne_10m_lakes.cpg")){
  ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", 
              destdir = "ne_maps/", load = FALSE) # major rivers
  
  ne_download(scale = 10, type = "lakes", category = "physical", 
              destdir = "ne_maps/", load = FALSE) # major lakes
  
  ne_download(scale = 10, type = "ocean", category = "physical",
              destdir = "ne_maps/", load = FALSE) # ocean
  
}

rivers <- ne_load(scale = 10, type = "rivers_lake_centerlines", destdir = "ne_maps", returnclass = "sf")
lakes <- ne_load(scale = 10, type = "lakes", destdir = "ne_maps", returnclass = "sf")
ocean <- ne_load(scale = 10, type = "ocean", destdir = "ne_maps", returnclass = "sf")

###Extracts the data based on lat and long.
#Making average temp data frame

tavg.data=extract(tavg,all_locations)
tavg.data=as.data.frame(tavg.data)
row.names(tavg.data)=row.names(all_locations)
#
##Making precip data frame
#
prec.data=extract(prec,all_locations)
prec.data=as.data.frame(prec.data)
row.names(prec.data)=row.names(all_locations)

##Get elevations
elev.data <- extract(region_SRTM, all_locations)
elev.data <- as.data.frame(elev.data)
row.names(elev.data) <- row.names(all_locations)


