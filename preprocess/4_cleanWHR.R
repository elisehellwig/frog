###################Clean and add WHR###########################

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
library(ehelpr)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'preprocess.R'))
stream <- readRDS(file.path(datapath, 'processed/RasiStreamLines.RDS'))


TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')

##################################################################
##################################################################
streamTA <- spTransform(stream, TA)
streamExt <- extent(streamTA)

whrpath <- file.path(datapath, 
                     'frog_model/data/datasets/cwhr_4Megan/CWHRVg.gdb')

whrlayers <- ogrListLayers(whrpath)
attributes(whrlayers) <- NULL

#ogrListLayers
whrlist <- lapply(whrlayers, function(l) {
    readOGR(dsn=whrpath, layer=l, stringsAsFactors = FALSE)
}) 

whrlistTA <- lapply(whrlist, function(spdf) {
    crop(spTransform(spdf, TA), streamExt)
    })

# whrpaths <- paste('whr', whrnames, sep='/')
# whrlist <- lapply(whrpaths, function(fn) shapefile(file.path(datapath, fn)))

whr <- do.call(bind, whrlistTA)

saveRDS(whr, file.path(datapath, 'processed/whr.RDS'))
shapefile(whr, file.path(datapath, 'processed/shapefiles/whr.shp'))

##################################################################


whrbuff <- gBuffer(whr, byid=TRUE, width=0)

habitattype <- overChr(streamTA, whr, 'WHRTYPE')
naIDs <- which(is.na(habitattype))

missing <- streamTA[naIDs, ]
missingWHR <- crop(whrbuff, extent(missing))
streamTAmissing <- crop(streamTA, extent(missing))
whrplot <- aggregate(missingWHR)


plot(missingWHR, col='lightblue', lwd=0.01, border='lightblue', 
     main='Extent of Wildlife Habitat Relationship (WHR) Layers \n and Stream Locations, Red = ouside WHR extent')
plot(streamTAmissing, col='navy', add=TRUE)
plot(missing, col='red3', add=TRUE)


#extracting values to see now many NAs we have

countNA(habitat)


