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

whrpath <- file.path(datapath, 
                     'frog_model/data/datasets/cwhr_4Megan/CWHRVg.gdb')

whrlayers <- ogrListLayers(whrpath)
attributes(whrlayers) <- NULL

#ogrListLayers
whrlist <- lapply(whrlayers, function(l) {
    readOGR(dsn=whrpath, layer=l)
}) 

whrlistTA <- lapply()

# whrpaths <- paste('whr', whrnames, sep='/')
# whrlist <- lapply(whrpaths, function(fn) shapefile(file.path(datapath, fn)))

whrall <- do.call(bind, whrlist)
whr <- aggregate(whrall)

stream <- readRDS(file.path(datapath, 'processed/RasiStreamLines.RDS'))

(NAs <- length(which(is.na(stream$whrtype))))


TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ')


streamTA <- spTransform(stream, TA) 
cwhrTA <- spTransform(cwhr, TA)


cwhragg <- aggregate(cwhrTA)
cwhrbuff <- gBuffer(cwhragg, byid=TRUE, width=0)


outsideStreams <- gDifference(streamTA, cwhrbuff, byid=TRUE)

plot(cwhragg, col='lightblue', main='Extent of Wildlife Habitat Relationship (WHR) Layers \n and Stream Locations, Red = ouside WHR extent')
plot(streamTA, add=TRUE, col='navy')
plot(outsideStreams, add=TRUE, col='red3')


#extracting values to see now many NAs we have

habitat <- overChr(streamTA, whrTA, 'WHRTYPE')

countNA(habitat)


