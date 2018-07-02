###################Clean and add WHR###########################

# Script Setup ------------------------------------------------------------

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
library(ggplot2)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'preprocess.R'))
ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines2.RDS'))


TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')


# Merging WHR Layers ------------------------------------------------------

# ##Only need to run this once
# 
# streamTA <- spTransform(stream, TA)
# streamExt <- extent(streamTA)
# 
# whrpath <- file.path(datapath, 
#                      'frog_model/data/datasets/cwhr_4Megan/CWHRVg.gdb')
# 
# whrlayers <- ogrListLayers(whrpath)
# attributes(whrlayers) <- NULL
# 
# #ogrListLayers
# whrlist <- lapply(whrlayers, function(l) {
#     readOGR(dsn=whrpath, layer=l, stringsAsFactors = FALSE)
# }) 
# 
# whrlistTA <- lapply(whrlist, function(spdf) {
#     crop(spTransform(spdf, TA), streamExt)
#     })
# 
# # whrpaths <- paste('whr', whrnames, sep='/')
# # whrlist <- lapply(whrpaths, function(fn) shapefile(file.path(datapath, fn)))
# 
# whr <- do.call(bind, whrlistTA)
# 
# saveRDS(whr, file.path(datapath, 'processed/whr.RDS'))
# shapefile(whr, file.path(datapath, 'processed/shapefiles/whr.shp'))
# 

# WHR Processing ----------------------------------------------------------

#read in merged file
whr <- readRDS(file.path(datapath, 'processed/whr.RDS'))
whr <- spTransform(whr, crs(ras))


# WHR Type ----------------------------------------------------------------


## All 70 observations with NAs for whrtype have no rasi presence
## Merging with lifeform and covertype does not eliminate these nas

ras$whrtype <- overChr(ras, whr, 'WHRTYPE') 
brrIds <- which(ras$whrtype=='BBR')
ras$whrtype[brrIds] <- "BAR"


# WHR Density -------------------------------------------------------------

ras$whrdensity <- overChr(ras, whr, 'WHRDENSITY') #70 NAs 
recodeBlank(ras$whrdensity)


# #after recoding blanks as NAs there were 13 rows with rasi presence that had
# #WHR Density as NA
# 
# densNA <- which(is.na(ras$whrdensity))
# table(ras$rasi[densNA])



# WHR Size ----------------------------------------------------------------

ras$whrsize <- overChr(ras, whr,'WHRSIZE') #194 NAs 

ras$whrsize <- recodeBlank(ras$whrsize)

####there are 10 observations with rasi presence that have have WHRsize as NA
# sizeNA <- which(is.na(ras$whrsize))
# table(ras$rasi[sizeNA])
# 
# whrNA <- union(sizeNA, densNA)
# table(ras$rasi[whrNA])


# Life Form ---------------------------------------------------------------

ras$lifeform <- overChr(ras, whr,'WHRLIFEFORM') # 870 NAs 
ras$lifeform <- recodeBlank(ras$lifeform)



# Cover Type --------------------------------------------------------------

ras$covertype <- overChr(ras, whr,'COVERTYPE') #478 NAs 
ras$covertype <- recodeBlank(ras$covertype)



# Save File ---------------------------------------------------------------

saveRDS(ras, file.path(datapath, 'processed/RasiStreamLines3.RDS'))


