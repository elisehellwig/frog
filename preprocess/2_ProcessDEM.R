###################DEM Process#####################
#this file imports some DEM (Digital Elevation Model) tiles from USGS, merges and crops them and then
#calculates some variables of interest (ex. slope, aspect etc.)


#loading required packages
library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)


#saving the path where data is stored as a variable for later use
datapath <- '/Users/echellwig/Research/frogData/data/'
funpath <- '/Users/echellwig/Research/frog/functions/'

#importing functions for preprocessing of data
source(file.path(funpath, 'preprocess.R'))

##########################################################
###########Creating DEM###################################


#creating file names of the DEMS
grids <- paste0('n', rep(c(40,41), each=3), 'w', rep(120:122, 2))
fnames <- paste0('float', grids, '_13.flt')

#modifying the third one because USGS can't seem to be consistent in their
#file structure
grids[3] <- 'USGS_NED_13_n40w122_GridFloat'
fnames[3] <- 'usgs_ned_13_n40w122_gridfloat.flt'


#loading the DEM tiles into R in list form 
demlist <- lapply(seq_along(fnames), function(i) {
    fn <- file.path(datapath,'DEM', grids[i], fnames[i])
    raster(fn)
})

#merging the dem tiles together into one object
dem <- do.call(merge, demlist)

#Creating a bounding box to use to crop the DEM object
streamExt <- extent(-121.7, -120, 39.2, 40.4)

#croping the object so we only have the DEM where we have streams
streamDEM <- crop(dem, streamExt)

#saving the DEM as a GRD
writeRaster(streamDEM, file.path(datapath, 'processed/localDEM.grd'))

########################Processing DEM##########################
streamDEM <- raster(file.path(datapath, 'processed/localDEM.grd'))

#calcualting aspect
streamDEM$aspect <- terrain(streamDEM$layer, 'aspect', unit = 'degrees')  


#Setting up the key for recoding aspect to a character
cardseq <-c(0, rep(seq(22.5, 337.5, by=45), each=2), 360)
mcard1 <- matrix(cardseq, ncol=2, byrow=TRUE)
dfcard <- data.frame(mcard1)
dfcard$id <- 1:9
dfcard$string <- c('N','NE','E','SE','S','SW','W','NW','N')
names(dfcard)[1:2] <- c('min','max')

aspect <- getValues(streamDEM$aspect)
aspectid <- recodeRange(aspect, dfcard, string=TRUE, digits=0) 

#Convert to cardinal directions N1=(0-45, 315-360), E2=(45-135), S3=(135-225), 
    #W4=(225-315)

streamDEM$slope <- terrain(streamDEM$layer, 'slope')

writeRaster(streamDEM, file.path(datapath,'processed/streamGeography.grd'),
            overwrite=TRUE)


