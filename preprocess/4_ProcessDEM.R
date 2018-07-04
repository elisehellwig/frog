###################DEM Process#####################
#This only needs to be run once, not every time we get new data

#this file imports some DEM (Digital Elevation Model) tiles from USGS, merges 
#and crops them and then calculates some variables (aspect, slope etc.)



# Setup -------------------------------------------------------------------


#loading required packages
library(raster)
library(rgdal)
library(rgeos)


#saving the path where data is stored as a variable for later use
datapath <- '/Users/echellwig/Research/frogData/data/'
funpath <- '/Users/echellwig/Research/frog/functions/'

#importing functions for preprocessing of data
source(file.path(funpath, 'preprocess.R'))

ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines3.RDS'))


# Load DEM --------------------------------------------------------------

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


# Process DEM -------------------------------------------------------------



streamDEM <- raster(file.path(datapath, 'processed/localDEM.grd'))

#calcualting aspect
streamDEM$aspect <- terrain(streamDEM$layer, 'aspect', unit = 'degrees')  

#calculating slope
streamDEM$slope <- terrain(streamDEM$layer, 'slope')

#Extracting all the variables
#note this takes about 2.5 hours to run
demdf <- extract(streamDEM, ras, df=TRUE)



# Write Files -------------------------------------------------------------

writeRaster(streamDEM, file.path(datapath, 'processed/localDEM.grd'))

writeRaster(streamDEM, file.path(datapath,'processed/streamGeography.grd'),
            overwrite=TRUE)

saveRDS(demdf, file.path(datapath, 'processed/extractedDEMvalues.RDS'))
