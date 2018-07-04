###################DEM Process#####################
#this file imports some DEM (Digital Elevation Model) tiles from USGS, merges and crops them and then
#calculates some variables of interest (ex. slope, aspect etc.)


# Setup -------------------------------------------------------------------


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

#change to RasiStreamLines3 when that gets created
ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines3.RDS'))



# Create DEM --------------------------------------------------------------
# ## only need to do this once
# 
# #creating file names of the DEMS
# grids <- paste0('n', rep(c(40,41), each=3), 'w', rep(120:122, 2))
# fnames <- paste0('float', grids, '_13.flt')
# 
# #modifying the third one because USGS can't seem to be consistent in their
# #file structure
# grids[3] <- 'USGS_NED_13_n40w122_GridFloat'
# fnames[3] <- 'usgs_ned_13_n40w122_gridfloat.flt'
# 
# 
# #loading the DEM tiles into R in list form 
# demlist <- lapply(seq_along(fnames), function(i) {
#     fn <- file.path(datapath,'DEM', grids[i], fnames[i])
#     raster(fn)
# })
# 
# #merging the dem tiles together into one object
# dem <- do.call(merge, demlist)
# 
# #Creating a bounding box to use to crop the DEM object
# streamExt <- extent(-121.7, -120, 39.2, 40.4)
# 
# #croping the object so we only have the DEM where we have streams
# streamDEM <- crop(dem, streamExt)
# 
# #saving the DEM as a GRD
# writeRaster(streamDEM, file.path(datapath, 'processed/localDEM.grd'))
# 

# Process DEM -------------------------------------------------------------



streamDEM <- raster(file.path(datapath, 'processed/localDEM.grd'))

#calcualting aspect
streamDEM$aspect <- terrain(streamDEM$layer, 'aspect', unit = 'degrees')  

#calculating slope
streamDEM$slope <- terrain(streamDEM$layer, 'slope')

#saving everything to a file
writeRaster(streamDEM, file.path(datapath,'processed/streamGeography.grd'),
            overwrite=TRUE)



# Extract Values ----------------------------------------------------------

#dem <- brick(file.path(datapath,'processed/streamGeography.grd'))

#note this takes about 2.5 hours to run
#demdf <- extract(dem, ras, df=TRUE)
#saveRDS(demdf, file.path(datapath, 'processed/extractedDEMvalues.RDS'))

demdf <- readRDS(file.path(datapath, 'processed/extractedDEMvalues.RDS'))
demdf$fid <- as.factor(demdf$ID)

#Calculating slope summary statistics for each reach
ras$slopemax <- collapseVariable(demdf$slope, demdf$fid, fun = max)
ras$slopemin <- collapseVariable(demdf$slope, demdf$fid, fun = min)
ras$slopemean <- collapseVariable(demdf$slope, demdf$fid, fun = mean)

#elevation 
ras$elevmax <- collapseVariable(demdf$layer, demdf$fid, fun = max)
ras$elevmin <- collapseVariable(demdf$layer, demdf$fid, fun = min)


# Process Aspect ----------------------------------------------------------

#create aspect key

## aspect values
vals <- c(0, rep(seq(22.5, 337.5, by=45), 2), 360)
aspdf <- as.data.frame(matrix(vals, ncol=2))

names(aspdf) <- c('min', 'max')

#IDs 
aspdf$id <- 1:nrow(aspdf)

#cardinal directions
aspdf$string <- c('N', 'NE', 'E', "SE",'S','SW','W','NW', 'N')


ras$cardinal <- sapply(1:length(ras), function(i) {
    v <- demdf[demdf$ID==i, 'aspect']
    vcard <- recodeRange(v, aspdf)
    getMode(vcard)
    
})

# Subset rows/columns ------------------------------------------------------

NAids  <- which(!is.na(ras$whrtype))
ras <- ras[NAids, ]

rnames1 <- c('comid', 'state', 'rasi', 'length', 'elevmax','elevmin', 
             'slopemax', 'slopemin', 'slopemean', 'cardinal',  
             'fcode', 'streamorde', 'ptype', 'soiltype', 'whrtype',
             'totdasqkm', 'divdasqkm')


var <- rep(c('ppt','tmax','tmin','tmean'), each=20)
quarter <- rep(rep(paste0('q', 1:4), each=5), 4)
sumstat <- rep(c('min','max','beg','end','lwm'), 16)

rnames2 <- paste0(var, quarter, sumstat)

rnames <- c(rnames1, rnames2)

rasfinal <- ras[, rnames]

names(rasfinal)[c(1, 11:17)] <- c('id', 'seasonality','streamOrder',
                                  'bedrock','soil','habitat','totDrainArea',
                                  'divDrainArea')

cc <- complete.cases(data.frame(rasfinal))

rasfinal <- rasfinal[cc, ]

# write files -------------------------------------------------------------



saveRDS(rasfinal, file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
shapefile(rasfinal, file.path(datapath, 
                         'processed/shapefiles/RasiStreamLinesFinal.shp'),
          overwrite=TRUE)

