#######################Data Prep######################
## this script imports the CA and NV data and merges it.


# Setup -------------------------------------------------------------------


#loading required packages 
library(raster)
library(rgdal)
library(tidyverse)
options(stringsAsFactors = FALSE)

#saving paths I use to variables for later use
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions for preprocessing of data
source(file.path(funpath, 'preprocess.R'))

#importing spatial streams data
ca <- shapefile(file.path(datapath,
            'raw/rasi/NHDflowline_Streams_Final_withRASI.shp'))
nv <- shapefile(file.path(datapath, 
            'raw/rasi/NHDflowlineGB_Streams_Final_withRASI.shp'))


# Spatial Merge ------------------------------------------------------


#adding state variable
ca$state <- 'CA'
nv$state <- 'NV'


#converting all names to lower case to help with column name matching
names(ca) <- tolower(names(ca))
names(nv) <- tolower(names(nv))

#removing underscores for column name consistency when merging
names(ca) <- gsub("ppt_q","pptq", names(ca))
names(nv) <- gsub("fdate_1", 'fdate', names(nv))

#getting all the columns both sets of data have
varnames <- intersect(names(ca), names(nv))

nvMerge <- nv[, varnames]
caMerge <- ca[, varnames]

#merging the datasets with only the columns they share
streams <- rbind(nvMerge, caMerge)


# Cleaning Streams Data ---------------------------------------------------

#removing the old rasi pa data
names(streams) <- gsub('_prese', '', names(streams))

streams$fcode <- recode(streams$fcode, `46006`='perennial', 
                        `46003`='intermittent', `46000`='other')


#calculating stream reach length, in km
streams$length <- SpatialLinesLengths(streams, longlat = TRUE)

streams$rasichk <- NULL

# Saving output ---------------------------------------------------------


#removing the column 'comid_12'
c12_ID <- which(names(streams)=='comid_12')
streamsfinal <- streams[,-c12_ID]


#saving the spatiallinesdataframe to to a smaller file type
saveRDS(streamsfinal, file.path(datapath, 'processed/RasiStreamLines1.RDS'))

#Saving spatiallinesdataframe as as shapefile
shapefile(streamsfinal, 
          file.path(datapath, 'processed/shapefiles/RasiStreamLines1.shp'),
          overwrite=TRUE)

