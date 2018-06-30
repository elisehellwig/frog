#######################Data Prep######################
## this script imports the CA and NV data and merges it.

###############Setup##############
#loading required packages 
library(raster)
library(rgdal)
library(tidyverse)

#saving paths I use to variables for later use
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions for preprocessing of data
source(file.path(funpath, 'preprocess.R'))

#importing spatial streams data
ca <- shapefile(file.path(datapath,
            'cathy/rasi_streams/NHDflowline_Streams_Final_withRASI.shp'))
nv <- shapefile(file.path(datapath, 
            'cathy/rasi_streams/NHDflowlineGB_Streams_Final_withRASI.shp'))

#importing cathy's data to see what we need
#########################################################
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

names(streams) <- gsub("_prese", '', names(streams))

streams$fcode <- recode(streams$fcode, `46006`='perennial', 
                        `46003`='intermittent', `46000`='other')


#calculating stream reach length
streams$length <- SpatialLinesLengths(streams)


#removing the column 'comid_12'
c12_ID <- which(names(streams)=='comid_12')
streamsfinal <- streams[,-c12_ID]


#saving the spatiallinesdataframe to to a smaller file type
saveRDS(streamsfinal, file.path(datapath, 'processed/RasiStreamLines1.RDS'))

#Saving spatiallinesdataframe as as shapefile
shapefile(streamsfinal, 
          file.path(datapath, 'processed/shapefiles/RasiStreamLines1.shp'),
          overwrite=TRUE)

