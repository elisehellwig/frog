#######################Data Prep######################
## this script imports the CA and NV data and merges it.


# Setup -------------------------------------------------------------------


#loading required packages 
library(raster)
library(rgdal)
options(stringsAsFactors = FALSE)

#saving paths I use to variables for later use
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'

#importing functions for preprocessing of data
source(file.path(funpath, 'preprocess.R'))

#importing spatial streams data
ns <- shapefile(file.path(datapath,
            'raw/rasi/NHDflowline_Streams_Final_withRASI.shp'))
gb <- shapefile(file.path(datapath, 
            'raw/rasi/NHDflowlineGB_Streams_Final_withRASI.shp'))


# Spatial Merge ------------------------------------------------------


#adding dataset variable
ns$source <- 'NorthernSierras'
gb$source <- 'GreatBasin'


#converting all names to lower case to help with column name matching
names(ns) <- tolower(names(ns))
names(gb) <- tolower(names(gb))

#removing underscores for column name consistency when merging
names(ns) <- gsub("ppt_q","pptq", names(ns))
names(gb) <- gsub("fdate_1", 'fdate', names(gb))

#getting all the columns both sets of data have
varnames <- intersect(names(ns), names(gb))

gbMerge <- gb[, varnames]
nsMerge <- ns[, varnames]

#merging the datasets with only the columns they share
streams <- rbind(gbMerge, nsMerge)


# Cleaning Streams Data ---------------------------------------------------

#removing the old rasi pa data
names(streams) <- gsub('_prese', '', names(streams))

streams$fcode <- dplyr::recode(streams$fcode, `46006`='perennial', 
                        `46003`='intermittent', `46000`='intermittent')


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

