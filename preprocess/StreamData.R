#######################Data Prep######################
## this script imports the stream and processes it for analysis.

###############Setup##############
library(raster)
library(rgdal)
library(tidyverse)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'qc.R'))

#importing spatial streams data
ca <- shapefile(file.path(datapath, 'frog_model/NHDFlowline_Streams_Final.shp'))
nv <- shapefile(file.path(datapath, 
                          'frog_model/NHDFlowlineGB_Streams_Final.shp'))


#importing extracted data
x <- read.table(file.path(datapath, 'cathy/rasi_mxt_allstr3.tab'), sep='\t',
                header=TRUE)

#########################################################

#converting all names to lower case
names(ca) <- tolower(names(ca))
names(nv) <- tolower(names(nv))

#removing underscores for consistency when merging
names(ca) <- gsub("ppt_q","pptq", names(ca))


#renaming this col so they are the same in both datasets
fdatID <- which(names(nv)=='fdate_1')
names(nv)[fdatID] <- 'fdate'

#getting all the columns both sets of data have
varnames <- intersect(names(ca), names(nv))

#merging the datasets with only the columns they share
streams <- merge(ca[, varnames],nv[, varnames])

#saving the spatiallinesdataframe to to a smaller file type
saveRDS(streams, file.path(datapath, 'processed/streamsGEO.RDS'))


