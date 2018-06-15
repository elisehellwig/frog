#######################Data Prep######################
## this script imports the stream and processes it for analysis.

###############Setup##############
library(raster)
library(rgdal)
library(tidyverse)

datapath <- '/Users/echellwig/Research/frogData/data/'

ca <- shapefile(file.path(datapath, 'frog_model/NHDFlowline_Streams_Final.shp'))
nv <- shapefile(file.path(datapath, 
                          'frog_model/NHDFlowlineGB_Streams_Final.shp'))

x <- read.table(file.path(datapath, 'cathy/rasi_mxt_allstr3.tab'), sep='\t',
                header=TRUE)

#########################################################

names(ca) <- tolower(names(ca))
names(nv) <- tolower(names(nv))

names(ca) <- gsub(".*q","prcp_q", names(ca))
names(nv) <- gsub(".*q","prcp_q", names(nv))



