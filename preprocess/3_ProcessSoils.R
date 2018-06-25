###############Process Soils#####################

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(foreign)
library(ehelpr)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'preprocess.R'))

soil <- shapefile(file.path(datapath, 
                'frog_model/data/output/MergedSoilsClipFinal.shp'))

ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines.RDS'))

#########################################################

#converting the soil polygons to latlon
soill <- spTransform(soil, crs(ras))

#extracting out the SoilOrderD variable to see how many NAs we have
ras$soiltype <- overChr(ras, soill, 'SoilOrderD')


#Aggregate soil area to make it easier to plot
soilagg <- aggregate(soill)

#see where the rasi data is outside of the soil coverage
geodif <- gDifference(ras, soilagg, byid=TRUE)

