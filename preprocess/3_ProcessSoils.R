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

soill <- spTransform(soil, crs(ras))

ras$soiltype <- overChr(ras, soill, 'SoilOrderD')


#for plotting
soilagg <- aggregate(soill)
geodif <- gDifference(ras, soilagg, byid=TRUE)

