###############Process Soils#####################

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(foreign)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'qc.R'))

soil <- shapefile(file.path(datapath, 
                'frog_model/data/output/MergedSoilsClipFinal.shp'))

ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines.RDS'))

#########################################################

soill <- spTransform(soil, crs(ras))

rsv <- overChr(ras, soill, 'SoilOrderD')

soil <- rbind(soilca[, varnames], soilnv[,varnames])

#for plotting
soilagg <- aggregate(soill)
geodif <- gDifference(ras, soilagg, byid=TRUE)

#removing rows from soil that dont exist in ras


RasiSoil <- merge(ras, soil, by.x='comid',by.y='NHDFlowlin')


