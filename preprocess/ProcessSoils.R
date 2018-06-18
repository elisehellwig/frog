###############Process Soils#####################

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(foreign)

datapath <- '/Users/echellwig/Research/frogData/data/'

soil <- shapefile(file.path(datapath, 
                'frog_model/data/output/MergedSoilsClipFinal.shp'))

ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines.RDS'))

#########################################################

soill <- spTransform(soil, crs(ras))
soilorder <- soill[,'SoilOrderD']

soilagg <- aggregate(soill)
geodif <- gDifference(ras, soilagg, byid=TRUE)

rasSoil <- over(ras, soilorder, returnList = TRUE)

rs <- sapply(rasSoil, function(rs) rs$SoilOrderD)
rsNA <- sapply(rs, function(v) getMode(v))

rasSoilvec <- sapply(rasSoil, function(rs) getMode(rs$SoilOrderD))

soil <- rbind(soilca[, varnames], soilnv[,varnames])

#removing rows from soil that dont exist in ras


RasiSoil <- merge(ras, soil, by.x='comid',by.y='NHDFlowlin')


