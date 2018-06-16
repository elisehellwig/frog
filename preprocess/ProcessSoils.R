###############Process Soils#####################

library(tidyverse)
library(raster)
library(rgdal)
library(foreign)

datapath <- '/Users/echellwig/Research/frogData/data/'

soilca <- read.dbf(file.path(datapath, 'cathy/soils/soil_intersect3.dbf'),
                   as.is=TRUE)
soilnv <-  read.dbf(file.path(datapath, 'cathy/soils/soil_intersectgb.dbf'),
                    as.is=TRUE)

geo <- readRDS(file.path(datapath, 'processed/streamsGEO.RDS'))

#########################################################

soilca$state <- 'CA'
soilnv$state <- 'NV'

varnames <- intersect(names(soilca), names(soilnv))

soil <- rbind(soilca[, varnames], soilnv[,varnames])

geosoil <- merge(geo, soil, by.x='comid','NHDFlowlin')


