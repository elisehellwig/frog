###############Process Soils#####################

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(foreign)
library(ehelpr)

options(stringsAsFactors = FALSE)
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

#extracting out other variables that will help us fill in the NAs
ras$symbol <- overChr(ras, soill, 'EsriSymbol')
ras$soilname <- overChr(ras, soill, 'muname')

#Names and symbols for missing soil types 
missingSoils <- as.data.frame(rasNAs[,c('symbol','soilname')])
write.csv(missingSoils, file.path(datapath, 'processed/missingSoils.csv'),
          row.names=FALSE)

###########################################################

#finding which rows have NAs
soilNAs <- which(is.na(ras$soiltype))

#saving the mu names and the esri symbols for help in identifying soil type
rasNAs <- ras[soilNAs, ]

#save missing soils data
write.csv(rasNAs, file.path(datapath, 'processed/missingSoils.csv'),
          row.names=FALSE)

#read in the df that tells us what to replace all the NAs in the soils data with
soilkey <- read.csv(file.path(datapath, 'processed/missingSoilsKey.csv'))

#doing the replacement
ras$soiltype[soilNAs] <- soilkey$soilkey

saveRDS(ras, file.path(datapath, 'processed/RasiStreamLines.RDS'))


