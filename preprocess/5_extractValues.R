##################Extract Values#####################

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(ehelpr)

funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'preprocess.R'))
orig <- read.table(file.path(datapath, 'cathy/rasi_mxt_allstr3.tab'),
                   header=TRUE, stringsAsFactors = FALSE, sep='\t')


rasi <- readRDS(file.path(datapath, 'processed/RasiStreamLines.RDS'))
dem <- brick(file.path(datapath,'processed/streamGeography.grd'))
whr <- readRDS(file.path(datapath, 'processed/whr.RDS'))
soil <- shapefile(file.path(datapath, 
                            'frog_model/data/output/MergedSoilsClipFinal.shp'))

ll <- crs(rasi)

############################################################################
whrll <- spTransform(whr, ll)

rasi$whrtype <- overChr(rasi, whrll, 'WHRTYPE')



######################################################################
#####################Slope and aspect#################################


#Setting up the key for recoding aspect to a character
cardseq <-c(0, rep(seq(22.5, 337.5, by=45), each=2), 360)
mcard1 <- matrix(cardseq, ncol=2, byrow=TRUE)
dfcard <- data.frame(mcard1)
dfcard$id <- 1:9
dfcard$string <- c('N','NE','E','SE','S','SW','W','NW','N')
names(dfcard)[1:2] <- c('min','max')

streamDEM$cardinal <- recodeRange(streamDEM$aspect, dfcard, string=TRUE,
                                  digits=0) 







