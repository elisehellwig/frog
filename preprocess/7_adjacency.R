
# Setup -------------------------------------------------------------------



library(raster)
library(rgdal)
library(rgeos)
library(spdep)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))

rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))

TA <- TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')


rasi <- spTransform(rasiSP, TA)
rasi$index <- 1:length(rasi)
rasi$ras <- as.numeric(as.character(rasi$rasi))

# Buffers -----------------------------------------------------------------

buffers <- buffer(rasi, width=0.5, dissolve=FALSE)

nbl <- poly2nb(buffers, snap=5)
lw <- nb2listw(nbl, zero.policy = TRUE, style='B')
am <- nb2mat(nbl, zero.policy = TRUE, style='B')
g <- graph_from_adjacency_matrix(am, mode='undirected')

shpa <- get.shortest.paths(g, 1)

steps <- sapply(1:length(nbl), function(i) {
    minSteps(g, i, presentIDs)
})

minSteps(g, 4, presentIDs)

moran.mc(rasi$ras, lw, nsim=9999, zero.policy = TRUE)

presentIDs0 <- rasi$index * rasi$ras
presentIDs <- intersect(presentIDs0, rasi$index)

la <- sapply(rasi$index, function(i) {
    any(nbl[[i]] %in% presentIDs)*1
})
