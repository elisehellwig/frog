###################DEM Process#####################
library(tidyverse)
library(raster)
library(rgdal)

datapath <- '/Users/echellwig/Research/frogData/data/'

grids <- paste0('n', rep(c(40,41), each=3), 'w', rep(120:122, 2))
fnames <- paste0('float', grids, '_13.flt')

grids[3] <- 'USGS_NED_13_n40w122_GridFloat'
fnames[3] <- 'usgs_ned_13_n40w122_gridfloat.flt'


demlist <- lapply(seq_along(fnames), function(i) {
    fn <- file.path(datapath,'DEM', grids[i], fnames[i])
    print(fn)
    raster(fn)
})

dem <- do.call(merge, demlist)

streamExt <- extent(-121.7, -120, 39.2, 40.4)

streamDEM <- crop(dem, streamExt)

writeRaster(streamDEM, file.path(datapath, 'processed/localDEM.grd'))
streamDEM <- raster(file.path(datapath, 'processed/localDEM.grd'))

streamDEM$aspect <- terrain(streamDEM$layer, 'aspect', unit = 'degrees')  


cardseq <-c(0, rep(seq(22.5, 337.5, by=22.5), each=2), 360)
mcard1 <- matrix(cardseq, ncol=2, byrow=TRUE)
mcard2 <- cbind(mcard1, 1:16)
rcard <- raster(streamDEM)

#Convert to cardinal directions N1=(0-45, 315-360), E2=(45-135), S3=(135-225), 
    #W4=(225-315)

streamDEM$slope <- terrain(streamDEM$layer, 'slope')

writeRaster(streamDEM, file.path(datapath, 'processed/streamGeography.grd'))


