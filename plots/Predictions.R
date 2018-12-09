
# Setup -------------------------------------------------------------------
library(raster)
library(rgdal)
library(spatialEco)
library(RColorBrewer)
library(ggmap)
library(tmap)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'

frst <- readOGR(dsn = file.path(datapath, 
                                'raw/forest/S_USA.AdministrativeForest.shp'))
bound <- readOGR(dsn = file.path(datapath,
                                 'raw/rasi/boundary/StudyBoundaryFinal.shp'))

fp <- readRDS(file.path(datapath, 'results/forestpolygons.RDS'))
r0 <- readRDS(file.path(datapath,'results/rasiplot.RDS'))
rr <- read.csv(file.path(datapath, 'results/RasiResultsDF.csv'))

fnames <- c('Lassen National Forest','Plumas National Forest',
            'Tahoe National Forest')

probpal <- colorRampPalette(brewer.pal(7, 'YlOrRd'))


# Data Processing ---------------------------------------------------------
rrr <- merge(r0, rr[, c('id','prob','predicted')], by='id')



r <- rrr[, c('id','rasi','prob.y','predicted.y')]
names(r)[3] <- 'Probability'
names(fp) <- 'Forest'

rpres <- r[r$rasi==1, ]
rabs <- r[r$rasi==0, ]
forestcols <- c('grey70','grey55', 'grey40')

f <- frst[frst$FORESTNAME %in% fnames,]
f$Forest <- c("Plumas", "Lassen","Tahoe")
fta <- spTransform(f, crs(r))
bta <- spTransform(bound, crs(r))

bb <- extent(c(-165600, -200, 140000, 267200))
bb2 <- extent(c(-165700 , -454, 137000, 259000))
bb3 <- extent(c(-165700 , -454, 137000, 255000))


fta2 <- crop(fta, bb2)
fta3 <- remove.holes(remove.holes(fta2))
fta3$Forest <- c("Plumas", "Lassen","Tahoe")
# Creating plot data ------------------------------------------------------


# 
# rasi <- read.csv(file.path(datapath, 'results/RasiResultsDF.csv'))
# 
# rsp <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
# rsp <- rsp[, c('id','source','forest')]
# 
# 
# r <- merge(rsp, rasi, all.x=FALSE, by='id')
# rta <- spTransform(r, crs(forestpolys))
# 
# saveRDS(rta, file.path(datapath,'results/rasiplot.RDS'))
# 


# Cleaning Background Polygons --------------------------------------------
# 
# ogrListLayers(file.path(datapath, 'raw/forest/S_USA.AdministrativeForest.shp'))
# 
# forestnames <- c('Lassen National Forest','Plumas National Forest',
#             'Tahoe National Forest')
# NFpath <- file.path(datapath,  'raw/forest/S_USA.AdministrativeForest.shp')
# 
# forest <- readOGR(dsn=NFpath,
#         layer='S_USA.AdministrativeForest')
# 
# f3 <-  forest[forest$FORESTNAME %in% forestnames, ]
# 


# tmap -------------------------------------------------------------------


maptitle <- expression(paste('Probability of Stream Occupancy for ',
                              italic('Rana sierrae')))

allprob <- tm_shape(fta3, bbox=bb, is.master = TRUE) + 
    tm_fill(col='Forest', palette=forestcols, 
            title='National Forest') +
    tm_shape(r) + 
    tm_lines(col='Probability', lwd=1.5, palette = probpal(10)) +
    tm_shape(bta) +
    tm_borders(col='black', lwd=3) +
    tm_layout(title=maptitle, legend.bg.color="grey87", legend.width = -0.2)

allfn <- file.path(datapath, 
           'results/plots/occupancy/allprobabilities2.tiff')
save_tmap(allprob, filename = allfn)


bbp <- extent(c(-155600, -200, 140000, 266200))

pres <- tm_shape(fta3, bbox=bb, is.master = TRUE) + 
    tm_fill(col='Forest', palette=forestcols, title='National Forest') +
    tm_shape(rpres) + 
    tm_lines(col='Probability', lwd=1.5, palette = probpal(10)) +
    tm_shape(rabs) + 
    tm_lines(col='black') +
    tm_shape(bta) +
    tm_borders(col='black', lwd=3) +
    tm_layout(title='Probability of Stream Occupancy for Occupied Streams',
              legend.bg.color="grey87", legend.width = -0.2)

presfn <- file.path(datapath, 'results/plots/occupancy/occupied.tiff')
save_tmap(pres, filename = presfn)


absplot <- tm_shape(fta3, bbox=bb, is.master = TRUE) + 
    tm_fill(col='Forest', palette=forestcols, title='National Forest') +
    tm_shape(rabs) + 
    tm_lines(col='Probability', lwd=1.5, palette = probpal(10)) +
    tm_shape(rpres) + 
    tm_lines(col='black') +
    tm_shape(bta) +
    tm_borders(col='black', lwd=3) +
    tm_layout(title='Probability of Stream Occupancy for Unccupied Streams',
              legend.bg.color="grey87", legend.width = -0.2)

absfn <- file.path(datapath, 'results/plots/occupancy/empty.tiff')
save_tmap(absplot, filename = absfn)




