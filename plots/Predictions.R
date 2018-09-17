library(raster)
library(rgdal)
library(RColorBrewer)
library(ggmap)
library(tmap)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'

frst <- readOGR()
fp <- readRDS(file.path(datapath, 'results/forestpolygons.RDS'))
r0 <- readRDS(file.path(datapath,'results/rasiplot.RDS'))

r <- r0[, c('id','rasi','prob','predicted')]
names(r)[3] <- 'Probability'
names(fp) <- 'Forest'

rpres <- r[r$rasi==1, ]
rabs <- r[r$rasi==0, ]
forestcols <- c('grey70','grey55', 'grey40')



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


# base plotting -----------------------------------------------------------



probpal <- colorRampPalette(brewer.pal(7, 'YlOrRd'))


r$probcol <-  probpal(50)[as.numeric(cut(r$prob, breaks = 50))]



png(file.path(datapath, 'results/plots/occupancy/occupied.png'), 
    width=1200, height=1200, res=400)
    plot(fp, col=forestcols, border=forestcols,
        main='R. Sierrae Occupied Streams')
    plot(rpres, col=rpres$probcol, lwd=1, add=TRUE)
    plot(rabs, col='black', lwd=0.5, add=TRUE)
dev.off()

png(file.path(datapath, 'results/plots/occupancy/empty.png'), 
    width=1200, height=1200, res=400)
    plot(fp, col=forestcols, border=forestcols,
        main='R. Sierrae Unoccupied Streams')
    plot(rabs, col=rabs$probcol, lwd=0.5, add=TRUE)
    plot(rpres, col='black', lwd=0.5, add=TRUE)
dev.off()

png(file.path(datapath, 'results/plots/occupancy/allprobabilities.png'), 
    width=1200, height=1200, res=400)
    plot(fp, col=forestcols, border=forestcols,
     main='R. Sierrae Streams')
    plot(r, col=r$probcol, lwd=0.5, add=TRUE)
dev.off()



# tmap -------------------------------------------------------------------


bb <- extent(c(-165600, -200, 140000, 266200))

allprob <- tm_shape(fp, bbox=bb, is.master = TRUE) + 
    tm_fill(col='Forest', palette=forestcols, title='National Forest') +
    tm_shape(r) + 
    tm_lines(col='Probability', lwd=1.5, palette = probpal(10)) +
    tm_layout(title='Probability of Stream Occupancy for R. sierrae',
              legend.bg.color="grey87", legend.width = -0.2)

allfn <- file.path(datapath, 'results/plots/occupancy/allprobabilities.tiff')
save_tmap(allprob, filename = allfn)


bbp <- extent(c(-155600, -200, 140000, 266200))

pres <- tm_shape(fp, bbox=bb, is.master = TRUE) + 
    tm_fill(col='Forest', palette=forestcols, title='National Forest') +
    tm_shape(rpres) + 
    tm_lines(col='Probability', lwd=1.5, palette = probpal(10)) +
    tm_shape(rabs) + 
    tm_lines(col='black') +
    tm_layout(title='Probability of Stream Occupancy for Occupied Streams',
              legend.bg.color="grey87", legend.width = -0.2)

presfn <- file.path(datapath, 'results/plots/occupancy/occupied.tiff')
save_tmap(pres, filename = presfn)


absplot <- tm_shape(fp, bbox=bb, is.master = TRUE) + 
    tm_fill(col='Forest', palette=forestcols, title='National Forest') +
    tm_shape(rabs) + 
    tm_lines(col='Probability', lwd=1.5, palette = probpal(10)) +
    tm_shape(rpres) + 
    tm_lines(col='black') +
    tm_layout(title='Probability of Stream Occupancy for Unccupied Streams',
              legend.bg.color="grey87", legend.width = -0.2)

absfn <- file.path(datapath, 'results/plots/occupancy/empty.tiff')
save_tmap(absplot, filename = absfn)




