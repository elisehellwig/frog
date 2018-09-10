library(raster)
library(RColorBrewer)


datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'

fp <- readRDS(file.path(datapath, 'results/forestpolygons.RDS'))
r <- readRDS(file.path(datapath,'results/rasiplot.RDS'))



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

# plotting ----------------------------------------------------------------



probpal <- colorRampPalette(brewer.pal(7, 'YlOrRd'))


r$probcol <-  probpal(50)[as.numeric(cut(r$prob, breaks = 50))]

rpres <- r[r$rasi==1, ]
rabs <- r[r$rasi==0, ]
forestcols <- c('grey80','grey60', 'grey40')

plot(fp, col=forestcols, border=forestcols,
     main='R. Sierrae Populated Streams')
plot(rpres, col=r$probcol, lwd=2, add=TRUE)
plot(rabs, col='black', lwd=1.5, add=TRUE)


plot(fp, col=forestcols, border=forestcols,
     main='R. Sierrae Populated Streams')
plot(rabs, col=r$probcol, lwd=2, add=TRUE)
plot(rpres, col='black', lwd=1.5, add=TRUE)
