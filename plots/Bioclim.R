library(raster)
library(RColorBrewer)
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'


rs <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
bio <- getData('worldclim', var='bio', res=0.5, lon=-121, lat=39)
names(bio) <- gsub('_11', '', names(bio))

temppal <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')))

# split forests -----------------------------------------------------------

forests <- c('Lassen','Plumas','Tahoe')

rsf <- lapply(forests, function(f) {
    rs[rs$forest==f, ]
})

forestExtents <- lapply(rsf, function(f) {
    extent(f)
})


bioR <- crop(bio, rs)
pres <- rs[rs$rasi==1, ]
abse <- rs[rs$rasi==0, ]

par(bty='n')


# Bio11 by Forest -------------------------------------------------------

bio11 <- bioR$bio11/10

png(file.path(datapath, 'results/plots/bio11.png'))
    par(bty='n')
    plot(bio11, col=temppal(255), axes=FALSE, bty='n',
        main='Mean Temperature of Coldest Quarter (degrees C)',
        sub='Black=Presence, Absence: Red = Lassen, Green = Plumas, Purple = Tahoe')
    plot(rsf[[1]], col='red',add=TRUE)
    plot(rsf[[2]], col='green4',add=TRUE)
    plot(rsf[[3]], col='purple',add=TRUE)
    plot(pres, lwd=2, add=TRUE)
dev.off()
# Bio11 by presence/absence -------------------------------------------------

bio11 <- bioR$bio11/10
plot(bio11, col=temppal(255), axes=FALSE,
     main='Mean Temperature of Coldest Quarter', 
     sub='Purple = R. sierrae present')
plot(pres, col='purple2', add=TRUE, lwd=2.5)
plot(abse, add=TRUE, lwd=0.5)


