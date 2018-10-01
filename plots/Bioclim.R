library(raster)
library(RColorBrewer)
library(tmap)
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'


rs <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
bio <- getData('worldclim', var='bio', res=0.5, lon=-121, lat=39)
names(bio) <- gsub('_11', '', names(bio))

heatpal <- colorRampPalette(brewer.pal(9, 'YlOrRd'))
temppal <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')))
precippal <- colorRampPalette(brewer.pal(9, 'Blues'))

# Process data -----------------------------------------------------------

rs <- rs[rs$elevmax>1524, ]

forests <- c('Lassen','Plumas','Tahoe')

rsf <- lapply(forests, function(f) {
    rs[rs$forest==f, ]
})

forestExtents <- lapply(rsf, function(f) {
    extent(f)
})

bb <- extent(-121.57, -120.02, 39.25, 40.35)
bioR <- crop(bio, bb)

biovars <- paste0('bio',1:19)
unitconv <- c(0.1, 0.1, 0.01, 0.01, rep(0.1, 7), rep(1, 8) )

for (i in 1:19) {
    values(bioR)[,biovars[i]] <- values(bioR)[,biovars[i]]*unitconv[i]
}

pres <- rs[rs$rasi==1, ]
abse <- rs[rs$rasi==0, ]

par(bty='n')


# Titles and Units --------------------------------------------------------

biotitles <- c('BIO 1: Annual Mean Temperature', 'BIO 2: Mean Diurnal Range',
            'BIO 3: Isothermality','BIO 4: Temperature Seasonality',
            'BIO 5: Max Temp of Warmest Month', 
            'BIO 6: Min Temp of Coldest Month', 'BIO 7: Annual Temp Range',
            'BIO 8: Mean Temp of Wettest Quarter',
            'BIO 9: Mean Temp of Driest Quarter',
            'BIO 10: Mean Temp of Warmest Quarter',
            'BIO 11: Mean Temp of Coldest Quarter', 
            'BIO 12: Annual Precipitation',
            'BIO 13: Precipitation of Wettest Month', 
            'BIO 14: Precipitation of Driest Month',
            'BIO 15: Precipitation Seasonality',
            'BIO 16: Precipitation of Wettest Quarter',
            'BIO 17: Precipitation of Driest Quarter',
            'BIO 18: Precipitation of Warmest Quarter',
            'BIO 19: Precipitation of Coldest Quarter')


xlabels <- c('Temperature (C)', 'Degrees C', 'Degrees C', 'Degrees C',
             'Temperature (C)', 'Temperature (C)', 'Degrees C',
             'Temperature (C)', 'Temperature (C)', 'Temperature (C)',
             'Temperature (C)', 'Precipitation (mm)', 'Precipitation (mm)',
             'Precipitation (mm)', 'Precipitation (mm)', 'Precipitation (mm)',
             'Precipitation (mm)','Precipitation (mm)','Precipitation (mm)')


pals <- unlist(c(replicate(5, heatpal), temppal, heatpal, temppal, heatpal, 
                 heatpal, temppal, replicate(8, precippal)))


biobreaks <- list(seq(0, 20, by=10), seq(10, 20, by=5), 
                  seq(0.4, 0.5, by=0.05),
                  seq(55, 75, by=10), seq(20, 36, by=8), 
                  seq(-15, 5, by=5), seq(25, 40, by=5), seq(-5, 10, by=5),
                  seq(10, 25, by=5), seq(10, 25, by=5), seq(-5, 10, by=5),
                  seq(200, 1800, by=800), seq(0, 400, by=200), 
                  seq(0, 20, by=5), seq(40, 90, by=25), 
                  seq(0, 1000, by=250), seq(0, 70, by=35), 
                  seq(0,70, by=35), seq(0,1000, by=500))

pals <- c(rep('seq', 5), 'div','seq','div','seq','div','div')

precipR <- subset(bioR, subset=12:19)
# Plotting Temp ---------------------------------------------------------

forestpal <- c('deepskyblue','forestgreen', 'purple')
forestpal2 <- c('darkorange3','forestgreen', 'purple')


bb <- extent()

for (i in 1:2) {
    fn <- paste0(biovars[i],'.tiff')
    biopath <- file.path(datapath, 'results/plots/bioclim', fn)

    bioplot <- tm_shape(bioR) +
        tm_raster(col=biovars[i], palette=pals[i], style='cont',
                  alpha=0.7, title=xlabels[i],
                  breaks=biobreaks[[i]]) + 
        tm_shape(pres) +
        tm_lines(col='black', lwd=1.5) +
        tm_shape(abse) +
        tm_lines(col='forest', palette=forestpal2, 
                 lwd=1.5, title.col='National Forest') +
        tm_layout(main.title=biotitles[i], 
                  aes.palette = list(seq='YlOrRd', 
                                     div=rev(brewer.pal(9, 'RdYlBu'))),
                  legend.position = c('left', 'bottom'),
                  legend.bg.color="grey87", legend.width = -0.2,
                  legend.height = -0.4, legend.title.size = 0.8,
                  title.size = 0.5)
    
    save_tmap(bioplot, filename=biopath)
}


# PrecipPlot --------------------------------------------------------------

for (i in 12:19) {
    fn <- paste0(biovars[i],'.tiff')
    biopath <- file.path(datapath, 'results/plots/bioclim', fn)
    
    bioplot <- tm_shape(bioR) +
        tm_raster(col=biovars[i], palette='seq', style='cont',
                  alpha=0.7, title=xlabels[i],
                  breaks=biobreaks[[i]]) + 
        tm_shape(pres) +
        tm_lines(col='black', lwd=1.5) +
        tm_shape(abse) +
        tm_lines(col='forest', palette=forestpal2, 
                 lwd=1.5, title.col='National Forest') +
        tm_layout(main.title=biotitles[i], 
                  aes.palette = list(seq='Blues'),
                  legend.position = c('left', 'bottom'),
                  legend.bg.color="grey87", legend.width = -0.2,
                  legend.height = -0.4, legend.title.size = 0.8,
                  title.size = 0.5)
    
    save_tmap(bioplot, filename=biopath)
}
