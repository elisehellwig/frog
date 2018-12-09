
# Setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'plotting.R'))


rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))
allvars <- names(rasi)

# Data formatting ---------------------------------------------------------
biovars <- paste0('bio',1:19)
unitconv <- c(0.1, 0.1, 0.01, 0.01, rep(0.1, 7), rep(1, 8) )

for (i in 1:19) {
    rasi[,biovars[i]] <- rasi[,biovars[i]]*unitconv[i]
}

#removing high leverage points
highlevs <- c(8933706, 8933732, 8063647)
rasi <- rasi[!(rasi$id %in% highlevs), ]

#transforming some variables
rasi$meadows <- sqrt(rasi$meadows)
rasi$waterbodies <- sqrt(rasi$waterbodies)
rasi$totDrainArea <- sqrt(rasi$totDrainArea)
rasi$divDrainArea <- sqrt(rasi$divDrainArea)

#managing factors
rasi$rasi <- factor(rasi$rasi)
rasi$streamOrder <- factor(rasi$streamOrder)
rasi$treesize <- as.factor(recode(rasi$treesize, `0`='No Trees', 
                                  `1`='Small Trees', `2`='Small Trees',
                                  `3`='Small Trees',`4`='Large Trees',
                                  `5`='Large Trees', `6`='Large Trees'))

levels(rasi$canopyClosure) <- c('Dense','Moderate','Open','Sparse','None')
rasi$canopyClosure <- factor(rasi$canopyClosure,
                             levels(rasi$canopyClosure)[5:1])
levels(rasi$habitat) <- c('Conifer','Hardwood','Herbaceous','Mixed Forest',
                          'Not Vegetated','Shrub')

rasi <- rasi[rasi$elevmax>1524, ]

# Write Plots -------------------------------------------------------------

vars <- allvars[ c(4, 6, 9:44)]
titles <- c('Stream Reach Length','Mean Elevation', 'Mean Slope', 
            'Stream Seasonality', 'Modified Strahler Stream Order', 
            'Bedrock Class', 'Soil Type', 
            'Wildlife Habitat Relationship\n(WHR) Type', 'Tree Age Class',
            'Canopy Closure', 'Total Upstream Cumulative\nDrainage Area',
            'Divergence-Routed Cumulative\nDrainage Area', 
            'Longitude', 'Latitude', 'Rock Type', 'Nearby Meadows',
            'Nearby Bodies of Water', 'Mean Reach Aspect',
            'National Forest',
            'BIO 1: Annual Mean Temperature', 'BIO 2: Mean Diurnal Range',
            'BIO 3: Isothermality','BIO 4: Temperature Seasonality',
            'BIO 5: Max Temp of Warmest Month', 
            'BIO 6: Min Temp of Coldest Month', 'BIO 7: Annual Temp Range',
            'BIO 8: Mean Temp of Wettest Quarter',
            'BIO 9: Mean Temp of Driest Quarter',
            'BIO 10: Mean Temp of Warmest\nQuarter',
            'BIO 11: Mean Temp of Coldest\nQuarter', 
            'BIO 12: Annual Precipitation',
            'BIO 13: Precipitation in Wettest Month', 
            'BIO 14: Precipitation in Driest Month',
            'BIO 15: Precipitation Seasonality',
            'BIO 16: Precipitation in Wettest\nQuarter',
            'BIO 17: Precipitation in Driest\nQuarter',
            'BIO 18: Precipitation in Warmest\nQuarter',
            'BIO 19: Precipitation in Coldest\nQuarter')

xlabels <- c('Length (km)', 'Elevation (m)', 'Slope (radians)',  '', 
             'Stream Order', '', '', '', 'Age Class', '',
             expression(sqrt('Drainage Area (sq. km)')), 
             expression(sqrt('Drainage Area (sq. km)')), 
             'Longitude (degrees)', 'Latitude (degrees)','', 
             expression(sqrt('Number of Meadows')), 
             expression(sqrt('Number of Waterbodies')), 
             'Direction (degrees)','',
             'Temperature (deg C)', 'Degrees C', 'Degrees C', 'Degrees C',
             'Temperature (deg C)', 'Temperature (deg C)', 'Degrees C',
             'Temperature (deg C)','Temperature (deg C)','Temperature (deg C)',
             'Temperature (deg C)', 'Precipitation (mm)', 'Precipitation (mm)',
             'Precipitation (mm)', 'Precipitation (mm)', 'Precipitation (mm)',
             'Precipitation (mm)','Precipitation (mm)','Precipitation (mm)')

#length(vars)

plotlist <- lapply(1:length(vars), function(i) {
    #print(vars[i])
    summaryPlot(rasi, vars[i], xlab=xlabels[i], plotTitle=titles[i],
                size=25,
                legend = expression(italic('R. sierrae')))
})

for (i in 1:length(vars)) {
    #width must be set to 1050 for i=6 (bedrock)
    print(vars[i])
    filename <- file.path(datapath, 'results/plots/summary2', 
                         paste0(vars[i], '.tiff'))
    tiff(filename, width=600)
       print(plotlist[[i]])
    dev.off()
}



