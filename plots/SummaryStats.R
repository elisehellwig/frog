
# Setup -------------------------------------------------------------------


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

rasi$rasi <- factor(rasi$rasi)
rasi$streamOrder <- factor(rasi$streamOrder)


# Write Plots -------------------------------------------------------------

vars <- allvars[ c(4:17, 19:21, 23, 104:122)]
titles <- c('Stream Reach Length', 'Minimum Elevation', 'Max Elevation',
            'Maximum Slope', 'Minimum Slope', 'Mean Slope', 'Mean Reach Aspect', 
            'Stream Seasonality','Modified Strahler Stream Order',
            'Bedrock Class', 'Soil Type', 'Wildlife Habitat Relationship (WHR)', 
            'Total Upstream Cumulative Drainage Area',
            'Divergence-Routed Cumulative Drainage Area', 
            'Is the stream perennial?', 'Longitude', 'Latitude', 'Rock Type', 
            'BIO 1: Annual Mean Temperature', 'BIO 2: Mean Diurnal Range',
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

xlabels <- c('Length (km)', 'Elevation (m)', 'Elevation (m)', 'Slope (radians)',
             'Slope (radians)', 'Slope (radians)', '', '', 'Stream Order',
             '', '', '', 'Drainage Area (sq. km)', 'Drainage Area (sq. km)',
             '', 'Longitude (degrees)', 'Latitude (degrees)','',
             'Temperature (deg C)', 'Degrees C', 'Degrees C', 'Degrees C',
             'Temperature (deg C)', 'Temperature (deg C)', 'Degrees C',
             'Temperature (deg C)','Temperature (deg C)','Temperature (deg C)',
             'Temperature (deg C)', 'Precipitation (mm)', 'Precipitation (mm)',
             'Precipitation (mm)', 'Precipitation (mm)', 'Precipitation (mm)',
             'Precipitation (mm)','Precipitation (mm)','Precipitation (mm)')


plotlist <- lapply(1:length(vars), function(i) {
    summaryPlot(rasi, vars[i], xlab=xlabels[i], plotTitle=titles[i])
})

for (i in 1:length(vars)) {
    filename <- file.path(datapath, 'results/plots/summary', 
                         paste0(vars[i], '.png'))
    png(filename, width=600)
       print(plotlist[[i]])
    dev.off()
}



