
# setup -------------------------------------------------------------------


library(dplyr)
library(ggplot2)
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))
source(file.path(funpath, 'plotting.R'))

r0 <- readRDS(file.path(datapath,'results/rasiplot.RDS'))


rasi0 <- read.csv(file.path(datapath,'results/RasiResultsDF.csv'))

vars <- read.csv(file.path(datapath, 'results/SelectedVariables.csv'))
vars <- as.character(vars$x)

rasi0 <- convertFactors(rasi0)
rasi0$rasi <- as.numeric(as.character(rasi0$rasi))

#subsetting data to be only some variables 
rasi <- rasi0 %>% 
    select(c(vars,'prob')) %>% 
    mutate(bio11 = bio11/10) %>% 
    mutate(predicted = ifelse(prob>0.7, 1, 0))


# Labels ------------------------------------------------------------------

titles <- c( 'Soil Type: Disturbed', 'BIO 11: Mean Temp of Coldest Quarter',
             'Canopy Closure: Dense', 'Habitat: Hardwood Forest',
             'Stream Reach Length', 'Nearby Meadows', 'Mean Slope', 
            'Modified Strahler Stream Order',
             'Total Upstream Cumulative Drainage Area',
            'Nearby Bodies of Water', 'Longitude')


xlabels <- c('Disturbed Soil', 'Temperature (deg C)', 'Dense Canopy',
             'Hardwood Forest','Length (km)', 'Number of Meadows','Slope (%)',
             'Stream Order','Sqrt Drainage Area km^2', 
             'Sqrt number of water bodies', 'Longitude (degrees)')

# Distributions -----------------------------------------------------------


plotlist <- lapply(1:(length(vars)-1), function(i) {
    print(vars[i+1])
    summaryPlot(rasi, vars[i+1], groupvar = 'predicted', alph=0.5,
                xlab=xlabels[i], plotTitle=titles[i])
})


# subsetting --------------------------------------------------------------

library(raster)
breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)
colors = c("green", "orange", "red", 'red4')

plot(r0, col=colors[cut(r0$prob, breaks)], lwd=1.5)

subset1 <- raster::select(r0, use='pol') #southeast1
subset2 <- raster::select(r0, use='pol') #northeast
subset3 <- raster::select(r0, use='pol') #northwest

subrasi1 <- rbind(subset1, subset2)
subrasi <- rbind(subrasi1, subset3)

sr <- data.frame(subrasi)
sr <- sr[,c(vars,'prob')]



# Subsetted Distributions -------------------------------------------------

sr <- convertFactors(sr)

plotlist <- lapply(1:(length(vars)-1), function(i) {
    print(vars[i+1])
    summaryPlot(sr, vars[i+1], groupvar = NA, alph=1,
                xlab=xlabels[i], plotTitle=titles[i])
})

