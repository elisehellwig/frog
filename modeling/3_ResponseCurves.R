library(raster)
library(dismo)
library(plyr)
library(dplyr)
library(ggplot2)
library(standardize)
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))
source(file.path(funpath, 'response.R'))



#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi0 <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
r0 <- readRDS(file.path(datapath,'results/rasiplot.RDS'))
vars <- read.csv(file.path(datapath, 'results/SelectedVariables.csv'))
vars <- as.character(vars$x)

rr <- r0[, vars] 

rasi0 <- convertFactors(rasi0, exclude='rasi')
r <- convertFactors(rr, exclude='rasi')

#subsetting data to be only some variables 
rasi <- rasi0 %>% select(vars) 


# Response curves ---------------------------------------------------------

# numvars <- vars[c(3,6:12)]
# chrvars <- vars[c(2,4,5)]
# 
# rcsNumeric <- ldply(numvars, function(var) {
#     d <- responseCurve(rasi, var, 50, 30, 23882929, plot=FALSE)
#     d$variable <- var
#     d
# })
# 
# write.csv(rcsNumeric, file.path(datapath, 'results/responseNum.csv'),
#           row.names = FALSE)
# 
# rcsChar <- ldply(chrvars, function(var) {
#     d <- responseCurve(rasi, var, 50, 30, 23882929, plot=FALSE)
#     d$variable <- var
#     d
# })
# 
# write.csv(rcsChar, file.path(datapath, 'results/responseChr.csv'),
#           row.names = FALSE)


la <- responseCurve(rasi, 'waterbodies', 1000, 30, 23882929)


g <- ggplot(data=la) +
    geom_line(aes(x=value, y=response)) +
    geom_ribbon(aes(x=value, ymax=upper, ymin=lower), alpha=0.4)

rn <- read.csv( file.path(datapath, 'results/responseNum.csv'))
rc <- read.csv( file.path(datapath, 'results/responseChr.csv'))

rn$valueScaled <- as.numeric(scale_by(value ~ variable, data=rn))

nplot <- ggplot(data=rn) + 
    geom_line(aes(x=valueScaled, y=response)) +
    geom_ribbon(aes(x=valueScaled, ymax=upper, ymin=lower), alpha=0.4) +
    facet_wrap( ~ variable)



rcs$value <- as.numeric(rcs$value)

v <- responseCurve(rasi, 'waterbodies', 10, 10, 23882929, plot=FALSE)

p <- responseCurve(rasi, 'waterbodies', 100, 30, 23882929)

pm <- p + labs(x='Waterbodies', y='Predicted Presence Probability') + 
    theme_bw() + scale_y_continuous(limits=c(0,1))


allvars <- names(rasi[,-1])

for (v in allvars) {
    print(v)
    p <- responseCurve(rasi, v, 100, 30, 8752)
    pm <- p + labs(x=v, y='Predicted Presence Probability') + 
        theme_bw() + scale_y_continuous(limits=c(0,1))
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/newresponse', filename))
    print(pm)
    dev.off()
}
