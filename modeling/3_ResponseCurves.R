library(dismo)
library(dplyr)
library(ggplot2)
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))
source(file.path(funpath, 'response.R'))



#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi0 <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
dropvars <- readRDS(file.path(datapath, 'processed/uselessVariables.RDS'))
dropvarsC <- readRDS(file.path(datapath, 'processed/uselessVariablesCathy.RDS'))


rasi0 <- convertFactors(rasi0)
rasi0$rasi <- as.numeric(as.character(rasi0$rasi))


#subsetting data to be only some variables 
rasi <- rasi0 %>% select(-dropvars) 


# Response curves ---------------------------------------------------------


p <- responseCurve(rasi, 'waterbodies', 100, 30, 8752)

pm <- p + labs(x='Waterbodies', y='Predicted Presence Probability') + 
    theme_bw() + scale_y_continuous(limits=c(0,1))


allvars <- names(rasi[,-1])

for (v in allvars) {
    print(v)
    p <- responseCurve(rasi, v, 10, 50, 8752)
    pm <- p + labs(x=v, y='Predicted Presence Probability') + 
        theme_bw() + scale_y_continuous(limits=c(0,1))
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/newresponse', filename))
    print(pm)
    dev.off()
}
