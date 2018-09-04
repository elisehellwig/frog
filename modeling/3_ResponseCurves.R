library(dplyr)
library(dismo)

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

rasilist <- stratifiedBootstrap(rasi, 100, 'rasi', seed=19293)

modlist <- lapply(rasilist, function(d) {
    maxent(d[,-1], d$rasi, 
           args=c("defaultprevalence=0.73", "lq2lqptthreshold=50"))
})

df <- responseCurve(rasi[,-1], rasi$rasi, modlist, 'waterbodies', 
                    confidence=0.95)




