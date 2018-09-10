
# Setup -------------------------------------------------------------------
library(dismo)
library(dplyr)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))


#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rr <- read.csv(file.path(datapath,'processed/RasiResultsDF.csv'))
rasi <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
dropvars <- readRDS(file.path(datapath, 'results/DroppedVariables.RDS'))


rasi <- convertFactors(rasi)
rasi$rasi <- as.numeric(as.character(rasi$rasi))



#subsetting data to be only some variables 
rasi1 <- rasi %>% select(-dropvars) 


# Running the model -------------------------------------------------------



mod <- maxent(rasi1[,-1], rasi1$rasi, 
              args=c("defaultprevalence=0.73","responsecurves=true",
                     "lq2lqptthreshold=50"))

rr$prob <- predict(mod, rasi1)
rr$predicted <- predictPres(mod, rasi1, prob=0.7)

saveRDS(mod, file.path(datapath, 'results/models/ModelFinal.RDS'))
write.csv(rr, file.path(datapath, 'results/RasiResultsDF.csv'),
          row.names = FALSE)


# Cross-Validating -------------------------------------------------
thresholds <- seq(0.05, 0.95, by=0.05)

cvthresh1 <- t(sapply(thresholds, function(th) {
    crossval(rasi1, seed=928191, threshold=th, errormetric=c('PPV','TruePos'),
             arguments=c('defaultprevalence=0.73', 'lq2lqptthreshold=50'))
}))

cvdf1 <- data.frame(threshold=thresholds,
                    PPV=round(cvthresh1[,1],3),
                    TruePos=round(cvthresh1[,2]))



write.csv(cvdf1, file.path(datapath, 'results/CrossValidationModelFinal.csv'),
          row.names = FALSE)
