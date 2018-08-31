
# Setup -------------------------------------------------------------------
library(dismo)
library(dplyr)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))


#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
dropvars <- readRDS(file.path(datapath, 'processed/uselessVariables.RDS'))
dropvarsC <- readRDS(file.path(datapath, 'processed/uselessVariablesCathy.RDS'))


rasi <- convertFactors(rasi)
rasi$rasi <- as.numeric(as.character(rasi$rasi))



#subsetting data to be only some variables 
rasi1 <- rasi %>% select(-dropvars) 
rasi1c <- rasi %>% select(-dropvarsC)

mod <- maxent(rasi1[,-1], rasi1$rasi, 
              args=c("defaultprevalence=0.73","responsecurves=true",
                     "replicates=10","lq2lqptthreshold=50"))

modC <- maxent(rasi1c[,-1], rasi1c$rasi,
               args=c("defaultprevalence=0.73","responsecurves=true",
                      "replicates=10","lq2lqptthreshold=50"))

saveRDS(mod, file.path(datapath, 'results/models/EliseModel.RDS'))
saveRDS(modC, file.path(datapath, 'results/models/CathyModel.RDS'))


# Cross-Validating -------------------------------------------------
thresholds <- seq(0.05, 0.95, by=0.05)

cvthresh1 <- t(sapply(thresholds, function(th) {
    crossval(rasi1, seed=928191, threshold=th, errormetric=c('PPV','TruePos'),
             arguments=c('defaultprevalence=0.73', 'lq2lqptthreshold=50'))
}))

cvdf1 <- data.frame(threshold=thresholds,
                    PPV=round(cvthresh1[,1],3),
                    TruePos=round(cvthresh1[,2]))


cvthreshC <- t(sapply(thresholds, function(th) {
    crossval(rasi1c, seed=928191, threshold=th, errormetric=c('PPV','TruePos'),
             arguments=c('defaultprevalence=0.73', 'lq2lqptthreshold=50'))
}))

cvdfC <- data.frame(threshold=thresholds,
                    PPV=round(cvthreshC[,1],3),
                    TruePos=round(cvthreshC[,2]))



write.csv(cvdf1, file.path(datapath, 'results/CrossValidationModelE.csv'),
          row.names = FALSE)
write.csv(cvdfC, file.path(datapath, 'results/CrossValidationModelC.csv'),
          row.names = FALSE)

# Response Plots ----------------------------------------------

allvars22 <- names(rasi2)[-1]

for (v in allvars22) {
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/response22', filename))
    response(mod2, var=v)
    dev.off()
}

