
# Setup -------------------------------------------------------------------
library(raster)
library(caret)
library(dismo)
library(rJava)
library(dplyr)
library(DMwR)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))


#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
dropvars1 <- readRDS(file.path(datapath, 'processed/uselessVariables.RDS'))
dropvars2 <- readRDS(file.path(datapath, 'processed/uselessVariables2.RDS'))
dropvars3 <- readRDS(file.path(datapath, 'processed/uselessVariables3.RDS'))


rasi <- convertFactors(rasi)
rasi$rasi <- factor(rasi$rasi)



#subsetting data to be only some variables and no obs below 1524m (5000ft)
rasi1 <- rasi %>% 
    select(-dropvars1) %>% 
    filter(elevmax>=1524)

rasi2 <- rasi1 %>% select(-dropvars2)
rasi3 <- rasi2 %>% select(-dropvars3)

mod1 <- maxent(rasi1[,-1], rasi1$rasi, args=c("defaultprevalence=0.73"))
mod2 <- maxent(rasi2[,-1], rasi2$rasi, args=c("defaultprevalence=0.73"))
mod3 <- maxent(rasi3[,-1], rasi3$rasi, args=c("defaultprevalence=0.73",
                                              "responsecurves=true"))


# Cross-Validating Sans Smote -------------------------------------------------
thresholds <- seq(0.05, 0.95, by=0.05)

cvthresh1 <- t(sapply(thresholds, function(th) {
    crossval(rasi1, seed=928191, threshold=th, errormetric=c('PPV','TruePos'),
             arguments='defaultprevalence=0.73')
}))

cvdf1 <- data.frame(threshold=thresholds,
                    PPV=round(cvthresh1[,1],3),
                    TruePos=round(cvthresh1[,2]))


cvthresh2 <- t(sapply(thresholds, function(th) {
    crossval(rasi2, seed=928191, threshold=th, errormetric=c('PPV','TruePos'))
}))

cvdf2 <- data.frame(threshold=thresholds,
                    PPV=round(cvthresh2[,1],3),
                    TruePos=round(cvthresh2[,2]))


cvthresh3 <- t(sapply(thresholds, function(th) {
    crossval(rasi3, seed=928191, threshold=th, errormetric=c('PPV','TruePos'))
}))

cvdf3 <- data.frame(threshold=thresholds,
                    PPV=round(cvthresh3[,1],3),
                    TruePos=round(cvthresh3[,2]))


write.csv(cvdf1, file.path(datapath, 'results/CrossValidationModel51.csv'),
          row.names = FALSE)
write.csv(cvdf2, file.path(datapath, 'results/CrossValidationModel22.csv'),
          row.names = FALSE)
write.csv(cvdf3, file.path(datapath,'results/CrossValidationModel15.csv'),
          row.names=FALSE)

# Response Plots ----------------------------------------------

allvars22 <- names(rasi2)[-1]

for (v in allvars22) {
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/response22', filename))
    response(mod2, var=v)
    dev.off()
}

