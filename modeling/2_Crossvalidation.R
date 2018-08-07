
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


rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))

#data is imbalanced at a ratio of 40:1

vars1 <- c('rasi', 'length','elevmax', 'elevmin','slopemin', 'slopemax', 
           'slopemean','totDrainArea', 'divDrainArea', 'south','perennial',
           'x','y', 'hard', paste0('bio', 1:19))
dropvars <- c('bio1', 'bio7', 'bio10','bio16','bio17','bio18','bio19')


varsla <- c('rasi', 'length','elevmax', 'elevmin','slopemin', 'slopemax', 
            'slopemean','totDrainArea', 'divDrainArea', 'south','perennial',
            'x','y', 'hard', paste0('bio', 1:19), 'rocktype')



#subsetting data to be only some variables and no obs below 1524m (5000ft)
rasi1 <- rasi[rasi$elevmax>=1524, vars1] 
rasi2 <- rasi1 %>% select(-dropvars)
rasila <- rasi[rasi$elevmax>=1524, varsla] 
rasila$rocktype <- factor(rasila$rocktype)

rasi1$rasi <- factor(rasi1$rasi)
rasi2$rasi <- factor(rasi2$rasi)

la2 <- maxent(rasi2[,-1], rasi2$rasi, args=c("defaultprevalence=0.73"))


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

# Crossvalidating with SMOTE ----------------------------------------------



cvthreshsmote1 <- t(sapply(thresholds, function(th) {
    crossval(rasi1, seed=928191, threshold=th, errormetric=c('PPV','TruePos'),
             smote=TRUE)
}))

cvdfsmote1 <- data.frame(threshold=thresholds,
                    PPV=round(cvthreshsmote1[,1],3),
                    TruePos=round(cvthreshsmote1[,2]))
