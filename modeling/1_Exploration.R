library(raster)
library(rgdal)
library(randomForest)

datapath <- '/Users/echellwig/Research/frogData/data/'

#rasi <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))

vars <- names(rasi)

rasi$pa <- factor(rasi$rasi)


# Subset Absences ---------------------------------------------------------

pres <- which(rasi$rasi==1)
abse <- which(rasi$rasi==0)

set.seed(28303020)
subAbs <- sample(abse, 100)

subsetIDs <- c(pres, subAbs)

rasiSub <- rasi[subsetIDs, ]

# Variable Selection ------------------------------------------------------


rasiclim <- rasiSub[,c(18:98)]

rasiSubf <- rasiSub
rasiSubf$rasi <- factor(rasiSub$rasi)

set.seed(203943)
rasirff <- randomForest(rasi ~ ., data=rasiSubf[,3:97])

vars1 <- c(vars[3:17],'tmaxq1min','pptq4max','tmaxq2min')
rasi1 <- rasi[, vars1]

rf1 <- randomForest(rasi ~ ., data=rasi1)


# MaxEnt attempt ----------------------------------------------------------



