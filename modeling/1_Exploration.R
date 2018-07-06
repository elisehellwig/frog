library(raster)
library(rgdal)
library(maxent)
library(randomForest)

datapath <- '/Users/echellwig/Research/frogData/data/'

#rasi <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))

vars <- names(rasi)


# Variable Selection ------------------------------------------------------



rasiclim <- rasi[,c(3,18:97)]

rasirf <- randomForest(rasi ~ ., data=rasiclim)
regimp <- importance(rasirf)

regvarIDs <- which(importance(rasirf)>1.5) 
regvars <- row.names(importance(rasirf))[regvarIDs]

round(cor(rasi[,regvars]),2)

rasiclimf <- rasiclim
rasiclimf$rasi <- factor(rasiclim$rasi)

rasirff <- randomForest(rasi ~ ., data=rasiclimf)

vars1 <- c(vars[3:17],'pptq4min','pptq4end','tminq2lwm')
rasi1 <- rasi[, vars1]

rf1 <- randomForest(rasi ~ ., data=rasi1)


# MaxEnt attempt ----------------------------------------------------------



