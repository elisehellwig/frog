library(raster)
library(rgdal)
library(randomForest)
library(dplyr)
library(dismo)
library(DMwR)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))

rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))


# Variable Selection ------------------------------------------------------

#rasi$rasi <- as.factor(rasi$rasi)

rasi5000 <- rasi[rasi$elevmax>1524, ]

fullvars <- c('rasi', 'length','elevmax', 'elevmin','slopemin', 
              'slopemax','slopemean',
              'totDrainArea', 'divDrainArea','south','perennial', 'x','y',
              'hard', paste0('bio', 1:19))


rasirf <- rasi5000[,fullvars]


set.seed(203943)
fold <- kfold(rasirf, 5, by=rasirf$rasi)
test <- rasirf[fold==1, ]
train <- rasirf[fold!=1, ]

mefull <- maxent(x=train[,-1], p=train$rasi)
fullpred <- predictPres(mefull, test, prob=0.7)

metric(fullpred, test$rasi, type='PPV')
metric(fullpred, test$rasi, type='NPV')


impOrd <- order(importance(rffull), decreasing=TRUE)
importance(rffull)[impOrd,]


vars1 <- c('rasi', 'y','x', 'length','divDrainArea','bio8','slopemin',
           'elevmin','bio13','bio6','bio11')
rasi1 <- rasirf[,vars1]
train1 <- rasi1[fold!=1, ]
test1 <- rasi1[fold==1,]

me1 <- maxent(x=train1[,-1], p=train1$rasi)
pred1 <- predictPres(me1, test1, prob=0.65)
metric(pred1, test1$rasi, type='PPV')



# SMOTE Maxent attempt --------------------------------------------------------

factorvars <- c('rasi','hard','south','perennial')
rasif <- rasirf

for (var in factorvars) {
    rasif[, var] <- as.factor(rasif[,var])
}

set.seed(192838)

test <- rasif[fold==1, ]
train <- rasif[fold!= 1,]
trainS <- SMOTE(rasi ~ ., data=rasif)


meS <- maxent(trainS[,-1], trainS$rasi)
predS <-  predictPres(meS, test, 0.6)
metric(predS, FtoN(test$rasi), type='PPV')
metric(predS, FtoN(test$rasi), type='confusionMatrix')



# Mild Variable Selection -------------------------------------------------

dropvars <- c('bio1', 'bio7', 'bio10','bio16','bio17','bio18','bio19')

rasi2 <- rasirf %>% select(-dropvars)

test2 <- rasi2[fold==1, ]
train2 <- rasi2[fold!=1, ]

me2 <- maxent(x=train2[,-1], p=train2$rasi)
pred2 <- predictPres(me2, test2, prob=0.7)
metric(pred2, test2$rasi, type='PPV')


# Adding Factors ----------------------------------------------------------

facvars <- c('rasi','length','elevmin','divDrainArea') 
facrasi <- rasi5000[, facvars]


mef <- maxent(facrasi[,-1], facrasi$rasi)


pa <- data.frame(alt=runif(100, 0, 1000),
                temp=rpois(100, 5),
                #aspect=sample(c('N','S','E','W'), 100, replace=TRUE),
                presence=sample(0:1, 100, replace=TRUE))

me <- maxent(x=pa[,-3], p=pa$presence)




