library(raster)
library(rgdal)
library(randomForest)
library(plyr)
library(dplyr)
library(dismo)
library(DMwR)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))

#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))


# Variable Selection ------------------------------------------------------

#rasi$rasi <- as.factor(rasi$rasi)

rasi5000 <- rasi[rasi$elevmax>1524, ]

fullids <- c(3:23, 104:122)
fullvars <- names(rasi5000)[fullids]

rasifull <- rasi5000[,fullvars]

rasifull$seasonality <- NULL
rasifull$south <- NULL
rasifull$hard <- NULL
#rasifull$perennial <- as.factor(rasifull$perennial)
rasifull$rasi <- as.factor(rasifull$rasi)


set.seed(203943)
fold <- kfold(rasifull, 5, by=rasifull$rasi)
fulldf <- expandFactors(rasifull, 'rasi')
fulldf2 <- convertFactors(fulldf, varnames= c('cardinal','bedrock','soil',
                                                 'habitat','rocktype'))
names(fulldf2)[56] <- 'waterrocktype'

test <- fulldf2[fold==1, ]
train <- fulldf2[fold!=1, ]

mefull <- maxent(x=fulldf2[,-1], p=fulldf2$rasi)

allvars <- names(fulldf2)[-1]

for (v in allvars) {
    print(v)
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/response', filename))
        response(mefull, var=v)
    dev.off()
}


fullpred <- predictPres(mefull, test, prob=0.5)

metric(fullpred, test$rasi, type='PPV')
metric(fullpred, test$rasi, type='NPV')


drop1 <- c('NW', 'NE','bio17','bio13', 'bio18','bio11','bio10', 'bio16',
           'bio1','bio9','SW','CON','Andisols', 'W','S','NE','waterrocktype',
           'igneous','metamorphic', 'sedimentary','elevmax','NoSoil', 'bio19',
           'divDrainArea', 'J','S','Tr', 'Qoa','Pm','Ti','gb', 'm','grMz',
           'Ec','C','SO', 'waterrocktype')
rasi1 <- fulldf2 %>% select(-drop1)

rasi1 <- rasirf[,vars1]
train1 <- rasi1[fold!=1, ]
test1 <- rasi1[fold==1,]

me1 <- maxent(x=rasi1[,-1], p=rasi1$rasi)

allvars1 <- names(rasi1)[-1]

for (v in allvars1) {
    print(v)
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/response', filename))
    response(mefull, var=v)
    dev.off()
}


pred1 <- predictPres(me1, test1, prob=0.65)
metric(pred1, test1$rasi, type='PPV')



# SMOTE Maxent attempt --------------------------------------------------------
##Note maxent() only works with two level factors

fids <- which(sapply(1:ncol(rasirf), function(i) (!is.numeric(rasirf[, i]))))
rasif <- rasirf

for (i in fids) {
    rasif[, i] <- as.factor(rasif[,i])
}

rasif[,'south'] <- as.factor(rasif[,'south'])

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




