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
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDFrowreduced.csv'))


# Create model dataframe ------------------------------------------------------

#rasi$rasi <- as.factor(rasi$rasi)

rasi5000 <- rasi[rasi$elevmax>1524, ]

fullids <- c(3:43)
fullvars <- names(rasi5000)[fullids]

rasifull <- rasi5000[,fullvars]

#rasifull$perennial <- as.factor(rasifull$perennial)
rasifull$rasi <- as.factor(rasifull$rasi)


set.seed(203943)
fold <- kfold(rasifull, 5, by=rasifull$rasi)

sansrock <- rasifull %>% select(-'bedrock')
sansrockbinary <- expandFactors(sansrock, 'rasi')
sansrockf <- convertFactors(sansrockbinary, varnames= c('cardinal','soil',
                                                 'habitat','rocktype',
                                                 'seasonality'))

fulldf <- expandFactors(rasifull, 'rasi')
fulldf2 <- convertFactors(fulldf, varnames= c('cardinal','bedrock','soil',
                                              'habitat','rocktype',
                                              'seasonality'))
names(fulldf2)[61] <- 'waterrocktype'

write.csv(fulldf2, file.path(datapath, 'processed/RasiModelDF.csv'),
          row.names = FALSE)
write.csv(sansrockf, file.path(datapath, 'processed/RasiModelDFsansrock.csv'),
          row.names = FALSE )
fulldf2 <- read.csv(file.path(datapath, 'processed/RasiModelDF.csv'))

mefull <- maxent(x=fulldf2[,-1], p=fulldf2$rasi)
rockfull <- maxent(x=sansrockf[,-1], p=sansrockf$rasi)

set.seed(203943)
fold <- kfold(fulldf2, 5, by=fulldf2$rasi)

test <- fulldf2[fold==1, ]
train <- fulldf2[fold!=1, ]

trainmodfull <- maxent(x=train[,-1], p=train$rasi)

fullpred <- predictPres(trainmodfull, test, prob=0.5)

metric(fullpred, test$rasi, type='PPV')
metric(fullpred, test$rasi, type='NPV')



# First variable selection ---------------------------------------------------

contrib <- extractResults(mefull,'.contribution')
imp <- extractResults(mefull, '.permutation.importance')

contribrock <- extractResults(mefull,'.contribution')
improck  <- extractResults(mefull, '.permutation.importance')


removeIDs <- which(contrib$output==0 | imp$output==0)
drop1 <- as.character(imp$variable[removeIDs])
saveRDS(drop1, file.path(datapath, 'processed/uselessVariables.RDS'))

    
rasi1 <- fulldf2 %>% select(-drop1)

train1 <- rasi1[fold!=1, ]
test1 <- rasi1[fold==1,]

me1 <- maxent(x=rasi1[,-1], p=rasi1$rasi)

allvars1 <- names(rasi1)[-1]
allnames1 <- c( "ReachLength", "MaxElevation", "MinElevation", "MaxSlope", 
                "MeanSlope", "North", "SoutEast","streamOrder", 
                "JGeology", "mvGeology", "MzvGeology", "PzGeology",       
                "PzvGeology", "QGeology", "QgGeology", "QvGeology",       
                "umGeology", "Disturbed", "Entisols", "Mollisols",
                "Hardwood", "MixedForest", "NoVegetation", "Shrub",      
                "TreeSize", "totalDrainArea", "divDrainArea", "Longitude",
                "Latitude",  "Meadows",   "Waterbodies",  "Bio3",     
                "Bio6",      "Bio7",      "Bio8",      "Bio12",    
                "Bio14",     "Bio15",     "Intermittent", "Alfisols",  
                "DenseCover" )

for (i in 1:length(allvars1)) {
    print(allnames1[i])
    filename <- paste0(allnames1[i], '.png')
    png(file.path(datapath, 'results/plots/response', filename))
    response(mefull, var=allvars1[i])
    dev.off()
}

me1train <- maxent(x=train1[,-1], p=train1$rasi)

pred1 <- predictPres(me1train, test1, prob=0.65)
metric(pred1, test1$rasi, type='PPV')
metric(pred1, test1$rasi, type='confusionMatrix')


# Second variable selection -----------------------------------------------



contrib1 <- extractResults(me1,'.contribution')
imp1<- extractResults(me1, '.permutation.importance')


removeIDs1 <- which(contrib1$output<0.5 | imp1$output<0.5)
drop2 <- as.character(imp1$variable[removeIDs1])

saveRDS(drop2, file.path(datapath, 'processed/uselessVariables2.RDS'))



rasi2 <- rasi1 %>% select(-drop2)

me2 <- maxent(rasi2[,-1], rasi2$rasi)
drop3 <- as.character(imp2$variable[removeIDs2])

saveRDS(drop3, file.path(datapath, 'processed/uselessVariables3.RDS'))


# Third Variable selection ------------------------------------------------

contrib2 <- extractResults(me2,'.contribution')
imp2<- extractResults(me2, '.permutation.importance')

removeIDs2 <- which(contrib2$output<=1 | imp2$output<=1)



# SMOTE Maxent attempt --------------------------------------------------------
##Note maxent() only works with two level factors

set.seed(192838)

trainS <- SMOTE(rasi ~ ., data=train1)


meS <- maxent(trainS[,-1], trainS$rasi)
predS <-  predictPres(meS, test1, 0.6)
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


