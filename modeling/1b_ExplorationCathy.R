library(dismo)
library(dplyr)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))

#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
rasi <- convertFactors(rasi)



# Remove Selected Vars ---------------------------------------------

BioIDs <- c(1,4,5,10,11,12,15,18,19)
dropBioIDs <- setdiff(1:19, BioIDs)
dropBios <- paste0('bio', dropBioIDs)

rasi <- rasi %>% select(-dropBios)



# Run Fullest model------------------------------------------------------

#rasi$rasi <- as.factor(rasi$rasi)

mefull <- maxent(x=rasi[,-1], p=rasi$rasi,
                 args=c("defaultprevalence=0.73","responsecurves=true",
                        "replicates=10","lq2lqptthreshold=50"))

set.seed(203943)
fold <- kfold(rasi, 5, by=rasi$rasi)

test <- rasi[fold==1, ]
train <- rasi[fold!=1, ]

trainmod2 <- maxent(x=train[,-1], p=train$rasi, 
                  args=c("defaultprevalence=0.73",
                         "lq2lqptthreshold=50"))

fullpred <- predictPres(trainmod2, test, prob=0.6)

metric(fullpred, test$rasi, type='PPV')
metric(fullpred, test$rasi, type='NPV')



# First variable selection ---------------------------------------------------

contrib <- extractResults(mefull,'.contribution')
imp <- extractResults(mefull, '.permutation.importance')

removeIDs <- which(contrib$output==0 | imp$output==0)
drop1 <- as.character(imp$variable[removeIDs])
    
rasi1 <- rasi %>% select(-drop1)

train1 <- rasi1[fold!=1, ]
test1 <- rasi1[fold==1,]

me1 <- maxent(x=rasi1[,-1], p=rasi1$rasi,
              args=c("defaultprevalence=0.73","responsecurves=true",
                     "replicates=10","lq2lqptthreshold=50"))


me1train <- maxent(x=train1[,-1], p=train1$rasi, 
                   args=c("defaultprevalence=0.73", 
                          "lq2lqptthreshold=50"))

pred1 <- predictPres(me1train, test1, prob=0.6)
metric(pred1, test1$rasi, type='PPV')
metric(pred1, test1$rasi, type='confusionMatrix')


# Second variable selection -----------------------------------------------



contrib1 <- extractResults(me1,'.contribution')
imp1<- extractResults(me1, '.permutation.importance')


removeIDs1 <- which(contrib1$output<0.5 | imp1$output<0.5)
drop2 <- as.character(imp1$variable[removeIDs1])


rasi2 <- rasi1 %>% select(-drop2)

me2 <- maxent(rasi2[,-1], rasi2$rasi, 
              args=c("defaultprevalence=0.73","responsecurves=true",
                     "replicates=10","lq2lqptthreshold=50"))


train2 <- rasi2[fold!=1, ]
test2 <- rasi2[fold==1,]


me2train <- maxent(x=train2[,-1], p=train2$rasi, 
                   args=c("defaultprevalence=0.73", 
                          "lq2lqptthreshold=50"))

pred2 <- predictPres(me2train, test2, prob=0.6)
metric(pred2, test2$rasi, type='PPV')
metric(pred2, test2$rasi, type='confusionMatrix')


# Third Variable selection ------------------------------------------------

contrib2 <- extractResults(me2,'.contribution')
imp2<- extractResults(me2, '.permutation.importance')

removeIDs2 <- which(contrib2$output<=1.5 | imp2$output<=1.5)

drop3 <- as.character(imp2$variable[removeIDs2])

rasi3 <- rasi2 %>% select(-drop3)

train3 <- rasi3[fold!=1, ]
test3 <- rasi3[fold==1,]


me3train <- maxent(x=train3[,-1], p=train3$rasi, 
                   args=c("defaultprevalence=0.73", 
                          "lq2lqptthreshold=50"))

pred3 <- predictPres(me3train, test3, prob=0.6)
metric(pred3, test3$rasi, type='PPV')
metric(pred3, test3$rasi, type='confusionMatrix')

me3 <-  maxent(x=rasi3[,-1], p=rasi3$rasi, 
               args=c("defaultprevalence=0.73", 'responsecurves=true',
                      "lq2lqptthreshold=50"))

rasi3 <- rasi %>% select(-alldropvars)

alldropvars <- Reduce(union, list(dropBios, drop1, drop2, drop3))
saveRDS(alldropvars, file.path(datapath, 'processed/uselessVariablesCathy.RDS'))


# Variable selection 4 ----------------------------------------------------


contrib3 <- extractResults(me3,'.contribution')
imp3<- extractResults(me3, '.permutation.importance')

removeIDs3 <- which(contrib3$output<=2 | imp3$output<=2)

drop4 <- as.character(imp3$variable[removeIDs3])

rasi4 <- rasi3 %>% select(-drop4)

train4 <- rasi4[fold!=1, ]
test4 <- rasi4[fold==1,]


me4train <- maxent(x=train4[,-1], p=train4$rasi, 
                   args=c("defaultprevalence=0.73", 
                          "lq2lqptthreshold=50"))
pred4 <- predictPres(me4train, test4, prob=0.6)
metric(pred4, test4$rasi, type='PPV')
metric(pred4, test4$rasi, type='confusionMatrix')


# some plots -------------------------------------------------
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

