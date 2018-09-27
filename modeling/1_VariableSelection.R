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
rasi <- rasi %>% select(-c(dropBios))


#rasip <- rasi[rasi$perennial=='1', ]
#rasi <- rasi %>% select(c(1, 32:39, 2:31, 40:50))

# Run Fullest model------------------------------------------------------

#rasi$rasi <- as.factor(rasi$rasi)

mefull <- maxent(x=rasi[,-1], p=rasi$rasi,
                 args=c("defaultprevalence=0.73","responsecurves=true",
                        "replicates=10","lq2lqptthreshold=50"))


mefullcv <- crossval(rasi, seed=203943, threshold=0.6,
                     arguments=c("defaultprevalence=0.73", 
                                 "lq2lqptthreshold=50"))

fullresults <- mefull@results[,11]

contrib <- extractResults(mefull,'.contribution', 10)
varimp <- extractResults(mefull,'.permutation.importance',10)

# First variable selection ---------------------------------------------------

keep1 <- keepVariables(mefull, 0)
 
rasi1 <- rasi %>% select(c('rasi', keep1))

me1 <- maxent(x=rasi1[,-1], p=rasi1$rasi,
              args=c("defaultprevalence=0.73","responsecurves=true",
                     "replicates=10","lq2lqptthreshold=50"))


me1cv <- crossval(rasi1, seed=203943, threshold=0.6,
                     arguments=c("defaultprevalence=0.73", 
                                 "lq2lqptthreshold=50"))


# Second variable selection -----------------------------------------------




keep2 <- keepVariables(me1, 0.5)


rasi2 <- rasi1 %>% select(c('rasi', keep2))

me2 <- maxent(rasi2[,-1], rasi2$rasi, 
              args=c("defaultprevalence=0.73","responsecurves=true",
                     "replicates=10","lq2lqptthreshold=50",
                     "extrapolate=false"))


me2cv <- crossval(rasi2, seed=203943, threshold=0.6,
                  arguments=c("defaultprevalence=0.73", 
                              "lq2lqptthreshold=50"))


# Third Variable selection ------------------------------------------------

keep3 <- keepVariables(me2, 1.5)

rasi3 <- rasi2 %>% select(c('rasi', keep3))

me3 <-  maxent(x=rasi3[,-1], p=rasi3$rasi, 
               args=c("defaultprevalence=0.73", "replicates=10", 
                      "lq2lqptthreshold=50"))


me3cv <- crossval(rasi3, seed=203943, threshold=0.6,
                        arguments=c("defaultprevalence=0.73", 
                                    "lq2lqptthreshold=50"))


chosenvars <- c('rasi', keep3)
write.csv(chosenvars, file.path(datapath, 'results/SelectedVariables.csv'),
          row.names = FALSE)

write.csv(rasi3, file.path(datapath, 'results/RasiResultsDF.csv'),
          row.names=FALSE)

# Variable selection 4 ----------------------------------------------------


keep4 <- keepVariables(me3, 2)

rasi4 <- rasi3 %>% select(c('rasi', keep4))

me4cv <- crossval(rasi4, seed=203943, threshold=0.6,
                  arguments=c("defaultprevalence=0.73", 
                              "lq2lqptthreshold=50"))

me4 <-  maxent(x=rasi4[,-1], p=rasi4$rasi, 
               args=c("defaultprevalence=0.73", "replicates=10", 
                      "lq2lqptthreshold=50", "responsecurves=true"))


