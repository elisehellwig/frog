
# Setup -------------------------------------------------------------------
library(dismo)
library(dplyr)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))


#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rr <- read.csv(file.path(datapath,'processed/RasiResultsDF.csv'))
rasi <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
vars <- read.csv(file.path(datapath, 'results/SelectedVariables.csv'))
vars <- as.character(vars$x)

rasi <- convertFactors(rasi)
rasi$rasi <- as.numeric(as.character(rasi$rasi))



#subsetting data to be only some variables 
rasi1 <- rasi[,vars] 

rasi1$bio11 <- rasi1$bio11/10

# Running the model -------------------------------------------------------

outdir <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data/maxent'

mod <- maxent(rasi1[,-1], rasi1$rasi, 
              args=c("defaultprevalence=0.73", "writeplotdata=true",
                     "lq2lqptthreshold=50", "responsecurves=true"))

rr$prob <- predict(mod, rasi1)
rr$predicted <- predictPres(mod, rasi1, prob=0.7)

metric(rr$predicted, rr$rasi, type='PPV')
metric(rr$predicted, rr$rasi, type='omission')
metric(rr$predicted, rr$rasi, type='NPV')


saveRDS(mod, file.path(datapath, 'results/models/ModelFinal.RDS'))
write.csv(rr, file.path(datapath, 'results/RasiResultsDF.csv'),
          row.names = FALSE)


# plot prep ---------------------------------------------------------------

rs <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))

rs <- rs  %>% 
    filter(elevmax>=1524) %>% 
    select(c('id',vars))

rs <- rs[!(rs$id %in% c(8933706, 8933732, 8063647)),]



# Cross-Validating -------------------------------------------------
thresholds <- seq(0.05, 0.95, by=0.05)

crossval(rasi1, seed = 928191, errormetric='COR', 
         arguments=c('defaultprevalence=0.73', 'lq2lqptthreshold=50'))

cvthresh1 <- t(sapply(thresholds, function(th) {
    crossval(rasi1, seed=928191, threshold=th, 
             errormetric=c('PPV','TruePos','accuracy','omission'))
}))

cvdf1 <- data.frame(threshold=thresholds,
                    PPV=round(cvthresh1[,1],3),
                    TruePos=round(cvthresh1[,2]),
                    omission=round(cvthresh1[,4], 3))



write.csv(cvdf1, file.path(datapath, 'results/CrossValidationModelFinal.csv'),
          row.names = FALSE)



modbootstrap <- maxent(rasi1[,-1], rasi1$rasi, 
              args=c("defaultprevalence=0.73", "writeplotdata=true",
                     "lq2lqptthreshold=50", "responsecurves=true", 
                     "replicates=1000", "replicatetype=bootstrap",
                     "randomseed=true","threads=4"))


# Variable importance -----------------------------------------------------

mod <- maxent(rasi1[,-1], rasi1$rasi, 
              args=c("defaultprevalence=0.73", "writeplotdata=true",
                     "lq2lqptthreshold=50", "responsecurves=true",
                     "replicates=5"))

varimportance <- data.frame(contrib=extractResults(mod, '.contribution', 5),
                            vimp=extractResults(mod, '.variable.importance.',
                                                5))
