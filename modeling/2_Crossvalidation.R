
# Setup -------------------------------------------------------------------
library(raster)
library(caret)
library(dismo)
library(rJava)
library(dplyr)
library(DMwR)

datapath <- '/Users/echellwig/Research/frogData/data/'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))

rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
rasi <- read.csv(file.path(datapath,'processed/RasiStreamDF.csv'))
biorasi <- read.csv(file.path(datapath, 'processed/BioRasiDF.csv'))

#data is imbalanced at a ratio of 40:1

# Add Bioclim Data --------------------------------------------------------

bio <- getData(name='worldclim', var='bio', res=0.5, lon=-121, lat=39)
rasiSP <- spTransform(rasiSP, crs(bio))
#biorasi <- extract(bio, rasiSP, fun=mean, df=TRUE)
#names(biorasi)[2:20] <- gsub('_11', '', names(biorasi)[2:20])
#biorasi1 <- cbind(rasi$rasi, biorasi[,-1])
#names(biorasi1)[1] <- 'rasi'
#write.csv(biorasi1, file.path(datapath, 'processed/BioRasiDF.csv'),
     #     row.names = FALSE )

biorasi <- read.csv(file.path(datapath, 'processed/BioRasiDF.csv'))


#vars <- c('rasi', 'length', 'bedrock','totDrainArea','elevmax','tmaxq2min',
        #  'tmaxq1min', 'cardinal')
#rasiv$bedrock <- as.numeric(rasiv$bedrock)
#rasiv$cardinal <- as.numeric(rasiv$cardinal)

brscale <- biorasi

for (i in 2:20) {
    brscale[,i] <- scale(biorasi[,i])
    attributes(brscale[,i]) <- NULL
}

rf <- randomForest(rasi~., data=brscale)

# Run MaxEnt Via dismo ----------------------------------------------------
#just the bioclim variables

#rasisub<- subabs(biorasi, 'rasi', 100, 710398)
set.seed(1929838)
k <- kfold(biorasi, k=5, biorasi$rasi)
train <- biorasi[k!=1, ]
test <- biorasi[k==1, ]

probdist <- undersampleMaxent(train, test, n=200, reps=1000, seed=1938372)
pd <- data.frame(probdist[,-1])
pd$id <- 1:nrow(pd)
pd$pa <- probdist[,1]

pdm <- melt(probdist, id.vars=c('id','pa'), value.name = 'prob')[,-3]
pdm$pa <- as.factor(pdm$pa)

histp <- ggplot(data=pdm) + geom_density(aes(x=prob, group=id, fill=pa), alpha=0.3)

minp <- apply(probdist[,1:1000], 1, mean)
probp <- ifelse(minp>0.5, 1, 0)
table(probp, probdist$pa)

cbind(probdist[,'pa'], minp)

set.seed(199384)
k <- dismo::kfold(rasisub, k=5)
test <- rasisub[k==1, ]
train <- rasisub[k!=1, ]

teras <- test$rasi
med1 <- maxent(x=train[,-1], train$rasi)
med1pa <- predictPres(med1, test, 0.5)

table(med1pa, teras)

metric(med1pa, teras, 'accuracy')
metric(med1pa, teras, 'specificity')
metric(med1pa, teras, 'PPV')
metric(med1pa, teras, 'NPV')


# Model All Rasi Vars -----------------------------------------------------

rasiall <- cbind(biorasi, rasi[, c(4:21) ])
rasiall$seasonality <- factor(rasiall$seasonality)

factorvars <- c('cardinal','seasonality','bedrock','soil','habitat')
for (var in factorvars) {
    rasiall[,var] <- as.numeric(rasiall[,var])
}

sub300 <- subabs(rasiall, 'rasi', 100, seed=92910, IDs=TRUE)
rasi1sub <- rasiall[sub300, ]
set.seed(2389320)
k300 <- kfold(rasi1sub, k=5)
test <- rasi1sub[k300==1, ]
train <- rasi1sub[k300!=1, ]


mod1 <- maxent(x=train[,-1], train$rasi, factors=factorvars)
pred1 <- predictPres(mod1, test, 0.5)
pt1 <- predictPres(mod1, train, 0.5)
metric(pred1, test$rasi, 'PPV')

predtrain <- predictPres(mod1, train, prob=0.5)

metric(predtrain, train$rasi, 'PPV')

#Mild Subset Model -----------------------------------------------------

#bio1, bio9
biovars <- c('bio1', 'bio5', 'bio8', 'bio13')
othervars <- c('length','x','y', 'totDrainArea', 'slopemean', 'south',
               'habitat')
rasivars <- unlist(c('rasi', biovars, othervars))

rasi2 <- rasiall[, rasivars] 
rasi2sub <- subabs(rasi2, 'rasi', 100, seed=29382)

set.seed(2389320)
k300 <- kfold(rasi2sub, k=5)
test <- rasi2sub[k300==1, ]
train <- rasi2sub[k300!=1, ]

mod2 <- maxent(x=train[,-1], train$rasi, factors='habitat')
pred2 <- predictPres(mod2, test, prob=0.5)
metric(pred2, test$rasi, 'PPV')
metric(pred2, test$rasi, 'confusionMatrix')

pt2 <- predictPres(mod2, train, prob=0.5)
metric(pt2, train$rasi, 'PPV')
