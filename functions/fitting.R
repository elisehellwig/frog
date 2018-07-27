subabs <- function(df, colname, n=100, seed=NA, IDs=FALSE) {
    
    pres <- which(df[,colname]==1)
    abse <- which(df[,colname]==0)
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    subabse <- sample(abse, n, replace=FALSE)
    
    allobs <- c(pres, subabse)
    
    if (IDs) {
        return(allobs)
    } else {
        return(df[allobs, ])
    }
    
}


predictPres <- function(mod, data, prob=NA) {
    
    predprob <- predict(mod, x=data)
    
    if (is.na(prob)) {
        predPA <- predprob
        
    } else {
        predPA <- ifelse(predprob>prob, 1, 0)
        
    }
    
    return(predPA)
}


TrainTestMod <- function(data, colname, testIDs, returnMod=FALSE) {

    responseID <- which(names(data)==colname)
    #print(1)
    test <- data[testIDs, ]
    train <- data[-testIDs, ]
    #print(2)
    mod <- maxent(data[,-responseID], data[,colname])
    #print(3)
    if (returnMod) {
        print('bad')
        return(mod)
        
    } else {
        #print(4)
        probs <- predictPres(mod, test)
        return(probs)
    } 
    
}


undersampleMod <- function(data, n, reps, seed=NA, test=0.2, colname='rasi',
                           multicore=6L) {
    require(dismo)
    require(parallel)
    
    m <- n + length(which(data[,colname]==1))
    
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    datasub <- lapply(1:reps, function(i) {
        subabs(data, colname, n)
    })
    
    testIDs <- sample(1:m, size=round(m*test))
    
    probs <- sapply(1:length(datasub), function(i) {
        print(i)
        TrainTestMod(datasub[[i]], colname, testIDs)
    })
    
    probs2 <- cbind(datasub[[1]][testIDs, colname], probs)
    
    
    return(probs2)
}



undersampleMaxent <- function(train, test, n, reps, seed=NA, colname='rasi') {
    require(dismo)

    responseID <- which(names(train)==colname)
    m <- n + length(which(train[,colname]==1))
    
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    trainsub <- lapply(1:reps, function(i) {
        subabs(train, colname, n)
    })
    
    mods <- lapply(trainsub, function(df) {
        print(i)
        maxent(df[,-responseID], df[,colname])
    })
    
    probs <- data.frame(sapply(mods, function(m) {
        predictPres(m, test)
    }))
    
    probs$id <- 1:nrow(probs)
    probs$pa <- test[,colname]
    
    return(probs)
}


