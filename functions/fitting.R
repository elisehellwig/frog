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
    
    mods <- lapply(1:length(trainsub), function(i) {
        print(i)
        maxent(trainsub[[i]][,-responseID], trainsub[[i]][,colname])
    })
    
    probs <- data.frame(sapply(mods, function(m) {
        predictPres(m, test)
    }))
    
    probs$id <- 1:nrow(probs)
    probs$pa <- test[,colname]
    
    return(probs)
}


