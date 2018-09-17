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




isModFactor <- function(v, trueFactorsOnly=FALSE) {
    
    
    fctr <- FALSE
    
    
    if (class(v)=='SpatialLinesDataFrame') {
        v <- v@data[,1]
    }
    
    if (trueFactorsOnly) {
        if (is.factor(v)) {
            fctr <- TRUE
        } 
        
    } else { 
        
        #print(str(v))
        vals <- unique(v)
        dif <- setdiff(vals, c(0,1))
        
         if (is.factor(v) | length(dif)==0) {
            fctr <- TRUE
         }
    }
    
    return(fctr)
    
}


modFactors <- function(df, returnIDs=FALSE, trueFactorsOnly=FALSE) {
    
    isF <- sapply(1:ncol(df), function(i) {
        isModFactor(df[,i], trueFactorsOnly)
    })
    
    FIDS <- which(isF)
    
    Fnames <- names(df)[FIDS]
    
    if (returnIDs) {
        return(FIDS)
    } else {
        return(Fnames)
    }
    
}

expandFactors <- function(df, responsevar) {
    
    fmla <- as.formula(paste(responsevar, '~ .'))
    mat1 <- model.matrix(fmla, data=df)[,-1]
    df1 <- as.data.frame(mat1)
    df1 <- cbind(df[,responsevar], df1)
    names(df1)[1] <- 'rasi'
    
    
    if (is.factor(df[,responsevar])) {
        rvid <- which(names(df)==responsevar)
        factors <- modFactors(df, trueFactorsOnly = TRUE)[-rvid]
    } else {
        factors <- modFactors(df, trueFactorsOnly = TRUE)
    }
    
    
    firstlevels <- sapply(factors, function(v) {
        #print(class(df[,v]))
        levels(df[,v])[1]
    })
    
    #print(firstlevels)
    
    firstcols <- sapply(1:length(firstlevels), function(i) {
        vec <- df[, factors[i]]
        ifelse(vec==firstlevels[i], 1, 0)
    })
    
    df2 <- as.data.frame(firstcols)
    names(df2) <- firstlevels
    
    dffinal <- cbind(df1, df2)
    
    return(dffinal)
}


removeNames <- function(df, varnames) {
    
    for (name in varnames) {
        names(df) <- gsub(name, '', names(df))
    }
    
    return(df)
}



convertFactors <- function(df, exclude=NA, varnames=NA) {
    
    #print(str(df))
    factornames <- modFactors(df)
    
    if (!is.na(exclude[1])) {
        factornames <- setdiff(factornames, exclude)
    }
    
    
    if (class(df[,1])=='SpatialLinesDataFrame') {
        
        for (name in factornames) {
            df@data[, name] <- as.factor(df@data[,name])
        } 
        
    } else {
        for (name in factornames) {
            df[, name] <- as.factor(df[,name])
        }
    }
    
    
    
    if (!is.na(varnames[1])) {
        df <- removeNames(df, varnames)
        
    }
    
    
    
    return(df)
    
}

