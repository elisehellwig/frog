stratifiedBootstrap <- function(data, reps, variable, seed=NA) {
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    presIDs <- which(data[,variable]==1)
    absIDs <- which(data[,variable]==0)
    
    presBS <- lapply(1:reps, function(i) {
        sample(presIDs, length(presIDs), replace=TRUE)
    })
    
    absBS <- lapply(1:reps, function(i) {
        sample(absIDs, length(absIDs), replace=TRUE)
    })
    
    
    datalist <- lapply(1:reps, function(i) {
        ids <- c(presBS[[i]], absBS[[i]])
        
        data[ids, ]
    })
    
    return(datalist)
}



meanMode <- function(v) {
    
    if (is.factor(v)) {
        lvls <- levels(v)
        mm <- lvls[which.max(tabulate(v))]
        
        
    } else if (identical(range(v), as.integer(c(0,1)))) {
        f <- as.factor(v)
        lvls <- levels(f)
        mm <- lvls[which.max(tabulate(f))]
    } else {
        mm <- mean(v)   
    }
    
    return(as.numeric(mm))
    
}



responseVals <- function(x, y, models, varname, nstep=10, confidence=NA) {
    
    varid <- which(names(x)==varname)
    var <- x[,varid]
    
    if (is.factor(var)) {
        varseq <- levels(var)
    } else {
        rvar <- range(var)
        varseq <- seq(rvar[1], rvar[2], length.out = nstep)
    }
    
    mu <- sapply(1:ncol(x), function(i) meanMode(x[,i]))
    muvec <- rep(mu, each=length(varseq))
    
    mumat <- matrix(muvec, ncol=ncol(x))
    
    mudat <- data.frame(mumat)
    names(mudat) <- names(x)
    
    mudat[,varid] <- varseq
    
    newdat <- convertFactors(mudat)
    
    if (length(models) > 1) {
        probs <- sapply(models, function(m) predict(m, x=newdat))
    } else {
        probs <- predict(models, x=newdat)
    }
    
    
    if (!is.na(confidence)) {
        ci1 <- ifelse(confidence>0.5, (1-confidence)/2, confidence/2) 
        ci2 <- 1-ci1
        
        probs2 <- t(sapply(1:nrow(probs), function(i) {
            quantile(probs[i,], c(ci1, ci2))
        }))
    } else {
        probs2 <- probs
    }
    
    curvedf <- data.frame(cbind(varseq, probs2))
    names(curvedf)[1] <- varname
    
    
    return(curvedf) 

}


responseCurve <- function(data, var, reps, nstep, seed=NA, conf=0.95) {
    
    originalmod <- maxent(data[,-1], data$rasi,
                          args=c("defaultprevalence=0.73",
                                 "lq2lqptthreshold=50"))
    
    sbs <- stratifiedBootstrap(data, reps, variable='rasi', seed=seed) 
    
    modlist <- lapply(sbs, function(d) {
        maxent(d[,-1], d$rasi, 
               args=c("defaultprevalence=0.73", "lq2lqptthreshold=50"))
    })
    
    df <- responseVals(data[,-1], data$rasi, originalmod, 
                       var, nstep=nstep)
    
    cidf <- responseVals(data[,-1], data$rasi, modlist, var, nstep=nstep,
                         confidence=conf)[,2:3]
    
    plotdat <- data.frame(cbind(df, cidf))
    
    names(plotdat) <- c('variable', 'response', 'lower','upper')
    
    for (i in 2:ncol(plotdat)) {
        plotdat[,i] <- as.numeric(as.character(plotdat[,i]))
    }
    
    if (is.factor(data[,var])) {
        rc <- ggplot(data=plotdat) + geom_bar(aes(x=variable, y=response), 
                                              stat='identity') +
            geom_errorbar(aes(x=variable, ymin=lower, ymax=upper), width=0.2)
        
    } else {
        rc <- ggplot(data=plotdat) + geom_line(aes(x=variable, y=response)) +
            geom_ribbon(aes(x=variable, ymax=upper, ymin=lower), alpha=0.4)
    }
    
    
    
    return(rc)
    
}

