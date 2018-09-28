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
        ids <- unlist(c(presBS[[i]], absBS[[i]]))
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



responseVals <- function(model, varname, nstep=10, confidence=NA) {
    
    
    
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
        
        probs1 <- t(sapply(1:nrow(probs), function(i) {
            quantile(probs[i,], c(ci1, ci2))
        }))
        
        probs2 <- cbind(probs, probs1)
    } else {
        probs2 <- probs
    }
    
    curvedf <- data.frame(cbind(varseq, probs2))
    names(curvedf)[1] <- varname
    
    
    return(curvedf) 

}


responseCurve <- function(data, var, reps, nstep, seed=NA, conf=0.95,
                          plot=TRUE) {
    
    print(1)
    originalmod <- maxent(data[,-1], data$rasi,
                          args=c("defaultprevalence=0.73",
                                 "lq2lqptthreshold=50"))
    
    print(2)
    sbs <- stratifiedBootstrap(data, reps, variable='rasi', seed=seed) 
    
    
    print(3)
    modlist <- lapply(sbs, function(d) {
        maxent(d[,-1], d$rasi, 
               args=c("defaultprevalence=0.73", "lq2lqptthreshold=50"))
    })
    
    cidf <- lapply(modlist, function(m) {
        d <- data.frame(unique(response(m, var, expand=0)))
        d
    })
    
    print(4)
    df <- data.frame(response(originalmod, var))
    
    print(5)
    
    plotdat <- data.frame(cbind(df, cidf))
    
    names(plotdat) <- c('value', 'response', 'lower','upper')
    
    
    print(6)
    for (i in 2:ncol(plotdat)) {
        plotdat[,i] <- as.numeric(as.character(plotdat[,i]))
    }
    
    print(7)
    if (plot) {
        if (is.factor(data[,var])) {
            rc <- ggplot(data=plotdat) + 
                geom_bar(aes(x=value, y=response), stat='identity') +
                geom_errorbar(aes(x=value, ymin=lower, ymax=upper), 
                              width=0.2) +
                ylim(low=0, high=1)
            
        } else {
            rc <- ggplot(data=plotdat) + 
                geom_line(aes(x=value, y=response)) +
                geom_ribbon(aes(x=value, ymax=upper, ymin=lower), alpha=0.4) +
                ylim(low=0, high=1)
        }
        
    } else {
        rc <- plotdat
    }
    
    print(8)
    
    
    return(rc)
    
}


bootCI <- function(df, confidence=0.95, varname='variable') {
    
    print(varname)
    
    lc <- (1-confidence)/2
    uc <- (1-confidence)/2 + confidence
    
    bins <- sort(unique(df[,'x']))
    #print(bins)
    
    
    dists <- lapply(bins, function(n) {
        df[df$x==n,'y']
    })
    
    CIs <- data.frame(variable=varname,
                      value=bins,
                      lower=sapply(dists, function(d) quantile(d, probs=lc)),
                      upper=sapply(dists, function(d) quantile(d, probs=uc)))
    
    
}




