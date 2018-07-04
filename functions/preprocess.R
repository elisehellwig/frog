isIdentical <- function(x, y) {
    
    if (length(x) != length(y)) {
        isID <- FALSE
    } else {
        checkpairs <- sapply(seq_along(x), function(i) {
            identical(x[i], y[i])
        })
        
        if (all(checkpairs)) {
            isID <- TRUE
        } else {
            isID <- FALSE
        }
    
    }
    
    return(isID)
}


getMode <- function(x) {
    
    if (all(is.na(x))) {
        mde <- NA
        
    } else if (length(x)==1) {
        mde <- x 
        
    } else {
        tab <- table(x)
        mdeID <- which.max(tab)
        mde <- attributes(tab)$dimnames[[1]][mdeID]
        
    }
    
    return(mde)
}


overChr <- function(x, y, variable) {
    require(rgeos)
    
    overlist <- over(x, y, returnList = TRUE)
    
    varlist <- sapply(overlist, function(ol) ol[,variable])
    
    most <- sapply(varlist, function(vl) getMode(vl))
    
    return(most)
}


recodeDF <- function(v, df, fromname='from',toname='to') {
    #note 
    
    vr <- v
    
    for (i in 1:nrow(df)) {
        vr[v==df[i, fromname]] <- df[i, toname]
    }
    
    return(vr)
    
}

recodeRange <- function(v, df, string=TRUE, digits=NA) {
    
    dfn <- names(df)
    reqnames <- c('min','max','id')
    if (string) {
        reqnames <- c(reqnames, 'string')
    }
    
    if (!all(reqnames %in% dfn)) {
        stop('Your df must have the columns: min, max, id (and string if string=TRUE).')
    }
    
    min <- df$min
    max <- df$max
    
    #print(class(v))
    if (!is.na(digits)) {
        vr <- round(v, digits)
        
    } else {
        vr <- v
    }
    
    for (i in 1:nrow(df)) {
        vr[v<=max[i] & v>=min[i]] <- df$id[i]
    }
    
    if (string) {
        vr <- as.character(vr)
        df$chrID <- as.character(df$id)
        
        for (i in 1:nrow(df)) {
            vr[vr==df$chrID[i]] <- df$string[i]
        }
    }
    
    return(vr)
    
    
}


recodeBlank <- function(x) {
    
    xsquished <- gsub(" ", "", x)
    
    blankIDs <- which(xsquished=='')
    x[blankIDs] <- NA

    return(x)
}


collapseVariable <- function(v, f, fun=mean, na.rm=NA) {
    
    if (!is.factor(f)) {
        f <- as.factor(f)
    }
    
    oneD <- tapply(v, f, FUN = fun)
    
    vec <- as.vector(unlist(oneD))
    
    return(vec)
    
}

