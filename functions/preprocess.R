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

recodeRange <- function(v, df, string=TRUE) {
    require(dplyr)
    
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
    
    for (i in 1:ncol(df)) {
        v[v<max[i] & v>min[i]] <- df$id[i]
    }
    
    if (string) {
        v <- as.character(v)
        df$chrID <- as.character(df$id)
        
        for (i in 1:ncol(df)) {
            v[v==df$chrID[i]] <- df$string[i]
        }
    }
    
    return(v)
    
    
}

