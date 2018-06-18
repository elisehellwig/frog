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



