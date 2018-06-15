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
