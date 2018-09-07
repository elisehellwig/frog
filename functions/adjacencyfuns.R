minSteps <- function(graph, pt, present) {
    
    paths <- get.shortest.paths(graph, pt)[[1]]
    
    pathlengths <- sapply(paths, length)
    
    preslengths <- pathlengths[present]
    
    steps <- preslengths[preslengths>1]
    
    if (length(steps)==0) {
        nstep <- 0
    } else {
        nstep <- min(steps)-1
    }
    
    return(nstep)
    
}