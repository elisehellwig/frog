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


# kap <- function(observed, predicted) {
#     
#     totObs <- nrow(observed)
#     confused <- table(observed, predicted)
#     
#     expected1 <- (sum(confused[,2])*sum(confused[2,]))/totObs
#     expected0 <- (sum(confused[,1])*sum(confused[1,]))/totObs
#     
#     expectedCorrect <- (expected1 + expected0)/totObs
#     
#    
#     kappa <- 
#     
#     
#     
# }
# 
