
metric <- function(predicted, observed, type='accuracy') {
    tab <- table(predicted, observed)
    
    if (nrow(tab)==1) {
        tab <- rbind(tab, c(0,0))
    }
    
    total <- sum(tab)
    
    if (type=='accuracy') { #percent correct
        met <- (tab[1,1] + tab[2,2])/total
        
    } else if (type=='misclassification') { #percent wrong
        met <- (tab[2,1] + tab[1,2])/total
        
    } else if (type=='specificity') { #When it's actually no, how often does it 
        met <- tab[1,1]/sum(tab[,1])   #predict no? 
        
    } else if (type=='PPV') { #positive predictive value: when it is predicts
        met <- tab[2,2]/sum(tab[2,]) #yes, how often is it yes
        
        if (!is.finite(met)) met <- 0
    
    } else if (type=='NPV') {#negative predictive value: when it predicts no, 
        met <- tab[1,1]/sum(tab[1,]) # how often is it no
            
    } else if (type=='confusionMatrix') {    
        met <- tab
        names(attributes(tab)$dimnames) <- c('predicted','observed')
        
    } else {
        stop('Metric type given is not recognized.')
    }
    

    return(met)
}


crossval <- function(df, errormetric='PPV', k=5, seed=NA, avg=TRUE,
                     threshold=0.5, smote=FALSE) {
    require(dismo)
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    fold <- kfold(df, k=k)
    error <- rep(NA, k)
    
    for (i in 1:k) {
        
        test <- df[fold==i, ]
        train <- df[fold!=i, ]
        
        if (smote) {
            train <- smote(rasi ~ ., train)
        } 
        
        mod <- maxent(x=train[,-grep('rasi', names(train))],
                      p=train$rasi)
        
        testprediction <- predictPres(mod, test, threshold)
        
        error[i] <- metric(testprediction, 
                           test$rasi,
                           type=errormetric)
        
        
    } 
    
    if (avg) {
        return(mean(error))
    } else {
        return(error)
    }
}


FtoN <- function(v) {
    ch <- as.character(v)
    n <- as.numeric(ch)
    
    return(n)
}



