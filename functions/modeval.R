
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
        
    } else if (type=='TruePos') {
        met <- tab[2,2]
        
    } else if (type=='omission') {
        met <- tab[1,2]/sum(tab[1,])
        
    } else {
        stop('Metric type given is not recognized.')
    }
    

    return(met)
}


crossval <- function(df, errormetric='PPV', k=5, seed=NA, avg=TRUE,
                     threshold=0.5, smote=FALSE, arguments=NA) {
    require(dismo)
    
    print(threshold)
    n <- length(errormetric)
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    #print(1)
    fold <- kfold(df, k=k, by=df$rasi)
    error <- matrix(NA, nrow=k, ncol=n)
    
    for (i in 1:k) {
        
        test <- df[fold==i, ]
        train <- df[fold!=i, ]
        
        if (smote) {
            print(2)
            if (!is.na(arguments[1])) {
                train <- SMOTE(rasi ~ ., train, args=arguments)
            } else {
                train <- SMOTE(rasi ~ ., train)
            }
            
        } 
        
        #print(grep('rasi', names(train)))
        if (!is.na(arguments[1])) {
            mod <- maxent(x=train[,-grep('rasi', names(train))],
                          p=train$rasi, args=arguments) 
        } else {
            mod <- maxent(x=train[,-grep('rasi', names(train))],
                          p=train$rasi)
        }
       
        if (errormetric[1]=='COR') {
            
            testprob <- predict(mod, test)
            
            error[i,] <- cor(testprob, test$rasi)
            
        } else {
           
            testprediction <- predictPres(mod, test, threshold)
            
            
            
            error[i,] <- sapply(errormetric, function(met) {
                metric(testprediction, test$rasi,type=met)
            })
             
        }
        
    } 
    
    if (avg) {
        meanerror <- apply(error, 2, mean)
        return(meanerror)
    } else {
        return(error)
    }
}


FtoN <- function(v) {
    ch <- as.character(v)
    n <- as.numeric(ch)
    
    return(n)
}

extractResults <- function(mod, output, reps=NA) {
    
    if (!is.na(reps)) {
        allresults <- mod@results[,(reps+1)]
        allresultnames <- attributes(allresults)$names
        
    } else {
        allresults <- mod@results
        allresultnames <- attributes(allresults)$dimnames[[1]]
    }
    
    #print(head(allresults))
    
    
    #print(head(allresultnames))
    ids <- grep(output, allresultnames, ignore.case = TRUE)

    if (length(dim(allresults))==0) {
        resultvec <- allresults[ids]
    } else {
        resultvec <- allresults[ids, ncol(allresults)]
    }
    
    
    if (length(ids)==1) {
        result <- unname(resultvec)
        
    } else {
        
        resultnames <- allresultnames[ids]
        resultnames <- gsub(output, '', resultnames)
        result <- data.frame(variable=resultnames,
                             output=unname(resultvec))
    }
    
    return(result)
    
}


keepVariables <- function(model, imp) {
    
    contrib <- extractResults(model,'.contribution')
    perm <- extractResults(model,'.permutation.importance')
    
    keepIDs <- which(contrib$output>imp & perm$output>imp)
    
    keepvars <- as.character(perm$variable[keepIDs])
    
    return(keepvars)
    
}





