summaryPlot <- function(df, var, groupvar='rasi', type='density', size=12,
                        colorpal=c('#a6cee3','#33a02c'), alph=0.5,
                        classes=c('Absent','Present'), plotTitle=NULL,
                        legend='R. Sierrae', width=NULL,
                        xlab='X', alignment=0.5) {
    
    if (is.na(groupvar)) {
        df$gv <- 1
        df$gv <- as.factor(df$gv)
                
    } else if (!is.factor(df[,groupvar])) {
        df[,groupvar] <- factor(df[,groupvar])
    }

    n <- length(unique(df[,var]))
    
    if (n==2) {
        df[,var] <- factor(df[,var])
    }
    
    if (is.numeric(df[,var])) {
        class <- 'numeric'
        maxchar <- 0
        ylab <- 'Density'
    } else {
        class <- 'categorical'
        char <- nchar(as.character(df[,var]))
        maxchar <- max(char)
        ylab <- 'Proportion'
    }
    
    df$var <- df[, var]
    
    splot <- ggplot(df)
    
    if (class=='categorical') {
        
        if (is.na(groupvar)) {
            splot <- splot + geom_bar(aes(x=var, (..count..)/sum(..count..)))
            
        } else {
            splot <- splot + geom_bar(aes_string(x=var, '..prop..', 
                                                 fill=groupvar, 
                                                 group=groupvar))
        }
        
        
    } else if (type=='density') {
        
        if (is.na(groupvar)) {
            splot <- splot + 
                geom_density(aes_string(x=var), alpha=alph) 
            
        } else {
            splot <- splot + 
                geom_density(aes_string(x=var, fill=groupvar, group=groupvar),
                             alpha=alph) 
        }
    
        
    } else {
        
        if (is.na(groupvar)) {
            splot <- splot + geom_histogram(aes_string(x=var),
                                            position='identity', alpha=alph,
                                            binwidth = width)
        } else {
            splot <- splot + geom_histogram(aes_string(x=var, fill=groupvar,
                                                       group=groupvar),
                                            position='identity', alpha=alph,
                                            binwidth = width)
        }
        
    }

    if (!is.na(groupvar)) {
        splot <- splot + scale_fill_manual(values=colorpal, labels=classes, 
                                           name=legend)
    }
    
    splot <- splot + theme_bw(size) + labs(x=xlab, y=ylab, title=plotTitle)
    
    if (maxchar>5) {
        splot <- splot + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(hjust = alignment)) 
    } else {
        splot <- splot + theme(plot.title = element_text(hjust = alignment)) 
    }
    
    
    return(splot)    
}


summarizePredictor <- function(predictor, response, name='predictor', 
                               digits=3) {

    summarylist <- tapply(predictor, response, summary)
    sumdf <- as.data.frame(round(do.call(cbind, summarylist), digits))
    respnames <- sort(unique(response))
    names(sumdf) <- paste0(name, respnames)

    return(sumdf)    
}



summaryTable <- function(x, response, digits=3, percent=TRUE) {
    require(reshape2)
    #print(class(x))
    
    Stat <- c('Min', '1st Q ', 'Median','Mean','3rd Q', 'Max')
    
    if (is.factor(x) | is.character(x)) {
        
        tab1 <-data.frame(table(x, response))
        names(tab1) <- c('predictorVal','rasi', 'freq')
        tab <- dcast(tab1, predictorVal ~ rasi, value.var = 'freq')
        names(tab) <- c('Value','Unoccupied','Ooccupied')
        
        if (percent) {
            
            tab[,2] <- round(tab[,2]/sum(tab[,2]), digits)
            tab[,3] <- round(tab[,3]/sum(tab[,3]), digits)
        }
        
    } else if (is.data.frame(x)) {
        
        if (length(digits)==1) {
            digits <- rep(digits, ncol(x))
        }
        
        pnames <- names(x)
        tablist <- lapply(1:ncol(x), function(i) {
            summarizePredictor(x[,i], response, pnames[i], digits=digits[i])
        })
        
        tab <- do.call(cbind, tablist)
        tab <- cbind(Stat, tab)
        rownames(tab) <- NULL
        
        
    } else {
        
        tab <- summarizePredictor(x, response, digits=digits)
        names(tab) <- c('Unoccupied','Occupied')
        tab <- cbind(Stat, tab)
        rownames(tab) <- NULL
    }
    
    
    return(tab)
    
} 




