summaryPlot <- function(df, var, groupvar='rasi', type='density', size=12,
                        colorpal=c('#a6cee3','#33a02c'), alph=0.5,
                        classes=c('Absent','Present'), plotTitle=NULL,
                        legend='R. Sierrae', width=NULL,
                        xlab='X', alignment=0.5) {
    
    if (!is.factor(df[,groupvar])) {
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
    
    splot <- ggplot(df)
    
    if (class=='categorical') {
        splot <- splot + geom_bar(aes_string(x=var, y='..prop..', fill=groupvar, 
                                             group=groupvar))
        
    } else if (type=='density') {
        
        splot <- splot + 
            geom_density(aes_string(x=var, fill=groupvar, group=groupvar),
                         alpha=alph) 
        
    } else {
        splot <- splot + geom_histogram(aes_string(x=var, fill=groupvar,
                                                   group=groupvar),
                                        position='identity', alpha=alph,
                                        binwidth = width)
    }

    splot <- splot + theme_bw(size) + 
        scale_fill_manual(values=colorpal, labels=classes, name=legend) +
        labs(x=xlab, y=ylab, title=plotTitle)
    
    if (maxchar>5) {
        splot <- splot + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  plot.title = element_text(hjust = alignment)) 
    } else {
        splot <- splot + theme(plot.title = element_text(hjust = alignment)) 
    }
    
    
    return(splot)    
}