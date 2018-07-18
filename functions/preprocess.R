#These are the functions I use to preprocess frog data

# This function checks if two vectors are identical, basically vectorized 
#version of identical()
isIdentical <- function(x, y) {
    #x and y are both vectors to be checked to see if they are identical
    
    if (length(x) != length(y)) { #if they are not the same length they
        isID <- FALSE             #can't be identical
        
    } else { #if they are the same length
        
        #check each pair indivdiually with identical
        checkpairs <- sapply(seq_along(x), function(i) {
            identical(x[i], y[i])
        })
        
        if (all(checkpairs)) { #if all are individually identical then 
            isID <- TRUE       #then the vectors are identical
        } else {
            isID <- FALSE #otherwise they are not
        }
    
    }
    
    #return whether or not two vectors are identical
    return(isID)
}


#This function extracts the mode from a vector of values
getMode <- function(x) {
    
    
    if (all(is.na(x))) { #if all values are NA
        mde <- NA #then the mode is NA
        
    } else if (length(x)==1) { #otherwise, if only one element, that is mode
        mde <- x 
        
    } else { #if there is more than one element...
        
        tab <- table(x) #count number of each value
        mdeID <- which.max(tab) #find index of which one occurs the most times
        mde <- attributes(tab)$dimnames[[1]][mdeID] #extract the value
        
    }
    
    #return the mode
    return(mde)
}


#this function generalizes the function over() to work for character vectors
#using the getMode as collapsing function if there is more than value per 
#geometry
overChr <- function(x, y, variable) {
    #x is the geometry used to query (ex. spatialpoints/linesdf)
    #y is the layer values are extracted from (ex. raster, spatialpolygonsdf)
    #variable is the variable within y you want queried from
    require(rgeos)
    
    #extract overlaping values of y for all layers of y
    overlist <- over(x, y, returnList = TRUE) 
    
    #get only the variable we want
    varlist <- sapply(overlist, function(ol) ol[,variable])
    
    #collapse that variable so there is only one value per geometry
    most <- sapply(varlist, function(vl) getMode(vl))
    
    return(most)
}

#This recodes a vector using a data.frame key provided
recodeDF <- function(v, df, fromname='from',toname='to') {
    #v is the vector to be recoded
    #df is the data.frame specifying what is recoded to what
    #fromname string giving the column name that gives the original values of 
        #the vector
    #toname string giving the column name that gives what the original values
        #get recoded to
    
    vr <- v #make a copy of our vector
    
    #loop over each of the rows in the df key and recode the value in each row
    for (i in 1:nrow(df)) {
        vr[v==df[i, fromname]] <- df[i, toname]
    }
    
    return(vr) #return recoded vector
    
}

#This function recodes a range of (numeric) values to a single value
recodeRange <- function(v, df, string=TRUE, digits=NA) {
    #v the vector to be recoded.
    #df the data.frame that specifies how ranges of numbers are recoded
        #ex specifying elements with values 1-5 are recoded as 'low'
    
    dfn <- names(df) #what columns are present in df key
    reqnames <- c('min','max','id') #the columns that need to be present
    
    if (string) {
        reqnames <- c(reqnames, 'string') #also this one if recoding to string
    }
    
    #check if all necessary columns are present and named properly
    if (!all(reqnames %in% dfn)) {
        stop('Your df must have the columns: min, max, id (and string if string=TRUE).')
    }
    
    #extracting minimum and maximum range values
    min <- df$min
    max <- df$max
    
    #print(class(v))
    
    #round values if specified
    if (!is.na(digits)) {
        vr <- round(v, digits)
        
    } else {
        vr <- v #make a copy
    }
    
    #loop over each element in the key and convert all values in that range
    #to the recoded id
    for (i in 1:nrow(df)) {
        vr[v<=max[i] & v>=min[i]] <- df$id[i]
    }
    
    if (string) { #if string is specified, convert 
        vr <- as.character(vr)
        df$chrID <- as.character(df$id)
        
        for (i in 1:nrow(df)) { #convert values to specified string values 
            vr[vr==df$chrID[i]] <- df$string[i]
        }
    }
    
    return(vr)
    
    
}

#This function converts blank elements (ex. ' ') to NAs
recodeBlank <- function(x) {
    #a vector with blank elements to be recoded
    
    xsquished <- gsub(" ", "", x) #convert all elements with only spaces
    
    blankIDs <- which(xsquished=='') #which elements have nothing in them
    x[blankIDs] <- NA #convert those elements to NA

    return(x) #return converted vector
}


#this function takes in a numeric vector and a factor and collapses the vector
#by the factor groupings. Basically a wrapper around tapply but it returns a 
#vector instead of a 1d array
collapseVariable <- function(v, f, fun=mean, rmNA=FALSE) {
    #v the vector to be collapsed
    #f the vector specifying the groupings of v, should be same length as v
    #fun the function used to do the collapsing, a summary stat
    #rmNA should na.rm=TRUE be passed to the tapply call
    
    #if f isn't a factor, convert it to one
    if (!is.factor(f)) {
        f <- as.factor(f)
    }
    
    if (rmNA) { #if nas need to be removed
        oneD <- tapply(v, f, FUN = fun, na.rm=TRUE)
    } else {
        oneD <- tapply(v, f, FUN = fun)
    }
   
    #convert 1d array to a vector
    vec <- as.vector(unlist(oneD))
    
    return(vec)
    
}

