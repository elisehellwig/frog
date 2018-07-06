##################Extract Values#####################

# Setup -------------------------------------------------------------------


#loading required packages
library(raster)
library(rgdal)
library(rgeos)


#saving the path where data is stored as a variable for later use
datapath <- '/Users/echellwig/Research/frogData/data/'
funpath <- '/Users/echellwig/Research/frog/functions/'

#loading DEM values for each of the stream reaches
demdf <- readRDS(file.path(datapath, 'processed/extractedDEMvalues.RDS'))

#loading stream reach spatiallinesdataframe
ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines3.RDS'))


# Collapse DEM Variables ---------------------------------------------------

#converting ID to factor for collapseVariable()
demdf$fid <- as.factor(demdf$ID)


#Calculating slope summary statistics for each reach
ras$slopemax <- collapseVariable(demdf$slope, demdf$fid, fun = max)
ras$slopemin <- collapseVariable(demdf$slope, demdf$fid, fun = min)
ras$slopemean <- collapseVariable(demdf$slope, demdf$fid, fun = mean)

##Calculating elevation summary statistics for each reach
ras$elevmax <- collapseVariable(demdf$layer, demdf$fid, fun = max)
ras$elevmin <- collapseVariable(demdf$layer, demdf$fid, fun = min)


# Process Aspect ----------------------------------------------------------

#######create aspect key#######

## aspect values
vals <- c(0, rep(seq(22.5, 337.5, by=45), 2), 360)
aspdf <- as.data.frame(matrix(vals, ncol=2))

names(aspdf) <- c('min', 'max')

#IDs 
aspdf$id <- 1:nrow(aspdf)

#cardinal directions
aspdf$string <- c('N', 'NE', 'E', "SE",'S','SW','W','NW', 'N')

#convert aspect to character cardinal directions
ras$cardinal <- sapply(1:length(ras), function(i) {
    v <- demdf[demdf$ID==i, 'aspect']
    vcard <- recodeRange(v, aspdf)
    getMode(vcard)
    
})

# Subset rows/columns ------------------------------------------------------

#removing habitat types that are NA
NAids  <- which(!is.na(ras$whrtype))
ras <- ras[NAids, ]


#the variables we want that are not precipitation or temperature
rnames1 <- c('comid', 'state', 'rasi', 'length', 'elevmax','elevmin', 
             'slopemax', 'slopemin', 'slopemean', 'cardinal',  
             'fcode', 'streamorde', 'ptype', 'soiltype', 'whrtype',
             'totdasqkm', 'divdasqkm')


#creating names of precip and temp variables
var <- rep(c('ppt','tmax','tmin','tmean'), each=20)
quarter <- rep(rep(paste0('q', 1:4), each=5), 4)
sumstat <- rep(c('min','max','beg','end','lwm'), 16)
rnames2 <- paste0(var, quarter, sumstat)

#all the variables we want
rnames <- c(rnames1, rnames2)

#subseting the data with the columns we want
rasfinal <- ras[, rnames]

#renaming a couple of confusingly named columns
names(rasfinal)[c(1, 11:17)] <- c('id', 'seasonality','streamOrder',
                                  'bedrock','soil','habitat','totDrainArea',
                                  'divDrainArea')

#really making sure we don't have any NAs
cc <- complete.cases(data.frame(rasfinal))
rasfinal <- rasfinal[cc, ]

# write files -------------------------------------------------------------



saveRDS(rasfinal, file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
shapefile(rasfinal, file.path(datapath, 
                              'processed/shapefiles/RasiStreamLinesFinal.shp'),
          overwrite=TRUE)

