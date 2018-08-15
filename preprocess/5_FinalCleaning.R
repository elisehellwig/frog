##################Extract Values#####################

# Setup -------------------------------------------------------------------


#loading required packages
library(raster)
library(rgdal)
library(rgeos)


#saving the path where data is stored as a variable for later use
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'preprocess.R'))


#loading DEM values for each of the stream reaches
demdf <- readRDS(file.path(datapath, 'processed/extractedDEMvalues.RDS'))
rockkey <- read.csv(file.path(datapath, 'processed/rockclassification.csv'),
                    stringsAsFactors = FALSE)

#loading stream reach spatiallinesdataframe
ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines3.RDS'))

biorasi <- read.csv(file.path(datapath, 'processed/BioRasiDF.csv'))


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


# Add extra Variables -----------------------------------------------------

TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')
rasproj <- spTransform(ras, TA)

ras$length <- gLength(rasproj, byid = TRUE)/1000

rasicoords <- coordinates(ras)

ras$x<- sapply(seq_along(rasicoords), function(i) {
    mean(rasicoords[[i]][[1]][,1])
})

ras$y <- sapply(seq_along(rasicoords), function(i) {
    mean(rasicoords[[i]][[1]][,2])
})

ras$perennial <- ifelse(ras$fcode=='46006', 1, 0)
ras$south <- ifelse(grepl('S', ras$cardinal), 1, 0)

ras$hard <- recodeDF(ras$ptype, rockkey, 'geoclass','hard')
ras$rocktype <- recodeDF(ras$ptype, rockkey, 'geoclass','rocktype')



# Subset rows/columns ------------------------------------------------------

#removing habitat types that are NA
NAids  <- which(!is.na(ras$whrtype))
ras <- ras[NAids, ]


#the variables we want that are not precipitation or temperature
rnames1 <- c('comid', 'source', 'rasi', 'length', 'elevmax', 
             'elevmin', 'slopemax', 'slopemin', 'slopemean', 'cardinal',  
             'fcode', 'streamorde', 'ptype', 'soiltype', 'whrtype',
             'totdasqkm', 'divdasqkm','south','perennial','x', 'y', 'hard',
             'rocktype')



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

#really making sure we don't have any NAs (there are 7 in bedrock)
cc <- complete.cases(data.frame(rasfinal))
rasfinal <- rasfinal[cc, ]


# Adding Bioclim variables ------------------------------------------------

bio <- getData('worldclim', var='bio', res=0.5, lon=-121, lat=39)
names(bio) <- gsub('_11', '', names(bio))
biorasi <- extract(bio, rasfinal, fun=mean)
biorasi <- as.data.frame(biorasi)

rasdf <- data.frame(rasfinal)
rasdf <- cbind(rasdf, biorasi)
# write files -------------------------------------------------------------


saveRDS(rasfinal, file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
shapefile(rasfinal, file.path(datapath, 
                              'processed/shapefiles/RasiStreamLinesFinal.shp'),
          overwrite=TRUE)
write.csv(rasdf, file.path(datapath, 'processed/RasiStreamDF.csv'),
          row.names = FALSE)

