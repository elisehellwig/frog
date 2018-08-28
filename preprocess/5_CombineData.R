##################Extract Values#####################

# Setup -------------------------------------------------------------------


#loading required packages
library(raster)
library(rgdal)
library(rgeos)
library(circular)


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
#rasr <-readRDS(file.path(datapath, 'processed/RasiStreamLines3reduced.RDS'))
#biorasi <- read.csv(file.path(datapath, 'processed/BioRasiDF.csv'))


# Collapse DEM Variables ---------------------------------------------------

#converting ID to factor for collapseVariable()
demdf$fid <- as.factor(demdf$ID)


#Calculating slope summary statistics for each reach
ras$slopemax <- collapseVariable(demdf$slope, demdf$fid, fun = max)
ras$slopemin <- collapseVariable(demdf$slope, demdf$fid, fun = min)
ras$slopemean <- collapseVariable(demdf$slope, demdf$fid, fun = mean)

##Calculating elevation summary statistics for each reach
ras$elevmax <- collapseVariable(demdf$layer, demdf$fid, fun = max)
ras$elevmean <- collapseVariable(demdf$layer, demdf$fid, fun = mean)

# Process Aspect ----------------------------------------------------------

ras$aspect <- sapply(1:length(ras), function(i) {
    v <- demdf[demdf$ID==i, 'aspect']
    vc <- circular(v, units='degrees', type='angles', modulo='2pi')
    round(as.numeric(mean(vc)))
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

ras$rocktype <- recodeDF(ras$ptype, rockkey, 'geoclass','rocktype')

# Subset rows/columns ------------------------------------------------------

#removing habitat types that are NA
NAids  <- which(!is.na(ras$whrtype))
ras <- ras[NAids, ]


#the variables we want that are not biovars
rnames <- c('comid', 'source', 'rasi', 'length', 'elevmax', 
             'elevmean', 'slopemax', 'slopemin', 'slopemean',  
             'fcode', 'streamorde', 'ptype', 'soiltype', 'whrtype', 'whrsize',
             'whrdensity', 'totdasqkm', 'divdasqkm','x', 'y', 'rocktype',
             'join_count','join_cou_1','aspect')



#subseting the data with the columns we want
rasfinal <- ras[, rnames]

#renaming a couple of confusingly named columns
names(rasfinal)[c(1, 10:18, 22:23)] <- c('id', 'seasonality','streamOrder',
                                  'bedrock','soil','habitat', 'treesize',
                                  'canopyClosure', 'totDrainArea',
                                  'divDrainArea', 'meadows','waterbodies')

rasfinal2 <- rasfinal
rasfinal2 <- rasfinal2[,-(16:17)]

#really making sure we don't have any NAs (there are 7 in bedrock)
cc <- complete.cases(data.frame(rasfinal))
rasfinal <- rasfinal[cc, ]

cc2 <- complete.cases(data.frame(rasfinal2))
rasfinal2 <- rasfinal2[cc2, ]


# Adding Bioclim variables ------------------------------------------------

# bio <- getData('worldclim', var='bio', res=0.5, lon=-121, lat=39)
# names(bio) <- gsub('_11', '', names(bio))
# biorasi <- extract(bio, rasfinal, fun=mean)
# biorasi2 <- extract(bio, rasfinal2, fun=mean)
# 
# biorasi <- as.data.frame(biorasi)
# biorasi2 <- as.data.frame(biorasi2)
# 
# saveRDS(biorasi, file.path(datapath, 'processed/BioVarsRowReduced.RDS'))
# saveRDS(biorasi2, file.path(datapath, 'processed/BioVars.RDS'))

biorasi <- readRDS(file.path(datapath, 'processed/BioVarsRowReduced.RDS'))
biorasi2 <- readRDS(file.path(datapath, 'processed/BioVars.RDS'))



rasdf <- data.frame(rasfinal)
rasdf2 <- data.frame(rasfinal2)

rasdf <- cbind(rasdf, biorasi)
rasdf2 <- cbind(rasdf2, biorasi2)


rasdf$meadows <- as.integer(rasdf$meadows)
rasdf$waterbodies <- as.integer(rasdf$waterbodies)
rasdf$treesize <- as.integer(rasdf$treesize)


rasdf2$meadows <- as.integer(rasdf2$meadows)
rasdf2$waterbodies <- as.integer(rasdf2$waterbodies)

# write files -------------------------------------------------------------


saveRDS(rasfinal2, file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
shapefile(rasfinal2, file.path(datapath, 
                              'processed/shapefiles/RasiStreamLinesFinal.shp'),
          overwrite=TRUE)
write.csv(rasdf2, file.path(datapath, 'processed/RasiStreamDF.csv'),
          row.names = FALSE)


write.csv(rasdf, file.path(datapath, 'processed/RasiStreamDFrowreduced.csv'),
          row.names = FALSE)
saveRDS(rasfinal, file.path(datapath, 
                             'processed/RasiStreamLinesFinalrowreduced.RDS'))
shapefile(rasfinal, file.path(datapath, 
          'processed/shapefiles/RasiStreamLinesFinalrowreduced.shp'),
          overwrite=TRUE)

