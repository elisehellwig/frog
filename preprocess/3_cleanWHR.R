###################Clean and add WHR###########################

# Script Setup ------------------------------------------------------------

library(raster)
library(rgdal)
library(rgeos)
options(stringsAsFactors = FALSE)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'

#importing functions
source(file.path(funpath, 'preprocess.R'))
ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines2.RDS'))
typeAgg <- read.csv(file.path(datapath,
                              'processed/TypeAggregationScheme.csv'))


TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')


# Merging WHR Layers ------------------------------------------------------

##Only need to run this once

# streamTA <- spTransform(ras, TA)
# streamExt <- extent(streamTA)
# 
# #setting up paths
whrpath1 <- file.path(datapath, 'raw/WHR/CWHRVg.gdb')
# whrpath2 <- file.path(datapath, 'raw/WHR/TNFWHR')
# whrpath <- c(whrpath1, whrpath1, whrpath2)
# 
# #setting up layers
# whrlayers <- ogrListLayers(whrpath[1])[-3]
# attributes(whrlayers) <- NULL
# whrlayers <- c(whrlayers, "Area_Lidar_1acre")
# 
# #ogrListLayers
# whrlist <- lapply(seq_along(whrlayers), function(i) {
#     readOGR(dsn=whrpath[i], layer=whrlayers[i], stringsAsFactors = FALSE)
# })
# 
# 
# 
# whrlistTA <- lapply(whrlist, function(spdf) {
#     crop(spTransform(spdf, TA), streamExt)
#     })
# 
# # whrpaths <- paste('whr', whrnames, sep='/')
# # whrlist <- lapply(whrpaths, function(fn) shapefile(file.path(datapath, fn)))
# 
# whrlistTA[[1]]$forest <- 'Plumas'
# whrlistTA[[2]]$forest <- 'Lassen'
# whrlistTA[[3]]$forest <- 'Tahoe'
# 
# whr <- do.call(bind, whrlistTA)
# 
# saveRDS(whr, file.path(datapath, 'processed/whr.RDS'))
# shapefile(whr, file.path(datapath, 'processed/shapefiles/whr.shp'),
#           overwrite=TRUE)


tahoeAlt <- readOGR(dsn=whrpath1, layer=ogrListLayers(whrpath1)[3],
                    stringsAsFactors = FALSE)


# # WHR Processing ----------------------------------------------------------
# 
#read in merged file
whr <- readRDS(file.path(datapath, 'processed/whr.RDS'))
whr <- spTransform(whr, crs(ras))
tahoeAlt <- spTransform(tahoeAlt, crs(ras))

# WHR Type ----------------------------------------------------------------


## 70 observations with NAs for whrtype, all have no rasi presence
## Merging with lifeform and covertype does not eliminate these nas



ras$whrtype <- overChr(ras, whr, 'WHRTYPE') 
whralt <- overChr(ras, tahoeAlt, 'WHRTYPE')
updaterows <- which(is.na(ras$whrtype) & (!is.na(whralt)))

ras$whrtype[updaterows] <- whralt[updaterows] 

ras$whrtype <- recodeDF(ras$whrtype, typeAgg)

# WHR Density -------------------------------------------------------------

ras$whrdensity <- overChr(ras, whr, 'WHRDENSITY') #397 NAs 
ras$whrdensity <- recodeBlank(ras$whrdensity)


# #after recoding blanks as NAs there were 13 rows with rasi presence that had
# #WHR Density as NA
# 
# densNA <- which(is.na(ras$whrdensity))
# table(ras$rasi[densNA])



# WHR Size ----------------------------------------------------------------

ras$whrsize <- overChr(ras, whr,'WHRSIZE') #194 NAs 
ras$forest <- overChr(ras, whr,'forest') #194 NAs 

ras$whrsize <- recodeBlank(ras$whrsize)



####there are 10 observations with rasi presence that have have WHRsize as NA
# sizeNA <- which(is.na(ras$whrsize))
# table(ras$rasi[sizeNA])
# 
# whrNA <- union(sizeNA, densNA)
# table(ras$rasi[whrNA])


# Life Form ---------------------------------------------------------------

ras$lifeform <- overChr(ras, whr,'WHRLIFEFORM') # 870 NAs, 28 w/ rasi pres
ras$lifeform <- recodeBlank(ras$lifeform)

ras$lifeform[which(ras$lifeform=='NFO')] <- 'No Forest'


# Cover Type --------------------------------------------------------------

ras$covertype <- overChr(ras, whr,'COVERTYPE') #480 NAs , 17 with rasi pres
ras$covertype <- recodeBlank(ras$covertype)

ras$covertype[which(ras$covertype=='XXX')] <- NA



# Save File ---------------------------------------------------------------

saveRDS(ras, file.path(datapath, 'processed/RasiStreamLines3.RDS'))
shapefile(ras, file.path(datapath, 
                         'processed/shapefiles/RasiStreamLines3.shp'),
          overwrite=TRUE)


