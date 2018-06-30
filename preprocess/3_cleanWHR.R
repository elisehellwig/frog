###################Clean and add WHR###########################

# Script Setup ------------------------------------------------------------

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
library(ggplot2)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

#importing functions
source(file.path(funpath, 'preprocess.R'))
ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines2.RDS'))


TA <- CRS('+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ')

whrname <- c('Alpine-Dwarf Shrub','Annual Grassland','Aspen','Barren',
             'Douglas-Fir', 'Eastside Pine','Jeffrey Pine',
             'Lacustrine','Lodgepole Pine','Mixed Chaparral',
             'Montane Chaparral', 'Montane Hardwood-Conifer',
             'Montane Hardwood', 'Montane Riparian', 'Pasture',
             'Perennial Grassland','Ponderosa Pine','Red Fir', 'Riverine',
             'Subalpine Conifer', 'Sagebrush','Sierran Mixed Conifer',
             'Urban', 'White Fir', 'Wet Meadow')


# Merging WHR Layers ------------------------------------------------------



streamTA <- spTransform(stream, TA)
streamExt <- extent(streamTA)

whrpath <- file.path(datapath, 
                     'frog_model/data/datasets/cwhr_4Megan/CWHRVg.gdb')

whrlayers <- ogrListLayers(whrpath)
attributes(whrlayers) <- NULL

#ogrListLayers
whrlist <- lapply(whrlayers, function(l) {
    readOGR(dsn=whrpath, layer=l, stringsAsFactors = FALSE)
}) 

whrlistTA <- lapply(whrlist, function(spdf) {
    crop(spTransform(spdf, TA), streamExt)
    })

# whrpaths <- paste('whr', whrnames, sep='/')
# whrlist <- lapply(whrpaths, function(fn) shapefile(file.path(datapath, fn)))

whr <- do.call(bind, whrlistTA)

saveRDS(whr, file.path(datapath, 'processed/whr.RDS'))
shapefile(whr, file.path(datapath, 'processed/shapefiles/whr.shp'))


# WHR Processing ----------------------------------------------------------



whr <- readRDS(file.path(datapath, 'processed/whr.RDS'))
whr <- spTransform(whr, crs(ras))

ras$whrtype <- overChr(ras, whr, 'WHRTYPE') #70 NAs 
ras$whrdensity <- overChr(ras, whr, 'WHRDENSITY') #70 NAs 
ras$whrsize <- overChr(ras, whr,'WHRSIZE') #194 NAs 



# WHR Type ----------------------------------------------------------------


#All observations with NAs for whrtype have no rasi presence

brrIds <- which(ras$whrtype=='BBR')
ras$whrtype[brrIds] <- "BAR"


typedf <- data.frame(name=whrname,
                     whr=table(ras$whrtype))
names(typedf) <- c('name','code','count')
typedf$xlabel <- paste0(typedf$name, ' (', typedf$code, ')') 


# WHR Type Plots ----------------------------------------------------------


typehist1 <- ggplot(data=typedf) +
    geom_bar(aes(x=xlabel, y=count), stat='identity') + 
    labs(x='WHR Code', y='Stream Reach Count', 
         title='Number of stream reaches per habitat type (WHR), n=3032') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) 



typehist2 <- ggplot(data=typedf[typedf$code!='SMC',]) +
    geom_bar(aes(x=xlabel, y=count), stat='identity') + 
    labs(x='Habitat Type', y='Stream Reach Count', 
         title='Number of stream reaches per habitat type (WHR), sans SMC') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) 


# WHR Density -------------------------------------------------------------

blankdens <- which(ras$whrdensity==' ' | ras$whrdensity=='')
ras$whrdensity[blankdens] <- NA


densNA <- which(is.na(ras$whrdensity))
table(ras$rasi[densNA])

#after recoding blanks as NAs there were 13 rows with rasi presence that had
#WHR Density as NA


densname <- c('D: 60-100% Closed','M: 40-59% Closed','P: 25-39% Closed',
              'S: 10-24% Closed','X: Not Specified in Metadata')

densdf <- data.frame(name=densname,
                     whr=table(ras$whrdensity)) #n=2705
names(densdf) <- c('name','code','count')

densdf$name <- factor(densdf$name, 
                      levels(densdf$name)[c(4,3,2,1,5)])

# WHR Density Plots -------------------------------------------------------


denshist <- ggplot(data=densdf) + 
    geom_bar(aes(x=name, y=count), stat='identity') +
    labs(x='Canopy Closure', y='Stream Reach Count',
         title='Number of stream reaches by percent canopy closure, n=2705') +
    theme_bw(12) + 
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) 
    


# WHR Size ----------------------------------------------------------------

blanksize <- which(ras$whrsize==' ' | ras$whrsize=='')
ras$whrsize[blanksize] <- NA

sizeNA <- which(is.na(ras$whrsize))
#there are 10 observations with rasi presence that have have WHRsize as NA
table(ras$rasi[sizeNA])

whrNA <- union(sizeNA, densNA)
table(ras$rasi[whrNA])

#There are 15 total observations with rasi presence that have either WHRsize 
#or whrdensity as NA

sizename <- c('Unknown','Seedling','Sapling','Pole','Small Tree', 
              'Medium/Large Tree')
dbh <- c('Not Specified in Metadata', "DBH < 1in", "DBH = 1-6in", 
         "DBH = 6-11in", "DBH = 11-24in", "DBH > 24in")

sizedf <- data.frame(name=sizename,
                     dbh=dbh,
                     whr=table(ras$whrsize)) 
names(sizedf) <- c('name', 'DBH', 'code','count')
sizedf$xlabel <- paste0(sizedf$name, ' (', sizedf$DBH, ')')

sizedf$xlabel <- fct_relevel(sizedf$xlabel, sizedf$xlabel[c(2:6,1)])


# WHR Size Plots ----------------------------------------------------------


sizehist <- ggplot(data=sizedf) +
    geom_bar(aes(x=xlabel, y=count), stat='identity') + 
    labs(x='Average Tree Size Class', y='Stream Reach Count', 
         title='Number of stream reaches by average DBH, n=2705') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) 

