###############Process Soils#####################
## This script cleans up the soil data and extracts the most common soil type
## for each stream reach


# Setup -------------------------------------------------------------------



library(raster)
library(rgdal)
library(rgeos)
#library(ehelpr)

options(stringsAsFactors = FALSE)
#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'

#importing functions
source(file.path(funpath, 'preprocess.R'))

soil <- shapefile(file.path(datapath, 
                'raw/soils/MergedSoilsClipFinal.shp'))

ras <- readRDS(file.path(datapath, 'processed/RasiStreamLines1.RDS'))


#read in the df that tells us what to replace all the NAs in the soils data with
soilkey <- read.csv(file.path(datapath, 'processed/missingSoilsKey.csv'))


# Extracting values from SSURGO ----------------------------------------------


#converting the soil polygons to latlon
soill <- spTransform(soil, crs(ras))

#extracting out the SoilOrderD variable to see how many NAs we have
ras$soiltype <- overChr(ras, soill, 'SoilOrderD')

#extracting out other variables that will help us fill in the NAs
ras$symbol <- overChr(ras, soill, 'EsriSymbol')
ras$soilname <- overChr(ras, soill, 'muname')



# Replacing missing values --------------------------------------------------



#finding which rows have NAs
soilNAs <- which(is.na(ras$soiltype))

#saving the mu names and the esri symbols for help in identifying soil type
rasNAs <- ras[soilNAs, ]


#Names and symbols for missing soil types 
#missingSoils <- as.data.frame(rasNAs[,c('symbol','soilname')])
#write.csv(missingSoils, file.path(datapath, 'processed/missingSoils.csv'),
        #  row.names=FALSE)



#doing the soil replacement
ras$soiltype[soilNAs] <- soilkey$class




#save file
saveRDS(ras, file.path(datapath, 'processed/RasiStreamLines2.RDS'))


