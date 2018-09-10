
# Setup -------------------------------------------------------------------
library(tidyverse)


datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))


rasiraw <- read.csv(file.path(datapath, 'processed/RasiStreamDF.csv'))

dropvars <- c('source','slopemin','slopemax',
              'bedrock', 'divDrainArea','forest')

# Remove observations -----------------------------------------------------

#removing low elevation observations brings us down to 2060
rasi <- rasiraw %>% 
    filter(elevmax>=1524) %>% 
    select(-dropvars)


# Add directional vars ----------------------------------------------------

rasi$south <- ifelse(between(rasi$aspect, 90, 270), 1, 0)
rasi$west <- ifelse(between(rasi$aspect, 180, 360), 1, 0)
rasi$SW <- ifelse(between(rasi$aspect, 135, 315), 1, 0)
rasi$SE <- ifelse(between(rasi$aspect, 45, 225), 1, 0)



# Process Factors ---------------------------------------------------------

rasi$habitat <- recode_factor(rasi$habitat, 'CON'='conifer', 'HDW'='hardwood',
                              'NVG'='nonveg','MIX'='mixed','HRB'='herbaceous',
                              'SHB'='shrub')

rasi$canopyClosure <- recode_factor(rasi$canopyClosure, 'D'='dense',
                                    'M'='moderate', 'P'='partial', 'S'='sparse',
                                    'X'='none')
rasi$treesize <- as.factor(recode(rasi$treesize, `0`='noTrees', 
                                  `1`='smallTrees', `2`='smallTrees',
                                  `3`='smallTrees',`4`='largeTrees',
                                  `5`='largeTrees', `6`='largeTrees'))


rasi$perennial <- rasi$seasonality
rasi$seasonality <- NULL
rasi$perennial <- as.numeric(ifelse(rasi$perennial=='perennial', 1,0))

rasi2 <- expandFactors(rasi, 'rasi')

rasi2 <- convertFactors(rasi2, varnames= c('habitat','rocktype', 'soil',
                                           'canopyClosure','treesize'))


# Adding/subtracting topographical vars -----------------------------------


rasi2$xy <- rasi2$x * rasi2$y

rasi2 <- rasi2 %>% select(-c(aspect,elevmax))


# Transforming variables --------------------------------------------------


rasi2$totDrainArea <- sqrt(rasi2$totDrainArea)
rasi2$waterbodies <- sqrt(rasi2$waterbodies)

# Removing outliers -------------------------------------------------------
#this takes us down to 2057

levIDs <- which(rasi2$totDrainArea>30 | rasi2$waterbodies > 8) 
rasi2 <- rasi2[-levIDs,]



rasi3 <- select(rasi2, -id)

write.csv(rasi3, file.path(datapath, 'processed/RasiModelDF.csv'),
          row.names = FALSE)
write.csv(rasi2, file.path(datapath, 'processed/RasiResultsDF.csv'),
          row.names = FALSE)

