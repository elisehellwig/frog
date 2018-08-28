
# Setup -------------------------------------------------------------------
library(tidyverse)


datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))


rasiraw <- read.csv(file.path(datapath, 'processed/RasiStreamDFrowreduced.csv'))

dropvars <- c('id','source','slopemin','slopemax', 'soil',
              'bedrock', 'divDrainArea', 'treesize')

# Remove observations -----------------------------------------------------

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

rasi$perennial <- rasi$seasonality
rasi$seasonality <- NULL
rasi$perennial <- as.numeric(ifelse(rasi$perennial=='perennial', 1,0))

rasi2 <- expandFactors(rasi, 'rasi')

rasi2 <- convertFactors(rasi2, varnames= c('habitat','rocktype',
                                           'canopyClosure'))

rasi2 <- rasi2 %>% select(-aspect)

write.csv(rasi2, file.path(datapath, 'processed/RasiModelDF.csv'),
          row.names = FALSE)
