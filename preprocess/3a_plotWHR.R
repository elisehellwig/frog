library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
library(ggplot2)

#setting paths
funpath <- '/Users/echellwig/Research/frog/functions/'
datapath <- '/Users/echellwig/Research/frogData/data/'

whrname <- c('Alpine-Dwarf Shrub','Annual Grassland','Aspen','Barren',
             'Douglas-Fir', 'Eastside Pine','Jeffrey Pine',
             'Lacustrine','Lodgepole Pine','Mixed Chaparral',
             'Montane Chaparral', 'Montane Hardwood-Conifer',
             'Montane Hardwood', 'Montane Riparian', 'Pasture',
             'Perennial Grassland','Ponderosa Pine','Red Fir', 'Riverine',
             'Subalpine Conifer', 'Sagebrush','Sierran Mixed Conifer',
             'Urban', 'White Fir', 'Wet Meadow')




# WHR Type Plots ----------------------------------------------------------



typedf <- data.frame(name=whrname,
                     whr=table(ras$whrtype))
names(typedf) <- c('name','code','count')
typedf$xlabel <- paste0(typedf$name, ' (', typedf$code, ')') 


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


# WHR Density Plots ---------------------------------------------------------





densname <- c('D: 60-100% Closed','M: 40-59% Closed','P: 25-39% Closed',
              'S: 10-24% Closed','X: Not Specified in Metadata')

densdf <- data.frame(name=densname,
                     whr=table(ras$whrdensity)) #n=2705
names(densdf) <- c('name','code','count')

densdf$name <- factor(densdf$name, 
                      levels(densdf$name)[c(4,3,2,1,5)])


denshist <- ggplot(data=densdf) + 
    geom_bar(aes(x=name, y=count), stat='identity') +
    labs(x='Canopy Closure', y='Stream Reach Count',
         title='Number of stream reaches by percent canopy closure, n=2705') +
    theme_bw(12) + 
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) 



# WHR Size Plots ----------------------------------------------------------



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


sizehist <- ggplot(data=sizedf) +
    geom_bar(aes(x=xlabel, y=count), stat='identity') + 
    labs(x='Average Tree Size Class', y='Stream Reach Count', 
         title='Number of stream reaches by average DBH, n=2705') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) 


# Life Form Plot ----------------------------------------------------------
#note I don't know what NFO stands for

lifename <- c('Conifer','Hardwood','Herbaceous','Mixed','NFO?','Shrub')
lifedf <- data.frame(name=lifename,
                     whr=table(ras$lifeform))
names(lifedf) <- c('name','code','count')

lifehist <- ggplot(data=lifedf) + 
    geom_bar(aes(x=name, y=count), stat='identity') +
    labs(x='WHR Life Form', y='Stream Reach Count',
         title='Number of stream reaches by lifeform, n=2232') +
    theme_bw(16) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# Cover Type Plot ---------------------------------------------------------



covername <- c('Barren', 'Conifer','Hardwood','Herbaceous','Mixed','Shrub',
               'Urban','Water')
coverdf <- data.frame(name=covername,
                     whr=table(ras$covertype))
names(coverdf) <- c('name','code','count')

coverhist <- ggplot(data=coverdf) + 
    geom_bar(aes(x=name, y=count), stat='identity') +
    labs(x='WHR Cover Type', y='Stream Reach Count',
         title='Number of stream reaches by cover type, n=2622') +
    theme_bw(16) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 







