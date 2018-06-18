##################Extract Values#####################

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)

datapath <- '/Users/echellwig/Research/frogData/data/'

orig <- read.table(file.path(datapath, 'cathy/rasi_mxt_allstr3.tab'),
                   header=TRUE, stringsAsFactors = FALSE, sep='\t')


