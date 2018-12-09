library(raster)
library(dismo)
library(plyr)
library(dplyr)
library(ggplot2)
datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
funpath <- '/Users/echellwig/Research/frog/functions/'

source(file.path(funpath, 'fitting.R'))
source(file.path(funpath, 'modeval.R'))
source(file.path(funpath, 'response.R'))

rasi0 <- read.csv(file.path(datapath,'processed/RasiModelDF.csv'))
rasi0$bio11 <- rasi0$bio11/10


vars <- read.csv(file.path(datapath, 'results/SelectedVariables.csv'))
vars <- as.character(vars$x)

numvars <- c('bio11','length','meadows','slopemean','streamOrder',
             'totDrainArea','waterbodies','x')

chrvars <- c('Disturbed','dense','hardwood')

# Import data -------------------------------------------------------------
# 
# filelist <- lapply(vars[-1], function(v) {
#     list.files(path=file.path(datapath,'maxent/bootstrap1000'),
#                pattern=v, full.names = TRUE)
# })
# 
# rd <- lapply(filelist, function(files) {
#     ldply(files, function(f) {
#         read.csv(f, stringsAsFactors = FALSE)
#     })
# })
# 
# modfiles <- sapply(vars[-1], function(v) {
#     name <- paste0(v, '.dat')
#     list.files(path=file.path(datapath,'maxent/FullModel/plots'),
#                pattern=name, full.names = TRUE)
# })
# 
# 
# md <- lapply(modfiles, function(f) read.csv(f, stringsAsFactors = FALSE))
# 
# # Subset Data -------------------------------------------------------------
# 
# ranges <- sapply(vars[-1], function(v) {
#     range(rasi0[,v])
# })
# 
# r <- lapply(1:length(rd), function(i) {
#     ids <- which(rd[[i]][,'x'] >= ranges[1,i] & rd[[i]][,'x'] <= ranges[2,i])
#     rd[[i]][ids,]
# })
# 
# m <- ldply(1:length(md), function(i) {
#     ids <- which(md[[i]][,'x'] >= ranges[1,i] & md[[i]][,'x'] <= ranges[2,i])
#     md[[i]][ids,]
# })
# 
# names(m)[2:3] <- c('value','prob')
# 
# 
# #lapply(1:11, function(i) {
# #    unique(table(r[[i]]$x))
# #})
# 
# 
# 
# # Calculate confidence intervals ------------------------------------------
# 
# rci <- ldply(1:length(r), function(i) bootCI(r[[i]], varname=vars[i+1]))
# 
# missingLengths <- setdiff(rci[rci$variable=='length','value'],
#                           m[m$variable=='length','value'])
# 
# badlengths <- which(rci$variable=='length' & rci$value %in% missingLengths)
# rci2 <- rci[-badlengths,]
# 
# rm <- merge(rci2, m, by=c('variable','value'))
# 
# levels(rm$variable) <- c('Disturbed Soil','Mean Winter Temperature (Bio 11)',
#                          'Dense Canopy Cover','Hardwood Forest',
#                          'Stream Length', 'Nearby Meadows (within 1 km)',
#                          'Mean Slope','Modified Strahler Stream Order',
#                          'Total Cumulative Drainage Area', 
#                          'Nearby Bodies of Water (within 1 km)',
#                          'Longitude')
# 
# 
# write.csv(rm, file.path(datapath, 'results/ResponseData.csv'),
#           row.names = FALSE)

# Separate by group -------------------------------------------------------

rm <- read.csv(file.path(datapath, 'results/ResponseData.csv'))

rm$variable <- revalue(rm$variable, c('Stream Length'='Stream Reach Length'))

numlevs <- levels(rm$variable)[c(4:11)]
chrlevs <- levels(rm$variable)[c(1:3)]

rnum <- rm[rm$variable %in% numlevs, ]
rchr <- rm[rm$variable %in% chrlevs, ]
# make num plot ------------------------------------------------------------

filenames <- c('x', 'slope', 'bio11', 'streamOrder', 'waterbodies', 
               'meadows', 'length', 'totDrainArea')

xlabels <- c('Longitude (degrees)', 'Percent Slope', 'Temperature (Degrees C)',
             'Stream Order', expression(sqrt('Number of Waterbodies')),
             'Number of Meadows','Length (km)', expression(sqrt('km'^2))) 



for (i in 1:length(numlevs)) {
    numplot <- ggplot(data=rnum[rnum$variable==numlevs[i], ]) + 
        geom_line(aes(x=value, y=prob)) +
        geom_ribbon(aes(x=value, ymax=upper, ymin=lower), alpha=0.4) +
        ylim(c(0,1)) + 
        theme_bw(15) + 
        labs(x=xlabels[i], y='', title=numlevs[i]) +
        theme(axis.text = element_text(size=16)) +
        theme(plot.title = element_text(hjust = 0.5))
    
    if (i==2) {
        numplot <- numplot + scale_x_continuous(labels = scales::percent)
    }
    
    
    path <- file.path(datapath, 'results/plots/response/individualplots',
                      paste0(filenames[i], '.tiff'))
    
    ggsave(path, plot=numplot, width=5, height=3)
}



# make chr plot -----------------------------------------------------------

rchr$value <- c('Not Dense','Dense', 'Not Disturbed',
                'Disturbed','Not Hardwood','Hardwood')


chrfiles <- c('dense','disturbed','hardwood')

for (i in 1:length(chrlevs)) {
    chrplot <- ggplot(data=rchr[rchr$variable==chrlevs[i],]) +
        geom_bar(aes(x=value,y=prob), stat='identity') +
        geom_errorbar(aes(x=value, ymin=lower, ymax=upper), width=0.2) +
        ylim(low=0, high=1) +
        theme_bw(15) +
        labs(y='', x='', title=chrlevs[i]) +
        theme(axis.text.x = element_text(size=14), 
              axis.text.y = element_text(size=16)) +
        theme(plot.title = element_text(hjust = 0.5))
    
    path <- file.path(datapath, 'results/plots/response/individualplots',
                      paste0(chrfiles[i], '.tiff'))
    
    ggsave(path, plot=chrplot, width=3.5, height=3)
}




