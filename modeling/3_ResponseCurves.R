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

filelist <- lapply(vars[-1], function(v) {
    list.files(path=file.path(datapath,'maxent/bootstrap1000'),
               pattern=v, full.names = TRUE)
})

rd <- lapply(filelist, function(files) {
    ldply(files, function(f) {
        read.csv(f, stringsAsFactors = FALSE)
    })
})

modfiles <- sapply(vars[-1], function(v) {
    name <- paste0(v, '.dat')
    list.files(path=file.path(datapath,'maxent/FullModel/plots'),
               pattern=name, full.names = TRUE)
})


md <- lapply(modfiles, function(f) read.csv(f, stringsAsFactors = FALSE))

# Subset Data -------------------------------------------------------------



ranges <- sapply(vars[-1], function(v) {
    range(rasi0[,v])
})

r <- lapply(1:length(rd), function(i) {
    ids <- which(rd[[i]][,'x'] >= ranges[1,i] & rd[[i]][,'x'] <= ranges[2,i])
    rd[[i]][ids,]
})

m <- ldply(1:length(md), function(i) {
    ids <- which(md[[i]][,'x'] >= ranges[1,i] & md[[i]][,'x'] <= ranges[2,i])
    md[[i]][ids,]
})

names(m)[2:3] <- c('value','prob')


lapply(1:11, function(i) {
    unique(table(r[[i]]$x))
})

# Calculate confidence intervals ------------------------------------------

rci <- ldply(1:length(r), function(i) bootCI(r[[i]], varname=vars[i+1]))

missingLengths <- setdiff(rci[rci$variable=='length','value'],
                          m[m$variable=='length','value'])

badlengths <- which(rci$variable=='length' & rci$value %in% missingLengths)
rci2 <- rci[-badlengths,]

rm <- merge(rci2, m, by=c('variable','value'))


# make plots --------------------------------------------------------------


rnum <- rm[rm$variable %in% numvars, ]
rchr <- rm[rm$variable %in% chrvars, ]




#rasiSP <- readRDS(file.path(datapath, 'processed/RasiStreamLinesFinal.RDS'))
r0 <- readRDS(file.path(datapath,'results/rasiplot.RDS'))
vars <- read.csv(file.path(datapath, 'results/SelectedVariables.csv'))
vars <- as.character(vars$x)

rr <- r0[, vars] 

rasi0 <- convertFactors(rasi0, exclude='rasi')
r <- convertFactors(rr, exclude='rasi')

#subsetting data to be only some variables 
rasi <- rasi0 %>% select(vars) 
rasi$bio11 <- rasi$bio11/10

# Running the model -------------------------------------------------------




# Response curves ---------------------------------------------------------

 numvars <- vars[c(3,6:12)]
 chrvars <- vars[c(2,4,5)]
 
 rcsNumeric <- ldply(numvars, function(var) {
     d <- data.frame(response(mod, var, expand=0, at=median))
     d$variable <- var
     names(d)[1:2] <- c('value','response')
     d
 })

 
 
 # 
# write.csv(rcsNumeric, file.path(datapath, 'results/responseNum.csv'),
#           row.names = FALSE)
# 
# rcsChar <- ldply(chrvars, function(var) {
#     d <- responseCurve(rasi, var, 50, 30, 23882929, plot=FALSE)
#     d$variable <- var
#     d
# })
# 
# write.csv(rcsChar, file.path(datapath, 'results/responseChr.csv'),
#           row.names = FALSE)


la <- responseCurve(rasi, 'waterbodies', 1000, 30, 23882929)


nplot <- ggplot(data=rcsNumeric) +
    geom_line(aes(x=value, y=response)) +
    facet_wrap(~variable, scales='free') +
    scale_y_continuous(limits = c(0, 1))

rn <- read.csv( file.path(datapath, 'results/responseNum.csv'))
rc <- read.csv( file.path(datapath, 'results/responseChr.csv'))

rn$valueScaled <- as.numeric(scale_by(value ~ variable, data=rn))

nplot <- ggplot(data=rn) + 
    geom_line(aes(x=valueScaled, y=response)) +
    facet_wrap( ~ variable)

nplot <- ggplot(data=rn[rn$variable=='waterbodies', ]) + 
    geom_line(aes(x=(value)^2, y=response)) 

rcs$value <- as.numeric(rcs$value)

v <- responseCurve(rasi, 'waterbodies', 10, 10, 23882929, plot=FALSE)

p <- responseCurve(rasi, 'waterbodies', 100, 30, 23882929)

pm <- p + labs(x='Waterbodies', y='Predicted Presence Probability') + 
    theme_bw() + scale_y_continuous(limits=c(0,1))


allvars <- names(rasi[,-1])

for (v in allvars) {
    print(v)
    p <- responseCurve(rasi, v, 100, 30, 8752)
    pm <- p + labs(x=v, y='Predicted Presence Probability') + 
        theme_bw() + scale_y_continuous(limits=c(0,1))
    filename <- paste0(v, '.png')
    png(file.path(datapath, 'results/plots/newresponse', filename))
    print(pm)
    dev.off()
}
