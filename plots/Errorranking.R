library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
cv <- read.csv(file.path(datapath, 'results/CrossValidationModelFinal.csv'))

cv$TruePos <- cv$TruePos/15

cv <- select(cv, -omission)

cvm <- melt(cv, id.vars = 'threshold',variable.name = 'metric')
levels(cvm$metric) <- c('Positive Predictive Value',
                        'True Positive Rate')


pal <- colorRampPalette(brewer.pal(9, 'YlGnBu'))

#convert true positive to percent
p <- ggplot(data=cvm) +
    geom_line(aes(x=threshold, y=value)) +
    facet_wrap(~metric) +
    labs(x='Probability of Occupancy Threshold',
         y='Metric Value') +
    theme_bw(6) 

tiff(file.path(datapath, 'results/plots/ErrorCurve.tiff'), width = 900,
     res=300)
    p
dev.off()
