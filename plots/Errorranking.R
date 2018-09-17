library(ggplot2)
library(reshape2)
library(RColorBrewer)

datapath <- '/Volumes/GoogleDrive/My Drive/OtherPeople/frogData/data'
cv <- read.csv(file.path(datapath, 'results/CrossValidationModelFinal.csv'))

cvm <- melt(cv, id.vars = 'threshold',variable.name = 'metric')
levels(cvm$metric) <- c('Positive Predictive Value','Number of True Positives')


pal <- colorRampPalette(brewer.pal(9, 'YlGnBu'))

p <- ggplot(data=cvm) +
    geom_line(aes(x=threshold, y=value)) +
    facet_wrap(~metric, scales='free') +
    labs(x='Probability Threshold',
         y='Metric Value') +
    theme_bw(10) 

tiff(file.path(datapath, 'results/plots/ErrorCurve.tiff'), width = 600,
     res=150)
    p
dev.off()
