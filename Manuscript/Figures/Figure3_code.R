
# This code plots figure 3.

########################################################################################################################
# Plot abundance over time
plot.new()
abunMax <- max(resultsMatrix[, 'abundance', ]) * 1.1
daySeries <- seq(1, simulationEnd)

quant0abun   <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.0)
quant10abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.1)
quant20abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.2)
quant30abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.3)
quant40abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.4)
quant50abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.5)
quant60abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.6)
quant70abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.7)
quant80abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.8)
quant90abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.9)
quant100abun <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 1.0)
meanAbun = apply(resultsMatrix[, 'abundance', ], 1, mean, na.rm=TRUE)

abundPlot <- ggplot() +
  geom_ribbon(aes(x=daySeries, ymax=quant100abun, ymin=quant0abun, fill='full range  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant90abun, ymin=quant10abun, fill='percentile 10 to 90  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant80abun, ymin=quant20abun, fill='percentile 20 to 80  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant70abun, ymin=quant30abun, fill='percentile 30 to 70  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant60abun, ymin=quant40abun, fill='percentile 40 to 60  ')) +
  geom_line(aes(daySeries, quant50abun, colour = 'median  ')) +
  geom_line(aes(daySeries, meanAbun, colour = 'mean  '), size=1.0) +
  scale_x_continuous(limits=c(0, simulationEnd), expand = c(0, 25), breaks=c(365, 730, 1095, 1460, 1825),
                     labels = c('1', '2', '3', '4', '5')) +
  scale_y_continuous(limits=c(0, abunMax), expand = c(0, 0)) +
  ylab('abundance') +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0))) +
  theme(axis.text=element_text(size=26, color='black'), 
        axis.title=element_text(size=26, face="bold", color='black')) +
  xlab('') +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour='black'),
        panel.background = element_blank()) +
  scale_colour_manual(name=NULL, values=c('median  '='#1B4F72',
                                          'mean  '='black')) +
  scale_fill_manual(name=NULL, values=c('full range  '='#5DADE2', 
                                        'percentile 10 to 90  '='#3498DB', 
                                        'percentile 20 to 80  '='#2E86C1', 
                                        'percentile 30 to 70  '='#2874A6', 
                                        'percentile 40 to 60  '='#21618C')) +
  theme(legend.text.align=0) + 
  theme(legend.position='none') 
########################################################################################################################


########################################################################################################################
# Plot disease prevalence over time

daySeries <- seq(1, simulationEnd)
prevMax <- max(resultsMatrix[, 'infective', ]) * 1.1

quant0inf   <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.0)
quant10inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.1)
quant20inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.2)
quant30inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.3)
quant40inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.4)
quant50inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.5)
quant60inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.6)
quant70inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.7)
quant80inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.8)
quant90inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.9)
quant100inf <- apply(resultsMatrix[, 'infective', ], 1, quantile, 1.0)
meanInf = apply(resultsMatrix[, 'infective', ], 1, mean, na.rm=TRUE)

infectPlot <- ggplot() +
  geom_ribbon(aes(x=daySeries, ymax=quant100inf, ymin=quant0inf, fill='full range  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant90inf, ymin=quant10inf, fill='percentile 10 to 90  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant80inf, ymin=quant20inf, fill='percentile 20 to 80  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant70inf, ymin=quant30inf, fill='percentile 30 to 70  ')) +
  geom_ribbon(aes(x=daySeries, ymax=quant60inf, ymin=quant40inf, fill='percentile 40 to 60  ')) +
  geom_line(aes(daySeries, quant50inf, colour = 'median  ')) +
  geom_line(aes(daySeries, meanInf, colour = 'mean  '), size=3.0) +
  scale_x_continuous(limits=c(0, simulationEnd), expand = c(0, 25), breaks=c(365, 730, 1095, 1460, 1825),
                     labels = c('1', '2', '3', '4', '5')) +
  scale_y_continuous(limits=c(0, prevMax), expand = c(0, 0)) +
  ylab('disease prevalence') +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0))) +
  theme(axis.text=element_text(size=26, color='black'), 
        axis.title=element_text(size=26, face="bold", color='black')) +
  xlab('year') +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, colour='black'),
        panel.background = element_blank()) +
  scale_colour_manual(name=NULL, values=c('median  '='#1B4F72',
                                          'mean  '='black')) +
  scale_fill_manual(name=NULL, values=c('full range  '='#5DADE2', 
                                        'percentile 10 to 90  '='#3498DB', 
                                        'percentile 20 to 80  '='#2E86C1', 
                                        'percentile 30 to 70  '='#2874A6', 
                                        'percentile 40 to 60  '='#21618C')) +
  theme(legend.text.align=0) + 
  theme(legend.text=element_text(size=26)) +
  theme(legend.position='bottom') 
########################################################################################################################

grid.draw(rbind(ggplotGrob(abundPlot), ggplotGrob(infectPlot)))
