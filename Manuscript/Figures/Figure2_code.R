
# This script generates Figure 2 in the manuscript.
# Save it as 2000x1000 png. Don't maintain aspect ratio when resizing.

rm(list = ls())
plot.new()

library(ggplot2)
library(cowplot)

population = 404
total_cost = c(0, 1019.09, 2757.3, 4735.89, 8453.7)
marginal_cost = total_cost / 404
percent_population = c(0, 25, 50, 75, 100)

ggplot() +
  geom_ribbon(aes(x=percent_population, ymax=marginal_cost, ymin=c(0, 0, 0, 0, 0), fill='total')) +
  geom_line(aes(percent_population, marginal_cost, colour = 'marginal'), size=3.0) +
  geom_point(aes(percent_population, marginal_cost, colour = 'marginal'), size=4) +
  ylab('contact cost (ZAR)') +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0))) +
  theme(axis.text=element_text(size=26, color='black'), 
        axis.title=element_text(size=26, face="bold", color='black')) +
  xlab('percent of population') +
  scale_colour_manual(name=NULL, values=c('marginal'='#1B4F72')) +
  scale_fill_manual(name=NULL, values=c('total'='#5DADE2')) +
  theme(legend.text.align=0) + 
  theme(legend.text=element_text(size=26)) +
  theme(legend.position='right') 

