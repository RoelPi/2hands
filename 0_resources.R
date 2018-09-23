library(data.table)
library(ggplot2)
library(lubridate)
library(magrittr)
library(stringr)
library(missForest)
library(dummies)
library(doParallel)

t <- theme(plot.title = element_text(face="bold"),
           axis.text.x = element_text(size=10,color='#000000',angle=45,hjust=1),
           axis.text.y = element_text(size=10,color='#000000'),
           axis.title.x = element_text(face="bold", size=10,color='#000000'),
           axis.title.y = element_text(face="bold", size=10,color='#000000'),
           panel.background = element_rect(fill='#ffffff', color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major = element_line(color='#a5a5a5', linetype='dashed',size=0.2),
           panel.grid.minor = element_line(color='#a5a5a5', linetype='dashed', size=0),
           legend.text = element_text(size=10,color='#000000'),
           legend.title = element_text(face='bold',size=10,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=10,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#E02128','#ff0072','#1B2C3F','#3e21aa','#2672A4','#43A39E','#EB9E0F','#333745','#8f8f8f','#515151','#000000')