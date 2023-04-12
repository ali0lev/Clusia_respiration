library(dplyr)
library(ggplot2)


vc_data <- data.frame(species = c(rep('prat',5),rep('tocu',5)), vcmax = c(c(31,31,26,41,39), c(39,44,36,33,35)), stringsAsFactors = FALSE)
vc_data_agg <- aggregate(data = vc_data, FUN = mean, .~species)
vc_data_SD <- aggregate(data = vc_data, FUN = function(x)  sd(x), .~species)
vc_data_SE <- vc_data_SD
vc_data_SE$vcmax <- vc_data_SE$vcmax/sqrt(5)
vc_data_error <- left_join(vc_data_SD, vc_data_SE, by = c('species' = 'species'), suffix = c('_SD', '_SE'))
vc_data_agg <- left_join(vc_data_agg, vc_data_error, by = c('species' = 'species'))

vc_data_agg 

ggplot(data = vc_data_agg, aes(x = species, y = vcmax), fill = species) + 
  geom_bar(stat = 'identity', position = 'dodge')+ 
  geom_errorbar(aes(ymin = vcmax - vcmax_SE, ymax = vcmax + vcmax_SE),
                width=0.15, 
                size=1,
                position=position_dodge(.5))

t.test(vc_data$vcmax[vc_data$species == 'prat'] , vc_data$vcmax[vc_data$species == 'tocu'])
