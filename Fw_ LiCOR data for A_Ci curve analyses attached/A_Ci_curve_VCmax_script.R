# this is a propper script to make graphs to depict the VC.max values for C. pratensis and C. tocuchensis
# Because there are not loads of data, I am going to just write the values from the Excell spreadsheet here; 
# this way I wont accidently interfere with the spreadsheets cos they have formulas which can be quite tempramental. 

library('ggplot2')
library("multcomp")
library('gridExtra')
library('agricolae')
library('dplyr')
library('reshape2')
library('ggsignif')


# rm(list=ls())

vc_data <- data.frame(species = c(rep('C. pratensis',5),rep('C. tocuchensis',5)),
                      vcmax = c(c(31,31,26,41,39), c(39,44,36,33,35)), stringsAsFactors = FALSE)
vc_data_agg <- aggregate(data = vc_data, FUN = mean, .~species)
vc_data_SD <- aggregate(data = vc_data, FUN = function(x)  sd(x), .~species)
vc_data_SE <- vc_data_SD
vc_data_SE$vcmax <- vc_data_SE$vcmax/sqrt(5)
vc_data_error <- left_join(vc_data_SD, vc_data_SE, by = c('species' = 'species'), suffix = c('_SD', '_SE'))
vc_data_agg <- left_join(vc_data_agg, vc_data_error, by = c('species' = 'species'))

vc_data_agg 

ggplot(data = vc_data_agg, aes(x = species, y = vcmax), fill = species) + 
  theme_classic()+   
  geom_bar(stat="identity", width = 0.5, position = "dodge", colour = 'black', size =0.8, aes(fill = species)) +
  scale_fill_manual(values = c( "grey50", "white"))+
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin = vcmax - vcmax_SD, ymax = vcmax + vcmax_SD),
                width=0.15, 
                size=1,
                position=position_dodge(.5)) +
  xlab(expression(bold('Species')))+
  ylab(expression(bold('V'[cmax]~'('*mu*'moles'~m^-2~s^-2*')')))+
  theme(axis.text.x = element_text(face = "italic", size = 16))+
  theme(axis.title = element_text(face = "italic", size = 18))+
  scale_fill_manual(values = c( "grey50", "white"),
                     name ="Species",
                     labels=c(expression(bold(paste(italic("C. pratensis")))),expression(bold(paste(italic("C. tocuchensis"))))))+


geom_signif(data = vc_data,
            comparisons = list(c("C. pratensis", "C. tocuchensis")), 
            test = 't.test', 
            y_position = 46, 
            annotations = 'NS')+ 
  geom_point(data = vc_data, aes(x=species, y=vcmax, fill = species), shape = 21, colour = 'black',size = 2,stroke = 1.5,  position = position_jitterdodge(jitter.width = 0.5, jitter.height=0, dodge.width = 1.5)) 
  
  
   

t.test(vc_data$vcmax[vc_data$species == 'C. pratensis'] , vc_data$vcmax[vc_data$species == 'C. tocuchensis'])
t.test( vc_data$vcmax[vc_data$species == 'C. tocuchensis'], vc_data$vcmax[vc_data$species == 'C. pratensis'] )
