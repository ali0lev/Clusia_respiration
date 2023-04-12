# citrate assay for samples taken from ORNL in the spring of 2016
library(ggplot2)
library(dplyr)
library(gridExtra)
# dimensions used for figure w515, h 195 (but I then tweaked it to match Licor graphs in paint)
rm(list=ls())
setwd("C:/Users/alilev93/Documents/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/ORNL work")
setwd("/Volumes/Seagate/illinois.2/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/ORNL work")


citrate_2016 <- read.csv("citrate_assay_ORNL_2016.csv", skip =2)

setwd("C:/Users/alilev93/Documents/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/from laptop desktop")
setwd("/Volumes/Seagate/illinois.2/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/from laptop desktop")



dryweights <- read.csv("Clusia_extraction_from_samples_ORNL_spring_2016.csv", stringsAsFactors = FALSE, skip = 1)
citrate_2016 <- left_join(citrate_2016, dryweights, by = c("sample" = "id"))
citrate_2016$D_OD <- citrate_2016$end_OD - citrate_2016$start_OD
citrate_2016$citrate <- (citrate_2016$D_OD - -0.005125219)/-0.003970438
citrate_2016$citrate[citrate_2016$species == "C. pratensis"] <- citrate_2016$citrate[citrate_2016$species == "C. pratensis"]
#need to change the value, which is the umol concentrattion of the cuvette, to the 

citrate_2016$citrate_dw <- citrate_2016$citrate/citrate_2016$dry_weight_in_epindorff

citrate_2016$citrate * 10

# setwd("C:/Users/alilev93/Documents/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/from laptop desktop")
setwd("/Volumes/Seagate/illinois.2/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/from laptop desktop")


ratio <- read.csv("fresh_to_dry_weight_calculated.csv", stringsAsFactors = FALSE)

citrate_2016$freshweight <- c("N")
citrate_2016$freshweight[citrate_2016$species == "C. pratensis" & citrate_2016$treatment == "drought"] <- citrate_2016$dry_weight_in_epindorff[citrate_2016$species == "C. pratensis" & citrate_2016$treatment == "drought"]*ratio$ratio[1]
citrate_2016$freshweight[citrate_2016$species == "C. pratensis" & citrate_2016$treatment == "well-watered"] <- citrate_2016$dry_weight_in_epindorff[citrate_2016$species == "C. pratensis" & citrate_2016$treatment == "well-watered"]*ratio$ratio[3]
citrate_2016$freshweight[citrate_2016$species == "C. tocuchensis" & citrate_2016$treatment == "drought"] <- citrate_2016$dry_weight_in_epindorff[citrate_2016$species == "C. tocuchensis" & citrate_2016$treatment == "drought"]*ratio$ratio[2]
citrate_2016$freshweight[citrate_2016$species == "C. tocuchensis" & citrate_2016$treatment == "well-watered"] <- citrate_2016$dry_weight_in_epindorff[citrate_2016$species == "C. tocuchensis" & citrate_2016$treatment == "well-watered"]*ratio$ratio[4]
citrate_2016$freshweight <- as.numeric(citrate_2016$freshweight)
citrate_2016$citrate_fw <- citrate_2016$citrate/citrate_2016$freshweight

# OK now I am going to calculate the area that each dry weight was equivalent to


citrate_2016$area <- c(0)
citrate_2016$area[citrate_2016$species == 'C. tocuchensis'] <- citrate_2016$dry_weight_in_epindorff[citrate_2016$species == 'C. tocuchensis'] * 77.49364
citrate_2016$area[citrate_2016$species == 'C. pratensis'] <- citrate_2016$dry_weight_in_epindorff[citrate_2016$species == 'C. pratensis'] * 76.99750
citrate_2016$citrate_area <- citrate_2016$citrate/citrate_2016$area
# the values inputted here were determined in the script 'malate_assay_from_ORNL_2016.R'


citrate_2016_se <- aggregate(.~species+time+treatment,citrate_2016[,c(-1)], function(x) sd(x)/sqrt(3))
citrate_2016_agg <- aggregate(.~species+time+treatment,citrate_2016[,c(-1)],mean)
citrate_2016_agg$citrate_dw_se <- citrate_2016_se$citrate_dw
citrate_2016_agg$citrate_fw_se <- citrate_2016_se$citrate_fw
citrate_2016_agg$citrate_area_se <- citrate_2016_se$citrate_area
citrate_2016_agg$time <- factor(citrate_2016_agg$time, levels = c( "6pm","00am", "6am", "12pm"))

citrate_2016_agg$citrate_fw_sd <- citrate_2016_agg$citrate_fw_se * sqrt(3)
citrate_2016_agg$citrate_dw_sd <- citrate_2016_agg$citrate_dw_se * sqrt(3)
citrate_2016_agg$citrate_area_sd <- citrate_2016_agg$citrate_area_se * sqrt(3)


# now I am going to capitalise well-watered and drought for the beneift of the graphs
citrate_2016_agg$treatment[citrate_2016_agg$treatment == 'well-watered'] <- 'Well-Watered'
citrate_2016_agg$treatment[citrate_2016_agg$treatment == 'drought'] <- 'Drought'
citrate_2016_agg$treatment <- factor(citrate_2016_agg$treatment, levels = c('Well-Watered', 'Drought'))

# Now for the graphs

line_graph_fw <- ggplot(data = citrate_2016_agg, aes(x = factor(time), y = citrate_fw, group = species, colour = species)) +
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(citrate_2016_agg$time[[3]]) +0.6,
            ymin = 9, ymax = Inf, colour = 'black', fill = 'white', size = 1) + 
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]),
            xmax = as.numeric(citrate_2016_agg$time[[5]]),
            ymin = 9, ymax = Inf, colour = 'grey', fill = 'grey') + 
#  theme(axis.line = element_line(colour = 'black', size = 1))
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(citrate_2016_agg$time[[3]]) +0.6,
            ymin = 9, ymax = Inf, colour = 'black', fill = NA,  size = 1) 
# Ok, so I had to manually give the rectangle the parameters to make it look tidy,
#for some reason the y axis does not go to 0, so i made it go to 9 and that fit ok
# the same is true for the size of the rectangle across the x axis; it is 0.6 more/less than where the data is
# So this graph will probs need some tinkering to get it to work for other data. 

line_graph_fw <- line_graph_fw + geom_line(aes(), size = 1.3, colour = 'black')
line_graph_fw <- line_graph_fw + geom_point(aes(fill = species), size = 4, pch = 21, colour = 'black')
line_graph_fw <- line_graph_fw + facet_grid(. ~ treatment)+ theme(strip.background = element_blank(), strip.text.x = element_blank())
line_graph_fw <- line_graph_fw + geom_errorbar(aes(ymin=citrate_fw-citrate_fw_sd, ymax=citrate_fw+citrate_fw_sd),
                                               width=0.1, 
                                               size=1,
                                               colour = "black")
line_graph_fw <- line_graph_fw + xlab("Time")
line_graph_fw <- line_graph_fw + ylab(expression(bold(atop(textstyle("Citrate"), atop('('*mu*mol~g^-1~Fwt*")")))))  

#line_graph_fw + theme(strip.text = element_text(face="italic"))
line_graph_fw <- line_graph_fw +
  theme(text = element_text(size=16 , face = 'bold'),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 9, face = 'bold'),
        legend.position = 'none')
line_graph_fw <- line_graph_fw +scale_fill_manual(values = c("grey50", "white"), name = 'Species')
line_graph_fw <- line_graph_fw + scale_x_discrete(labels=c("6pm" = "6 pm", "6am" = "6 am", '12pm' = '12 pm', '00am' = '00 am'))



line_graph_fw


line_graph_dw <- ggplot(data = citrate_2016_agg, aes(x = factor(time), y = citrate_dw, group = species, colour = species)) +
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(citrate_2016_agg$time[[3]]) +0.6,
            ymin = -90, ymax = Inf, colour = 'black', fill = 'white', size = 1) + 
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]),
            xmax = as.numeric(citrate_2016_agg$time[[5]]),
            ymin = -90, ymax = Inf, colour = 'grey', fill = 'grey') + 
  #  theme(axis.line = element_line(colour = 'black', size = 1))
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(citrate_2016_agg$time[[3]]) +0.6,
            ymin = -90, ymax = Inf, colour = 'black', fill = NA,  size = 1) 
# Ok, so I had to manually give the rectangle the parameters to make it look tidy,
#for some reason the y axis does not go to 0, so i made it go to 9 and that fit ok
# the same is true for the size of the rectangle across the x axis; it is 0.6 more/less than where the data is
# So this graph will probs need some tinkering to get it to work for other data. 

line_graph_dw <- line_graph_dw + geom_line(aes(), size = 1.3, colour = 'black')
line_graph_dw <- line_graph_dw + geom_point(aes(fill = species), size = 4, pch = 21, colour = 'black')
line_graph_dw <- line_graph_dw + facet_grid(. ~ treatment)+ theme(strip.background = element_blank(), strip.text.x = element_blank())
line_graph_dw <- line_graph_dw + geom_errorbar(aes(ymin=citrate_dw-citrate_dw_sd, ymax=citrate_dw+citrate_dw_sd),
                                               width=0.1, 
                                               size=1,
                                               colour = "black")
line_graph_dw <- line_graph_dw + xlab("Time")
line_graph_dw <- line_graph_dw + ylim(0,1700)
line_graph_dw <- line_graph_dw +  ylab(expression(bold(atop(textstyle("Citrate"), atop('('*mu*mol~g^-1~Dwt*")")))))  

#line_graph_dw + theme(strip.text = element_text(face="italic"))
line_graph_dw <- line_graph_dw +
  theme(text = element_text(size=16, face = 'bold'),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 9, face = 'bold'),
        legend.position = 'none')
line_graph_dw <- line_graph_dw +scale_fill_manual(values = c("grey50", "white"), name = 'Species')
line_graph_dw <- line_graph_dw + scale_x_discrete(labels=c("6pm" = "6 pm", "6am" = "6 am", '12pm' = '12 pm', '00am' = '00 am'))


line_graph_dw


grid.arrange(line_graph_dw, line_graph_fw)


line_graph_area <- ggplot(data = citrate_2016_agg, aes(x = factor(time), y = citrate_area*10, group = species, colour = species)) +
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(citrate_2016_agg$time[[3]]) +0.6,
            ymin = -10, ymax = Inf, colour = 'black', fill = 'white', size = 1) + 
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]),
            xmax = as.numeric(citrate_2016_agg$time[[5]]),
            ymin = -10, ymax = Inf, colour = 'grey', fill = 'grey') + 
  #  theme(axis.line = element_line(colour = 'black', size = 1))
  geom_rect(data = citrate_2016_agg, aes(),
            xmin = as.numeric(citrate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(citrate_2016_agg$time[[3]]) +0.6,
            ymin = -10, ymax = Inf, colour = 'black', fill = NA,  size = 1) 
# Ok, so I had to manually give the rectangle the parameters to make it look tidy,
#for some reason the y axis does not go to 0, so i made it go to 9 and that fit ok
# the same is true for the size of the rectangle across the x axis; it is 0.6 more/less than where the data is
# So this graph will probs need some tinkering to get it to work for other data. 

line_graph_area <- line_graph_area + geom_line(aes(), size = 1.3, colour = 'black')
line_graph_area <- line_graph_area + geom_point(aes(fill = species), size = 4, pch = 21, colour = 'black')
line_graph_area <- line_graph_area + facet_grid(. ~ treatment)+ theme(strip.background = element_blank(), strip.text.x = element_blank())
line_graph_area <- line_graph_area + geom_errorbar(aes(ymin=citrate_area*10-citrate_area_sd*10, ymax=citrate_area*10+citrate_area_sd*10),
                                                   width=0.1, 
                                                   size=1,
                                                   colour = "black")
line_graph_area <- line_graph_area + xlab("Time")
line_graph_area <- line_graph_area + ylim(0,220)
line_graph_area <- line_graph_area + ylab(expression(bold(atop(textstyle("Citrate"), atop('('*mmol~m^-2*")")))))  
#line_graph_area + theme(strip.text = element_text(face="italic"))
line_graph_area <- line_graph_area +
  theme(text = element_text(size=16, face = 'bold'),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 9, face = 'bold'),
        legend.position = 'none')
line_graph_area <- line_graph_area +scale_fill_manual(values = c("grey50", "white"), name = 'Species')
line_graph_area <- line_graph_area + scale_x_discrete(labels=c("6pm" = "6 pm", "6am" = "6 am", '12pm' = '12 pm', '00am' = '00 am'))


line_graph_area



###############################################################################################################################################

# Now the t-tests which I am using for the paper, 
# these just compare dawn and dusk, for simplicity. 


well_watered_pratensis_dw <- t.test(citrate_2016$citrate_dw[citrate_2016$time == '6am' & citrate_2016$treatment == 'well-watered' & citrate_2016$species == 'C. pratensis'],
                                    citrate_2016$citrate_dw[citrate_2016$time == '6pm' & citrate_2016$treatment == 'well-watered' & citrate_2016$species == 'C. pratensis'],
                                    paired = FALSE, alternative = 'greater' )


drought_pratensis_dw <- t.test(citrate_2016$citrate_dw[citrate_2016$time == '6am' & citrate_2016$treatment == 'drought' & citrate_2016$species == 'C. pratensis'],
                               citrate_2016$citrate_dw[citrate_2016$time == '6pm' & citrate_2016$treatment == 'drought' & citrate_2016$species == 'C. pratensis'],
                               paired = FALSE, alternative = 'greater' )



well_watered_tocuchensis_dw <- t.test(citrate_2016$citrate_dw[citrate_2016$time == '6am' & citrate_2016$treatment == 'well-watered' & citrate_2016$species == 'C. tocuchensis'],
                                      citrate_2016$citrate_dw[citrate_2016$time == '6pm' & citrate_2016$treatment == 'well-watered' & citrate_2016$species == 'C. tocuchensis'],
                                      paired = FALSE, alternative = 'greater' )


drought_tocuchensis_dw <- t.test(citrate_2016$citrate_dw[citrate_2016$time == '6am' & citrate_2016$treatment == 'drought' & citrate_2016$species == 'C. tocuchensis'],
                                 citrate_2016$citrate_dw[citrate_2016$time == '6pm' & citrate_2016$treatment == 'drought' & citrate_2016$species == 'C. tocuchensis'],
                                 paired = FALSE, alternative = 'greater' )



##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########
##########
########## ok now I am going to make a csv file which I can export and use in the r script 'malate_assay_from_ORNL.R'
########## This way I can make the bar graphs for AoB as one single facet graph

row.names(citrate_2016_agg) <- NULL
write.csv(citrate_2016_agg, 'citrate_data_to_add_to_malate_script_for_AoB_bar_graphs.csv')
write.csv(citrate_2016_se, 'citrate_errors_data_to_add_to_malate_script_for_AoB_bar_graphs.csv')

