# malate assay for ORNL samples from 2016
# export dimensions w520 h195 
library(ggplot2)
library(dplyr)
library(gridExtra)

# rm(list=ls())

prat_colour <- 'grey50'
tocu_colour <- 'white'

setwd("C:/Users/alilev93/Documents/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/from laptop desktop")
setwd("/Volumes/Seagate/illinois.2/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/from laptop desktop")

malate_2016 <- read.csv("malate_assay_from_ORNL_2016.csv", skip =2)

dryweights <- read.csv("Clusia_extraction_from_samples_ORNL_spring_2016.csv", stringsAsFactors = FALSE, skip = 1)
malate_2016 <- left_join(malate_2016, dryweights, by = c("sample" = "id"))
malate_2016$D_OD <- malate_2016$end_OD - malate_2016$start_OD
malate_2016$malate <- (malate_2016$D_OD )*221 + 3
(malate_2016$D_OD - 0.007877466)/0.005833588
(malate_2016$D_OD )*216 + 10


malate_2016$malate[malate_2016$species == "C. pratensis"] <- malate_2016$malate[malate_2016$species == "C. pratensis"]
#need to change the value, which is the umol concentrattion of the cuvette, to the 
#Ok this so far gives me the amount of malate (in nMoles) of malate in the cuvette. 
# I now need to work out the amount that was in the buffered solution. 
# So from my notebook i can see that for each sample I suspended the plant material in 400uL of buffer.
# and then I added 10uL to the cuvettes for prat and 20uL for tocu. 
# So I need to multiply the value of malate by 40 for prat and 20 for tocu
malate_2016$malate[malate_2016$species == "C. pratensis"] <- malate_2016$malate[malate_2016$species == "C. pratensis"]*40
malate_2016$malate[malate_2016$species == "C. tocuchensis"] <- malate_2016$malate[malate_2016$species == "C. tocuchensis"]*20

# Then this 400uL sample comes from a bigger sample that was 1500uL. 
# So i need to multilpy both species by 3.75 (3.75*400 = 1500). 
malate_2016$malate <- malate_2016$malate*3.75

# So this gives me the nMoles that were extracted from the total tissue sample
# then to get this to uM i need to divide by 1000

malate_2016$malate <- malate_2016$malate/1000

#malate_2016$malate <- malate_2016$malate/1000

malate_2016$malate_dw <- malate_2016$malate/malate_2016$dry_weight_in_epindorff



setwd("//campus/rdw/Biology/b5054536/copied_from_laptop/ORNL work")
ratio <- read.csv("fresh_to_dry_weight_calculated.csv", stringsAsFactors = FALSE)

malate_2016$freshweight <- c("N")
malate_2016$freshweight[malate_2016$species == "C. pratensis" & malate_2016$treatment == "drought"] <- malate_2016$dry_weight_in_epindorff[malate_2016$species == "C. pratensis" & malate_2016$treatment == "drought"]*ratio$ratio[1]
malate_2016$freshweight[malate_2016$species == "C. pratensis" & malate_2016$treatment == "well-watered"] <- malate_2016$dry_weight_in_epindorff[malate_2016$species == "C. pratensis" & malate_2016$treatment == "well-watered"]*ratio$ratio[3]
malate_2016$freshweight[malate_2016$species == "C. tocuchensis" & malate_2016$treatment == "drought"] <- malate_2016$dry_weight_in_epindorff[malate_2016$species == "C. tocuchensis" & malate_2016$treatment == "drought"]*ratio$ratio[2]
malate_2016$freshweight[malate_2016$species == "C. tocuchensis" & malate_2016$treatment == "well-watered"] <- malate_2016$dry_weight_in_epindorff[malate_2016$species == "C. tocuchensis" & malate_2016$treatment == "well-watered"]*ratio$ratio[4]
malate_2016$freshweight <- as.numeric(malate_2016$freshweight)
malate_2016$malate_fw <- malate_2016$malate/malate_2016$freshweight

# Now I am going to calculate the data on a per area basis

setwd('C:/Users/alilev93/Documents/For_PhD/respiration paper/copied_from_laptop/copied_from_laptop/ORNL work')
ratio2 <- read.csv('fresh_to_dry_weight.csv', skip = 1)
# all the leaf disks I used for this fwt to dwt ratio were 10 cm squared, 
# so i can work out the ratio of dwt to area for each species
ratio2$x <- 10/ratio2$dryweight

ratio2$dryweight/10*10000
ratio2_agg <- aggregate(.~species, data = ratio2, FUN = mean)

# so ratio2_ag$x gives me the conversion  value to turn dwt into area

malate_2016$area <- c(0)
malate_2016$area[malate_2016$species == 'C. tocuchensis'] <- malate_2016$dry_weight_in_epindorff[malate_2016$species == 'C. tocuchensis'] * 77.49364
malate_2016$area[malate_2016$species == 'C. pratensis'] <- malate_2016$dry_weight_in_epindorff[malate_2016$species == 'C. pratensis'] * 76.99750
malate_2016$malate_area <- malate_2016$malate/malate_2016$area

malate_2016$dry_weight_in_epindorff[malate_2016$species == 'C. tocuchensis'] * 77.49364
malate_2016$dry_weight_in_epindorff[malate_2016$species == 'C. pratensis'] * 76.99750
malate_2016$dry_weight_in_epindorff[malate_2016$species == 'C. pratensis']/200


# so this gives the value in uMol per cm^2
# I will do some conversions in the graph to make it into mMol per m^2

malate_2016_se <- aggregate(.~species+time+treatment,malate_2016[,c(-1)], function(x) sd(x)/sqrt(3))
malate_2016_agg <- aggregate(.~species+time+treatment,malate_2016[,c(-1)],mean)
malate_2016_agg$malate_dw_se <- malate_2016_se$malate_dw
malate_2016_agg$malate_fw_se <- malate_2016_se$malate_fw
malate_2016_agg$malate_area_se <- malate_2016_se$malate_area
malate_2016_agg$time <- factor(malate_2016_agg$time, levels = c( "6pm","00am", "6am", "12pm"))

malate_2016_agg$malate_fw_sd <- malate_2016_agg$malate_fw_se * sqrt(3)
malate_2016_agg$malate_dw_sd <- malate_2016_agg$malate_dw_se * sqrt(3)
malate_2016_agg$malate_area_sd <- malate_2016_agg$malate_area_se * sqrt(3)


# now I am going to capitalise well-watered and drought for the beneift of the graphs
malate_2016_agg$treatment[malate_2016_agg$treatment == 'well-watered'] <- 'Well-Watered'
malate_2016_agg$treatment[malate_2016_agg$treatment == 'drought'] <- 'Drought'
malate_2016_agg$treatment <- factor(malate_2016_agg$treatment, levels = c('Well-Watered', 'Drought'))





# Ok now I am going to make the graphs

line_graph_fw <- ggplot(data = malate_2016_agg, aes(x = factor(time), y = malate_fw, group = species, colour = species)) +
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(malate_2016_agg$time[[3]]) +0.6,
            ymin = -1.5, ymax = Inf, colour = 'black', fill = 'white', size = 1) + 
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]),
            xmax = as.numeric(malate_2016_agg$time[[5]]),
            ymin = -1.5, ymax = Inf, colour = 'grey80', fill = 'grey80') + 
  #  theme(axis.line = element_line(colour = 'black', size = 1))
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(malate_2016_agg$time[[3]]) +0.6,
            ymin = -1, ymax = Inf, colour = 'black', fill = NA,  size = 1) 
# Ok, so I had to manually give the rectangle the parameters to make it look tidy,
#for some reason the y axis does not go to 0, so i made it go to -1.5 and that fit ok
# the same is true for the size of the rectangle across the x axis; it is 0.6 more/less than where the data is
# So this graph will probs need some tinkering to get it to work for other data. 

line_graph_fw <- line_graph_fw + geom_line(aes(), size = 1.3, colour = 'black')
line_graph_fw <- line_graph_fw + geom_point(aes(fill = species), size = 4, pch = 21, colour = 'black')
line_graph_fw <- line_graph_fw + geom_errorbar(aes(ymin=malate_fw-malate_fw_sd, ymax=malate_fw+malate_fw_sd),
                                               width=0.1, 
                                               size=1,
                                               colour = "black")
#line_graph_fw <- line_graph_fw + geom_point(aes(), size = 2, colour = 'white')
line_graph_fw <- line_graph_fw + facet_grid(. ~ treatment) + theme(strip.background = element_blank(), strip.text.x = element_blank())

line_graph_fw <- line_graph_fw + xlab(expression(bold("Time")))
line_graph_fw <- line_graph_fw + ylab(expression(bold(atop(textstyle("Malate"), atop('('*mu*mol~g^-1~Fwt*")")))))  
#line_graph_fw + theme(strip.text = element_text(face="italic"))
line_graph_fw <- line_graph_fw +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 9, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        legend.position = 'none')
line_graph_fw <- line_graph_fw +scale_fill_manual(values = c( "grey50", "white"), name = 'Species')

line_graph_fw <- line_graph_fw + scale_x_discrete(labels=c("6pm" = "6 pm", "6am" = "6 am", '12pm' = '12 pm', '00am' = '00 am'))
                                           
line_graph_fw



line_graph_dw <- ggplot(data = malate_2016_agg, aes(x = factor(time), y = malate_dw, group = species, colour = species)) +
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(malate_2016_agg$time[[3]]) +0.6,
            ymin = -12, ymax = Inf, colour = 'black', fill = 'white', size = 1) + 
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]),
            xmax = as.numeric(malate_2016_agg$time[[5]]),
            ymin = -12, ymax = Inf, colour = 'grey80', fill = 'grey80') + 
  #  theme(axis.line = element_line(colour = 'black', size = 1))
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(malate_2016_agg$time[[3]]) +0.6,
            ymin = -12, ymax = Inf, colour = 'black', fill = NA,  size = 1) 
# Ok, so I had to manually give the rectangle the parameters to make it look tidy,
#for some reason the y axis does not go to 0, so i made it go to -1.5 and that fit ok
# the same is true for the size of the rectangle across the x axis; it is 0.6 more/less than where the data is
# So this graph will probs need some tinkering to get it to work for other data. 

line_graph_dw <- line_graph_dw + geom_line(aes(), size = 1.3, colour = 'black')
line_graph_dw <- line_graph_dw + geom_point(aes(fill = species), size = 4, pch = 21, colour = 'black')
line_graph_dw <- line_graph_dw + geom_errorbar(aes(ymin=malate_dw-malate_dw_sd, ymax=malate_dw+malate_dw_sd),
                                               width=0.1, 
                                               size=1,
                                               colour = "black")
#line_graph_dw <- line_graph_dw + geom_point(aes(), size = 2, colour = 'white')
line_graph_dw <- line_graph_dw + facet_grid(. ~ treatment) + theme(strip.background = element_blank(), strip.text.x = element_blank())

line_graph_dw <- line_graph_dw + xlab(expression(bold("Time")))
line_graph_dw <- line_graph_dw + ylab(expression(bold(atop(textstyle("Malate"), atop('('*mu*mol~g^-1~Dwt*")"))))) 
#line_graph_dw + theme(strip.text = element_text(face="italic"))
line_graph_dw <- line_graph_dw +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 9, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        legend.position = 'none')
line_graph_dw <- line_graph_dw +scale_fill_manual(values = c( "grey50", "white"), name = 'Species')

line_graph_dw <- line_graph_dw + scale_x_discrete(labels=c("6pm" = "6 pm", "6am" = "6 am", '12pm' = '12 pm', '00am' = '00 am'))


line_graph_dw




#### Now area graph


line_graph_area <- ggplot(data = malate_2016_agg, aes(x = factor(time), y = malate_area*10, group = species, colour = species)) +
  # I am multiplying the value of malate_area by 10 to convert it from uMol cm^-2 to mMol m^-2 
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(malate_2016_agg$time[[3]]) +0.6,
            ymin = -1.5, ymax = Inf, colour = 'black', fill = 'white', size = 1) + 
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]),
            xmax = as.numeric(malate_2016_agg$time[[5]]),
            ymin = -1.5, ymax = Inf, colour = 'grey80', fill = 'grey80') + 
  #  theme(axis.line = element_line(colour = 'black', size = 1))
  geom_rect(data = malate_2016_agg, aes(),
            xmin = as.numeric(malate_2016_agg$time[[7]]) -0.6,  # I am doing this a bit of a crude way (by giving the rectangles borders, because i cba to find a clever way to do it)
            xmax = as.numeric(malate_2016_agg$time[[3]]) +0.6,
            ymin = -1, ymax = Inf, colour = 'black', fill = NA,  size = 1) 
# Ok, so I had to manually give the rectangle the parameters to make it look tidy,
#for some reason the y axis does not go to 0, so i made it go to -1.5 and that fit ok
# the same is true for the size of the rectangle across the x axis; it is 0.6 more/less than where the data is
# So this graph will probs need some tinkering to get it to work for other data. 

line_graph_area <- line_graph_area + geom_line(aes(), size = 1.3, colour = 'black')
line_graph_area <- line_graph_area + geom_point(aes(fill = species), size = 4, pch = 21, colour = 'black')
line_graph_area <- line_graph_area + geom_errorbar(aes(ymin=malate_area*10-malate_area_sd*10, ymax=malate_area*10+malate_area_sd*10),
                                                   width=0.1, 
                                                   size=1,
                                                   colour = "black")
#line_graph_area <- line_graph_area + geom_point(aes(), size = 2, colour = 'white')
line_graph_area <- line_graph_area + facet_grid(. ~ treatment) + theme(strip.background = element_blank(), strip.text.x = element_blank())

line_graph_area <- line_graph_area + xlab(expression(bold("Time")))
line_graph_area <- line_graph_area + ylab(expression(bold(atop(textstyle("Malate"), atop('('*mmol~m^-2*")")))))  

#line_graph_area + theme(strip.text = element_text(face="italic"))
line_graph_area <- line_graph_area +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 9, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        legend.position = 'none')
line_graph_area <- line_graph_area +scale_fill_manual(values = c( "grey50", "white"), name = 'Species')
line_graph_area <- line_graph_area + scale_x_discrete(labels=c("6pm" = "6 pm", "6am" = "6 am", '12pm' = '12 pm', '00am' = '00 am'))


line_graph_area
line_graph_dw
line_graph_fw













#### Ok now I am going to do some t-tests (between dawn and dusk) to include in my figure legends in my thesis chapter



well_watered_pratensis_fw <- t.test(malate_2016$malate_fw[malate_2016$time == '6am' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. pratensis'],
                                    malate_2016$malate_fw[malate_2016$time == '6pm' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. pratensis'],
                                    paired = FALSE, alternative = 'greater' )


drought_pratensis_fw <- t.test(malate_2016$malate_fw[malate_2016$time == '6am' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. pratensis'],
                               malate_2016$malate_fw[malate_2016$time == '6pm' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. pratensis'],
                               paired = FALSE, alternative = 'greater' )



well_watered_tocuchensis_fw <- t.test(malate_2016$malate_fw[malate_2016$time == '6am' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. tocuchensis'],
                                      malate_2016$malate_fw[malate_2016$time == '6pm' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. tocuchensis'],
                                      paired = FALSE, alternative = 'greater' )


drought_tocuchensis_fw <- t.test(malate_2016$malate_fw[malate_2016$time == '6am' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. tocuchensis'],
                                 malate_2016$malate_fw[malate_2016$time == '6pm' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. tocuchensis'],
                                 paired = FALSE, alternative = 'greater' )





well_watered_pratensis_area <- t.test(malate_2016$malate_area[malate_2016$time == '6am' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. pratensis'],
                                      malate_2016$malate_area[malate_2016$time == '6pm' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. pratensis'],
                                      paired = FALSE, alternative = 'greater' )


drought_pratensis_area <- t.test(malate_2016$malate_area[malate_2016$time == '6am' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. pratensis'],
                                 malate_2016$malate_area[malate_2016$time == '6pm' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. pratensis'],
                                 paired = FALSE, alternative = 'greater' )



well_watered_tocuchensis_area <- t.test(malate_2016$malate_area[malate_2016$time == '6am' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. tocuchensis'],
                                        malate_2016$malate_area[malate_2016$time == '6pm' & malate_2016$treatment == 'well-watered' & malate_2016$species == 'C. tocuchensis'],
                                        paired = FALSE, alternative = 'greater' )


drought_tocuchensis_area <- t.test(malate_2016$malate_area[malate_2016$time == '6am' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. tocuchensis'],
                                   malate_2016$malate_area[malate_2016$time == '6pm' & malate_2016$treatment == 'drought' & malate_2016$species == 'C. tocuchensis'],
                                   paired = FALSE, alternative = 'greater' )












# Ok now I am going to make bar graphs, which eliminate the midday and midnight data - as they muddy the waters a bit

# First I am going to add data from the script 'citrate_assay_ORNL_2016.R'
# This data has been processed in this script, so I can add it to the malate_2016_agg data frame

citrate_2016_agg <- read.csv('citrate_data_to_add_to_malate_script_for_AoB_bar_graphs.csv')
citrate_2016_agg <- citrate_2016_agg[,-1]

colnames(malate_2016_agg)[c(9,10,12,14,15,16,17,18,19,20)] <- c('conc', 'conc_dw', 'conc_fw', 'conc_area', 'conc_dw_se', 'conc_fw_se', 'conc_area_se', 'conc_fw_sd', 'conc_dw_sd', 'conc_area_sd')
colnames(citrate_2016_agg)[c(9,10,12,14,15,16,17,18,19,20)] <- c('conc', 'conc_dw', 'conc_fw', 'conc_area', 'conc_dw_se', 'conc_fw_se', 'conc_area_se', 'conc_fw_sd', 'conc_dw_sd', 'conc_area_sd')

malate_2016_agg$metabolite <- c('malate')
citrate_2016_agg$metabolite <- c('citrate')

both_metabolites_agg <- rbind(malate_2016_agg, citrate_2016_agg)


ggplot(data = both_metabolites_agg[both_metabolites_agg$time != '00am' & both_metabolites_agg$time != '12pm' ,], aes(x = factor(time), y = conc_area*10, group = species,  fill = species)) +
  theme_classic() +
  geom_bar(colour = 'black' , stat = 'identity', size = 1.1) +
  geom_errorbar(aes(ymin=conc_area*10-conc_area_sd*10, ymax=conc_area*10+conc_area_sd*10),
                width=0.3, 
                size=1,
                colour = "black") +
  xlab(expression(bold("Time"))) +
  ylab(expression(bold("Metabolite Concentration "~'('*mmol~m^-2*")"))) +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text.x  = element_text(face = "bold.italic"),
        strip.text.y  = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = c( "grey50", "white"), name = 'Species') +
  facet_grid(metabolite + treatment ~ species)


####### I'm actually not that happy with this, because there is so much more malate than citrate that it makes it hard to see the malate data

# So I think I am actually going to make the graphs seperately, and then grid.arrange them together


malate_bargraph_area <- ggplot(data = both_metabolites_agg[both_metabolites_agg$time != '00am' & both_metabolites_agg$time != '12pm' &  both_metabolites_agg$metabolite == 'malate',], aes(x = factor(time), y = conc_area*10, group = species,  fill = species)) +
  theme_classic() +
  geom_bar(colour = 'black' , stat = 'identity', size = 1.1) +
  geom_errorbar(aes(ymin=conc_area*10-conc_area_sd*10, ymax=conc_area*10+conc_area_sd*10),
                width=0.3, 
                size=1,
                colour = "black") +
  xlab(expression(bold("Time"))) +
  ylab(expression(bold("Malate "~'('*mmol~m^-2*")"))) +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text.x  = element_text(face = "bold.italic"),
        strip.text.y  = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = c( "grey50", "white"), name = 'Species') +
  facet_grid(treatment ~ species)

citrate_bargraph_area <- ggplot(data = both_metabolites_agg[both_metabolites_agg$time != '00am' & both_metabolites_agg$time != '12pm' &  both_metabolites_agg$metabolite == 'citrate',], aes(x = factor(time), y = conc_area*10, group = species,  fill = species)) +
  theme_classic() +
  geom_bar(colour = 'black' , stat = 'identity', size = 1.1) +
  geom_errorbar(aes(ymin=conc_area*10-conc_area_sd*10, ymax=conc_area*10+conc_area_sd*10),
                width=0.3, 
                size=1,
                colour = "black") +
  xlab(expression(bold("Time"))) +
  ylab(expression(bold("Citrate "~'('*mmol~m^-2*")"))) +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text.x  = element_text(face = "bold.italic"),
        strip.text.y  = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = c( "grey50", "white"), name = 'Species') +
  facet_grid(treatment ~ species)

##########################
# Now doing the same graphs, but on a per fw basis

malate_bargraph_fw <- ggplot(data = both_metabolites_agg[both_metabolites_agg$time != '00am' & both_metabolites_agg$time != '12pm' &  both_metabolites_agg$metabolite == 'malate',], aes(x = factor(time), y = conc_fw*10, group = species,  fill = species)) +
  theme_classic() +
  geom_bar(colour = 'black' , stat = 'identity', size = 1.1) +
  geom_errorbar(aes(ymin=conc_fw*10-conc_fw_sd*10, ymax=conc_fw*10+conc_fw_sd*10),
                width=0.3, 
                size=1,
                colour = "black") +
  xlab(expression(bold("Time"))) +
  ylab(expression(bold("Malate "~'('*mu*mol~g^-1~Fwt*")"))) +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text.x  = element_text(face = "bold.italic"),
        strip.text.y  = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = c( "grey50", "white"), name = 'Species') +
  facet_grid(treatment ~ species)

citrate_bargraph_fw <- ggplot(data = both_metabolites_agg[both_metabolites_agg$time != '00am' & both_metabolites_agg$time != '12pm' &  both_metabolites_agg$metabolite == 'citrate',], aes(x = factor(time), y = conc_fw*10, group = species,  fill = species)) +
  theme_classic() +
  geom_bar(colour = 'black' , stat = 'identity', size = 1.1) +
  geom_errorbar(aes(ymin=conc_fw*10-conc_fw_sd*10, ymax=conc_fw*10+conc_fw_sd*10),
                width=0.3, 
                size=1,
                colour = "black") +
  xlab(expression(bold("Time"))) +
  ylab(expression(bold("Citrate "~'('*mu*mol~g^-1~Fwt*")"))) +
  theme(text = element_text(size=17),
        legend.text = element_text(face = 'italic'),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        legend.position = 'none',
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text.x  = element_text(face = "bold.italic"),
        strip.text.y  = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = c( "grey50", "white"), name = 'Species') +
  facet_grid(treatment ~ species)






grid.arrange(malate_bargraph_area, citrate_bargraph_area)
grid.arrange(malate_bargraph_fw, citrate_bargraph_fw)

# saving images with dimenstions w = 640, h = 840

  