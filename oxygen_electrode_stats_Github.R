

#rm(list=ls())

library('ggplot2')
library("multcomp")
library('gridExtra')
library('agricolae')
library('dplyr')
library('reshape2')

#first want to see if i can reformat straight from the output files of the Hansatech oxygraph. 
setwd("~/For_PhD/respiration paper/Data_from_the_machinie/Data_from_the_machinie")
setwd("/Volumes/Seagate/illinois.2/For_PhD/respiration paper/Data_from_the_machinie/Data_from_the_machinie")

a=read.csv("a_toc-.csv")
a$row=row(a)
class(a)
start=a$HANSATECH.INSTRUMENTS...O2view.Recording[(a$row[a$HANSATECH.INSTRUMENTS...O2view.Recording=="300.0"])+1]
end = a$HANSATECH.INSTRUMENTS...O2view.Recording[(a$row[a$HANSATECH.INSTRUMENTS...O2view.Recording=="600.0"])+1]
start=as.numeric(paste(start))
end = as.numeric(paste(end))
rate = (start-end)/300
#so what this does is:
#take the csv file from Hansatech, which is in a rubish format
#find the rownumber of the 300th second. designate start = the conents of that rownumber+1. this is because the next row down is the O2 content.
#do the same thing for finding the O2 content at 600 seconds
#calculate the rate of the change


#i have now made the above code into a function so i can do it on all the files. 
getrate= function (x) {
              x$row=row(x)
              start=x$HANSATECH.INSTRUMENTS...O2view.Recording[(x$row[x$HANSATECH.INSTRUMENTS...O2view.Recording=="300.0"])+1]
              end = x$HANSATECH.INSTRUMENTS...O2view.Recording[(x$row[x$HANSATECH.INSTRUMENTS...O2view.Recording=="600.0"])+1]
              start=as.numeric(paste(start))
              end = as.numeric(paste(end))
              x = (start-end)/300
              return(x)
              
}



list.files()
#make sure all the xcel files are not open when doing this.


#now this is a for loop to find the rates for each sample
qq=NULL
for (z in (list.files()[1:12])){
  qq=c(qq,getrate(read.csv(z)))
}
qq

p=read.csv("night time measurements.csv", skip=1, header=TRUE)
condition=c("minus_H2O","plus_H2O" ,"minus_H2O", "plus_H2O","plus_H2O", "minus_H2O","minus_H2O","plus_H2O","plus_H2O", "minus_H2O", "plus_H2O","minus_H2O")
species=c("toc", "prat", "prat", "toc", "prat","prat", "toc", "toc", "prat", "prat", "toc","toc")
df=data.frame(sample_code =letters[1:12] , rate=qq, species = species, condition=condition, time = p$time, freshweight=p$weight..g.)
#this generates a dataframe which contains all of the rates for each sample
#now i can start doing some stats.

#need to normalise the rate, based on the freshweight
df$fw_rate=df$rate/df$freshweight

df$condition=factor(df$condition, levels = c("plus_H2O","minus_H2O"))



summary_df=aggregate(fw_rate~species+condition, df, FUN=mean)
summary_df$SE <- c(
  sd(df$fw_rate[df$species=="prat" & df$condition=="plus_H2O"])/sqrt(3),
  sd(df$fw_rate[df$species=="toc" & df$condition=="plus_H2O"])/sqrt(3),
  sd(df$fw_rate[df$species=="prat" & df$condition=="minus_H2O"])/sqrt(3),
  sd(df$fw_rate[df$species=="toc" & df$condition=="minus_H2O"])/sqrt(3)
)
summary_df$SD <- c(
  sd(df$fw_rate[df$species=="prat" & df$condition=="plus_H2O"]),
  sd(df$fw_rate[df$species=="toc" & df$condition=="plus_H2O"]),
  sd(df$fw_rate[df$species=="prat" & df$condition=="minus_H2O"]),
  sd(df$fw_rate[df$species=="toc" & df$condition=="minus_H2O"])
)
#this is a summary of the mean of each species*treatment combo

# Now I am going to make a temp file, which I will join to the dw and area data to make a facet_grid graph

fw_summary <- summary_df
fw_summary$temp <- paste0(fw_summary$species, fw_summary$condition)

ggplot(data=summary_df, aes(x=condition, y=fw_rate, colour=species))+geom_point()
graph1=ggplot(data=df, aes(x=condition, y=fw_rate, colour=species))+geom_point()+geom_boxplot()

aov=aov(fw_rate~species*condition, data=df)
summary(aov)
#shows that there is a significant interaction between species and condition


#now i want to make a bar graph as a different way of looking at the data.
graph2 = ggplot(data = summary_df, aes(x=condition, y=fw_rate*1000, group=species, fill=species))

graph2 <-  graph2 + theme_classic()

graph2 = graph2 + geom_bar(stat="identity", width = 0.5, position = "dodge", colour = 'black', size =0.8)
graph2=graph2+ylab(expression(bold(atop(textstyle("Rate of"~ O[2]~"Consumption"),atop("("*"nmoles "~s^-1~g^-1~Fwt*")")))))

graph2=graph2+xlab("Condition")
graph2 = graph2 + scale_x_discrete(labels=c("well watered", "droughted"))
graph2 = graph2+scale_fill_manual(values = c( "grey50", "white"),
                                    name ="Species",
                           labels=c(expression(bold(paste(italic("C. pratensis")))),expression(bold(paste(italic("C. tocuchensis"))))))
graph2 <- graph2+geom_errorbar(aes(ymin=fw_rate*1000-SD*1000, ymax=fw_rate*1000+SD*1000),
                     width=0.15, 
                     size=1,
                     position=position_dodge(.5))
graph2 <- graph2 + theme(axis.text=element_text(size=18),
                         axis.title = element_text( color="black", face="bold", size=18),
                         legend.text = element_text(size=16),
                         legend.title = element_text(size=18),
                         legend.text.align = 0
)
graph2 <- graph2 + geom_point(data = df, aes(x=condition, y=fw_rate*1000, fill = species), shape = 21, colour = 'black',size = 2,stroke = 1.5,  position = position_jitterdodge(jitter.width = 0.2, jitter.height=0, dodge.width = 1.3)) 

graph2

# this adds the error bars, which show the standard deviation of the mean. 


df$species_and_condition <- paste0(df$species, df$condition)
# this is to make a variable with which to do Tukey Kramer analysis

tov=aov(fw_rate~condition*species, data=df)
summary(tov)
TukeyHSD(tov)
tukey.test <- HSD.test(aov, trt = c('condition', 'species'))
tukey.test

#graph2 now needs to have the letters added above the bars to represent the classes of the Tukey-Kramer analysis
TK_letters_FW <- c('x','x','y','x')
graph2 <- graph2 +
  ylim(0,2) +
  geom_text(aes(label=TK_letters_FW), position=position_dodge(width=0.5), vjust=-4.5)


graph2

# now i want to repeat but calculaing it based on dry weight 
setwd("~/For_PhD/respiration paper/Data_from_the_machinie")

fwtdw <- read.csv("fresh_to_dry_weight.csv",header=TRUE, stringsAsFactors = FALSE, skip =1)
fwtdw$ratio <- fwtdw$freshweight/fwtdw$dryweight
fwtdw <- aggregate(.~species+condition, data = fwtdw, mean)
fwtdw <- fwtdw[c(1,2,6)]
fwtdw <- fwtdw[order(fwtdw$species),]
# so by dividing the rate by the $ratio, i can work out the oxygen consumption per units of dry weights

oc <- 7
fw <- 6
dw <- 1.2
ratio <- fw/dw

fwr <- oc/fw
dwr <- oc/(1.2)
fwr*ratio

# So first go back to the df data.frame

df$dw_rate <- c(0)
df$dw_rate[df$species== "prat" & df$condition == "plus_H2O"] <- (df$fw_rate[df$species== "prat" & df$condition == "plus_H2O"])*fwtdw[2,3]
df$dw_rate[df$species== "prat" & df$condition == "minus_H2O"] <- (df$fw_rate[df$species== "prat" & df$condition == "minus_H2O"])*fwtdw[1,3]
df$dw_rate[df$species== "toc" & df$condition == "plus_H2O"] <- (df$fw_rate[df$species== "toc" & df$condition == "plus_H2O"])*fwtdw[4,3]
df$dw_rate[df$species== "toc" & df$condition == "minus_H2O"] <- (df$fw_rate[df$species== "toc" & df$condition == "minus_H2O"])*fwtdw[3,3]


summary_df=aggregate(dw_rate~species+condition, df, FUN=mean)
summary_df$SE <- c(
  sd(df$dw_rate[df$species=="prat" & df$condition=="plus_H2O"])/sqrt(3),
  sd(df$dw_rate[df$species=="toc" & df$condition=="plus_H2O"])/sqrt(3),
  sd(df$dw_rate[df$species=="prat" & df$condition=="minus_H2O"])/sqrt(3),
  sd(df$dw_rate[df$species=="toc" & df$condition=="minus_H2O"])/sqrt(3)
)
summary_df$SD <- c(
  sd(df$dw_rate[df$species=="prat" & df$condition=="plus_H2O"]),
  sd(df$dw_rate[df$species=="toc" & df$condition=="plus_H2O"]),
  sd(df$dw_rate[df$species=="prat" & df$condition=="minus_H2O"]),
  sd(df$dw_rate[df$species=="toc" & df$condition=="minus_H2O"])
)
#this is a summary of the  mean of each species*treatment combo

dw_summary <- summary_df
dw_summary$temp <- paste0(dw_summary$species, dw_summary$condition)

ggplot(data=summary_df, aes(x=condition, y=dw_rate, colour=species))+geom_point()
graph3=ggplot(data=df, aes(x=condition, y=dw_rate, colour=species))+geom_point()+geom_boxplot()

aov=aov(dw_rate~species*condition, data=df)
summary(aov)
#shows that there is a significant interaction between species and condition
tukey.test <- HSD.test(aov, trt = c('condition', 'species'))
tukey.test



#now i want to make a bar graph as a different way of looking at the data.
graph4 = ggplot(data = summary_df, aes(x=condition, y=dw_rate*1000, group=species, fill=species))
graph4 = graph4 + geom_bar(stat="identity", width = 0.5, position = "dodge", colour = 'black', size =0.8)
graph4=graph4+ ylab(expression(bold(atop(textstyle("Rate of"~ O[2]~"Consumption"),atop("("*mu*"moles "~s^-1~g^-1~"dwt)")))))

graph4 <-  graph4 + theme_classic()
  
graph4=graph4+xlab("Condition")
graph4 = graph4 + scale_x_discrete(labels=c("well watered", "droughted"))
graph4 = graph4+scale_fill_discrete("Species",
                                    labels=c(expression(bold(paste(italic("C. pratensis")))),expression(bold(paste(italic("C. tocuchensis"))))))
graph4 <- graph4+geom_errorbar(aes(ymin=dw_rate*1000-SD*1000, ymax=dw_rate*1000+SD*1000),
                               width=0.15, 
                               size=1,
                               position=position_dodge(.5))
graph4 = graph4+scale_fill_manual(values = c( "#E69F00", "#56B4E9"),
                                  name ="Species",
                                  labels=c(expression(bold(paste(italic("C. pratensis")))),expression(bold(paste(italic("C. tocuchensis"))))))
graph4 <- graph4 + theme(axis.text=element_text(size=18),
                       axis.title = element_text( color="black", face="bold", size=18),
                       legend.text = element_text(size=16),
                       legend.title = element_text(size=18),
                       legend.text.align = 0
)

graph4

# Now add the letters that represent the TUkey Kramer

TK_letters_DW <- c('x','x','y','x')
graph4 <- graph4 +
  ylim(0,12) +
  geom_text(aes(label=TK_letters_DW), position=position_dodge(width=0.5), vjust=-3.4)


graph4

# this adds the error bars, which show the standard deviation of the mean. 




tov=aov(dw_rate~condition*species, data=df)
summary(tov)





#### OK so now i'm going to repeat the analysis above, but make a graph for oxygen consumption on a per area basis. 
#### because i'm lazy i copied the whole first bit, including the importing of the data. 
####

#first want to see if i can reformat straight from the output files of the Hansatech oxygraph. 
setwd("~/For_PhD/respiration paper/Data_from_the_machinie/Data_from_the_machinie")

a=read.csv("a_toc-.csv")
a$row=row(a)
class(a)
start=a$HANSATECH.INSTRUMENTS...O2view.Recording[(a$row[a$HANSATECH.INSTRUMENTS...O2view.Recording=="300.0"])+1]
end = a$HANSATECH.INSTRUMENTS...O2view.Recording[(a$row[a$HANSATECH.INSTRUMENTS...O2view.Recording=="600.0"])+1]
start=as.numeric(paste(start))
end = as.numeric(paste(end))
rate = (start-end)/300
#so what this does is:
#take the csv file from Hansatech, which is in a rubish format
#find the rownumber of the 300th second. designate start = the conents of that rownumber+1. this is because the next row down is the O2 content.
#do the same thing for finding the O2 content at 600 seconds
#calculate the rate of the change


#i have now made the above code into a function so i can do it on all the files. 
getrate= function (x) {
  x$row=row(x)
  start=x$HANSATECH.INSTRUMENTS...O2view.Recording[(x$row[x$HANSATECH.INSTRUMENTS...O2view.Recording=="300.0"])+1]
  end = x$HANSATECH.INSTRUMENTS...O2view.Recording[(x$row[x$HANSATECH.INSTRUMENTS...O2view.Recording=="600.0"])+1]
  start=as.numeric(paste(start))
  end = as.numeric(paste(end))
  x = (start-end)/300
  return(x)
  
}



list.files()
#make sure all the xcel files are not open when doing this.


#now this is a for loop to find the rates for each sample
qq=NULL
for (z in (list.files()[1:12])){
  qq=c(qq,getrate(read.csv(z)))
}
qq

p=read.csv("night time measurements.csv", skip=1, header=TRUE)
condition=c("minus_H2O","plus_H2O" ,"minus_H2O", "plus_H2O","plus_H2O", "minus_H2O","minus_H2O","plus_H2O","plus_H2O", "minus_H2O", "plus_H2O","minus_H2O")
species=c("toc", "prat", "prat", "toc", "prat","prat", "toc", "toc", "prat", "prat", "toc","toc")
df=data.frame(sample_code =letters[1:12] , rate=qq, species = species, condition=condition, time = p$time, freshweight=p$weight..g.)
#this generates a dataframe which contains all of the rates for each sample
#now i can start doing some stats.

#need to normalise the rate, based on the freshweight
df$fw_rate=df$rate/df$freshweight
#fw_rate is the rate/the freshweight
#the arithmetic mean is appropriate for these data
df$condition=factor(df$condition, levels = c("plus_H2O","minus_H2O"))

# now I'm going to calculate the rate per area (cm)
df$rate <- df$rate/10*10000


summary_df=aggregate(rate~species+condition, df, FUN=mean)
summary_df$SE <- c(
  sd(df$rate[df$species=="prat" & df$condition=="plus_H2O"])/sqrt(3),
  sd(df$rate[df$species=="toc" & df$condition=="plus_H2O"])/sqrt(3),
  sd(df$rate[df$species=="prat" & df$condition=="minus_H2O"])/sqrt(3),
  sd(df$rate[df$species=="toc" & df$condition=="minus_H2O"])/sqrt(3)
)
summary_df$SD <- c(
  sd(df$rate[df$species=="prat" & df$condition=="plus_H2O"]),
  sd(df$rate[df$species=="toc" & df$condition=="plus_H2O"]),
  sd(df$rate[df$species=="prat" & df$condition=="minus_H2O"]),
  sd(df$rate[df$species=="toc" & df$condition=="minus_H2O"])
)
#this is a summary of the arithmatic mean of each species*treatment combo


area_summary <- summary_df
area_summary$temp <- paste0(area_summary$species, area_summary$condition)

summary_all <- left_join(fw_summary, dw_summary, by = c("temp" = "temp"), suffix = c('_fw', '_dw'))
summary_all <- left_join(summary_all, area_summary, by = c("temp" = "temp"))
colnames(summary_all)[12:16] <- paste0(colnames(summary_all)[12:16], '_area')
summary_all <- summary_all[,c(-6,-7,-8,-12,-13)]
summary_all_melt <- melt(summary_all)

summary_all_melt <- cbind(summary_all_melt[c(1:4,13:16,25:28),],
      summary_all_melt[c(5:8,17:20,29:32),4],
      summary_all_melt[c(9:12,21:24,33:36),4]
)
colnames(summary_all_melt)[c(1,2,4,5,6)] <- c('species', 'condition','rate','rate_SE','rate_SD')
summary_all_melt

# So I made this summary_all table to see if I could do a facet_grid of the bar charts
# I am going to do this graph at the end of the script


ggplot(data=summary_df, aes(x=condition, y=rate, colour=species))+geom_point()
graph1=ggplot(data=df, aes(x=condition, y=rate, colour=species))+geom_point()+geom_boxplot()

aov=aov(rate~species*condition, data=df)
summary(aov)
TukeyHSD(tov)
tukey.test <- HSD.test(aov, trt = c('condition', 'species'))
tukey.test
# 




#shows that there is a significant interaction between species and condition


#now i want to make a bar graph as a different way of looking at the data.
per_area_graph = ggplot(data = summary_df, aes(x=condition, y=rate, group=species, fill=species))
per_area_graph = per_area_graph + geom_bar(stat="identity", width = 0.5, position = "dodge", colour = 'black', size =0.8)
per_area_graph=per_area_graph+ylab(expression(bold(atop(textstyle("Rate of"~ O[2]~"Consumption"),atop("("*mu*"moles "~m^-2~s^-1*')')))))

per_area_graph <-  per_area_graph + theme_classic()

per_area_graph=per_area_graph+xlab("Condition")
per_area_graph = per_area_graph + scale_x_discrete(labels=c("well watered", "droughted"))
per_area_graph = per_area_graph+scale_fill_manual(values = c( "grey50", "white"),
                                                  name ="Species",
                                                  labels=c(expression(bold(paste(italic("C. pratensis")))),expression(bold(paste(italic("C. tocuchensis"))))))
                                                  
per_area_graph <- per_area_graph+geom_errorbar(aes(ymin=rate-SD, ymax=rate+SD),
                                               width=0.15, 
                                               size=1,
                                               position=position_dodge(.5))
per_area_graph <- per_area_graph + theme(axis.text=element_text(size=18),
                                         axis.title = element_text( color="black", face="bold", size=18),
                                         legend.text = element_text(size=16),
                                         legend.title = element_text(size=18),
                                         legend.text.align = 0
)

per_area_graph <-  per_area_graph + geom_point(data = df, aes(x=condition, y=rate, fill = species), shape = 21, colour = 'black',size = 2,stroke = 1.5,  position = position_jitterdodge(jitter.width = 0.2, jitter.height=0, dodge.width = 1.3)) 
                                                                                                                                                 

per_area_graph



# Now adding the letters from the Tukey Kramer

TK_letters_area <- c('x','xy','z','y')
per_area_graph <- per_area_graph +
  ylim(0,1.3) +
  geom_text(aes(label=TK_letters_area), position=position_dodge(width=0.5), vjust=-4.1)

per_area_graph


tov=aov(rate~condition*species, data=df)
summary(tov)

graph2  
grid.arrange(  graph4, per_area_graph,   layout_matrix = rbind(c(1,2)))

# Now to make a figure for publication

grid.arrange(graph2 + theme(legend.position = "none"), 
             graph4 + theme(legend.position = "none"),
             per_area_graph + theme(legend.position = "none"),
             layout_matrix = rbind(c(1,2,3)))

      

