#####################################3
#This script load and explores worm data
#Created by Pablo Maldonado on 08/24/2019
######################################
rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(rowr)
library(ggpubr)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
        library(plyr)
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
                if (na.rm) sum(!is.na(x))
                else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- plyr::ddply(data, groupvars, .drop=.drop,
                             .fun = function(xx, col) {
                                     c(N    = length2(xx[[col]], na.rm=na.rm),
                                       mean = mean   (xx[[col]], na.rm=na.rm),
                                       sd   = sd     (xx[[col]], na.rm=na.rm)
                                     )
                             },
                             measurevar
        )
        
        # Rename the "mean" column    
        datac <- plyr::rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
}
setwd("~/Desktop/R Code to Review/03112021/")

load('./CompleteDataSet.rda')

# 
# data.DF <- data.DF %>%
#         dplyr::filter(id)
#plotting original data
data.plot <- ggplot(data = data.DF, mapping = aes(x = x, y = y),factor(genotype, level = c('N2', 'ICE'))) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=1) +
        theme(legend.position = "none")
data.plot

all.tracks.DF <- data.DF %>%
        filter (y > 0, y < 450000) 
#plotting data filtered by Y
all.tracks.plot <- ggplot(data = all.tracks.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=1) +
        theme(legend.position = "none")

all.tracks.plot

all.worms.DF <- all.tracks.DF %>%
        dplyr::group_by (id, genotype) %>%
        dplyr::summarize (y_min = min(y), 
                          y_max = max(y), 
                          y_distance = max (y) - min (y),
                          time_min = min (time), 
                          time_max = max (time), 
                          time_observed = max (time) - min (time)) %>%
        mutate (rate.change.y = y_distance/time_observed)

real.worms.DF <- all.worms.DF %>%
        filter (
                #were observed for at least 1 mins and 30 and started at the buttom
                time_observed > 120,
                y_max >= 25000,
                y_min <10000
        )

table (all.worms.DF$genotype)
table (real.worms.DF$genotype)

#calculating average speed
genotype.DF <- real.worms.DF %>% 
        dplyr::group_by (genotype) %>%
        dplyr::summarize (n = n(),
                          mean_rate.change.y = mean (rate.change.y), 
                          min_rate.change.y = min (rate.change.y),
                          max_rate.change.y = max (rate.change.y),
                          mean_distance = mean (y_distance), 
                          min_distance = min (y_distance),
                          max_distance = max (y_distance),
                          mean_time = mean (time_observed), 
                          min_time = min (time_observed),
                          max_time = max (time_observed)
        )
genotype.DF 

#full track for real worms
real.tracks.DF <- all.tracks.DF %>%
        filter (id %in% real.worms.DF$id)

real.tracks.plot <- ggplot(data = real.tracks.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=2) +
        theme(legend.position = "none") +
        labs(title = 'All worms')
real.tracks.plot
#ggsave(real.tracks.plot, device = "pdf")


hist(real.worms.DF$time_observed) 
tgc <- summarySE(real.worms.DF, measurevar="time_observed", groupvars=c("genotype"))
h <- ggplot(real.worms.DF, aes(x=time_observed, color=genotype)) +
        geom_density(fill="white", alpha=0.02,) +
        geom_density()+
        facet_wrap(~genotype)+
        geom_vline(data=tgc, aes(xintercept=time_observed, color=genotype),
                   linetype="dashed") 
         


h
ggsave("timetotargetdistribution.pdf")

#some worms are outliers. Let's see their path.

outliers.worms.DF <- real.worms.DF %>%
        filter (
                #were observed for at least 2 mins and started at the buttom
                time_observed > 900, 
                y_min < 10000
        )


table (outliers.worms.DF$genotype)

#full track for real worms
Bad.worms.DF <- all.tracks.DF %>%
        filter (id %in% outliers.worms.DF$id)

bad.worms.plot <- ggplot(data = Bad.worms.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=2) +
        theme(legend.position = "none") + 
        labs(title = 'Bad worms')
bad.worms.plot

#speed by genotype
names(real.worms.DF)
ggplot(real.worms.DF, aes(x=rate.change.y)) +
        geom_histogram() +
        facet_wrap( ~ genotype, ncol=2)

#plot track of fast worms
fast.worms.DF <- real.worms.DF %>%
        filter (
                #were observed for at least 2 mins and started at the buttom
                rate.change.y > 130
        )

fast.tracks.DF <- all.tracks.DF %>%
        filter (id %in% fast.worms.DF$id)

fast.worms.plot <- ggplot(data = fast.tracks.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=2) +
        theme(legend.position = "none") +
        labs(title = 'Fastest worms')
fast.worms.plot

ggsave("fastestworms.pdf")
injured.worms.DF <- all.worms.DF %>%
        filter (
                #were observed for at least 2 mins and started at the buttom
                time_observed > 150,
                y_min < 5000,
                y_max < 30000
        )


injured.tracks.DF <- all.tracks.DF %>%
        filter (id %in% injured.worms.DF$id)

injured.tracks.plot <- ggplot(data = injured.tracks.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=2) +
        ylim(5000,35000)+ 
        theme(legend.position = "none")+
        labs(title = 'Injured worms')
injured.tracks.plot
ggsave("brokentracks.pdf")
#fake worms
fake.tracks.DF <- all.tracks.DF %>%
        filter (!(id %in% real.worms.DF$id))

fake.tracks.plot <- ggplot(data = fake.tracks.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype) +
        ylim(5000,32000)+ 
        theme(legend.position = "none") +
        labs(title = 'Messy tracks')
fake.tracks.plot
ggsave("faketracksplot.pdf")

####Why is it excluding tracks that seem complete?
tracks.plot <- ggplot(subset(fake.tracks.DF,genotype == "N2", mapping = aes(x = x, y = y))) +
        geom_path(aes(x, y, group=genotype, colour=genotype))+
        coord_fixed(ratio = 1, xlim = (NULL), ylim = NULL,
                    expand = TRUE, clip = "on")+
        facet_wrap(id~genotype, strip.position = "bottom") +
        theme_minimal()

tracks.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_blank(), 
                    axis.title = element_blank(), axis.text = element_blank())
ggsave("alltracksn2fake.pdf")
table (real.tracks.DF$genotype)
table (fake.tracks.DF$genotype)
table (injured.worms.DF$genotype)
table (outliers.worms.DF$genotype)
table (fast.worms.DF$genotype)
table (real.worms.DF$genotype)
#######Looks for ymax of each worm and find the time, cut all after time=yymax do this at the beginning
#######Make a table with each genotyp and proportion of fake, real and total worms and lost worms
rel.worm.DF <- all.tracks.DF %>%
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize (y_min = min(y), 
                          y_max = max(y), 
                          y_distance = max (y) - min (y),
                          time_min = min (time), 
                          time_max = max (time), 
                          time_observed = max (time) - min (time)) %>%
        mutate (rate.change.y = y_distance/time_observed)
real.worms.data.DF <- all.tracks.DF %>%
        filter ((id %in% real.worms.DF$id))
save(rel.worm.DF, file = "./completedatawithdetailsofdetection.rda")

AllWorms.DF <- all.tracks.DF %>%
        filter ((id %in% real.worms.DF$id))
save(AllWorms.DF, file = ".//full.position.rda")

break
real.worms.data.DF <- real.worms.data.DF %>% 
        mutate(backward = recode(backward, "R" = 1, "F"=0, "I"=0))


save(real.worms.data.DF, file = ".//Realwormsforanalysis.rda")

tgc <- summarySE(real.worms.DF, measurevar="time_observed", groupvars=c("genotype"))



summary <- real.worms.DF %>% # the names of the new data frame and the data frame to be summarised
        group_by(genotype) %>%   # the grouping variable
        dplyr::summarise(time_observed1 = mean(time_observed),  # calculates the mean of each group
                  sd_PL = sd(time_observed), # calculates the standard deviation of each group
                  n_PL = n(),  # calculates the sample size per group
                  SE_PL = sd(time_observed)/sqrt(n())) # calculates the standard error of each group
names(summary)[2]<-"time_observed"

sem <- function(){
        sd(x,na.rm = TRUE)/sqrt(length(x, na.rm = TRUE), na.rm = TRUE)
}
sem(real.worms.DF$time_observed)
secplot <- ggplot(real.worms.DF, aes(y = time_observed, x = genotype,fill = genotype, group = genotype), colour="black") + 
        stat_summary(fun.y = mean, #Calculates mean to be plotted
                     fun.ymin = function(x) mean(x) - sd(x)/sqrt(105), ####Calculates range of error bar +/- sem
                     fun.ymax = function(x) mean(x) + sd(x)/sqrt(104),
                     geom = "errorbar", size =1, width =0.1) + ####adss error bar ,width controls width of end of error bar
        stat_summary(fun.y = mean,
                     geom = "bar") + ###Sets what type of plot to show
        ylab("Time to target (s)")+
        theme_classic2()+
        theme(legend.position = "none",axis.title = element_text(size=20),axis.text = element_text(size=15))+
        theme(axis.text.x= element_text(size = 20, colour = "black"),
              axis.text.y= element_text(size = 20, colour = "black"),
              axis.line = element_line(size = 1),
              axis.ticks.length = unit(9, "pt"),
              axis.ticks = element_line(size = 1),
              plot.margin=unit(c(1,3.5,1,1),"cm"))+
        scale_y_continuous(expand = c(0, 0)) #####Puts data at origin
        #stat_compare_means(data = real.worms.DF,aes(group = genotype), label = "p.signif",label.y = 450) +####Calculates significance and adds it to plot
        #scale_x_discrete(limits = positions)

secplot + stat_compare_means(method = "t.test",aes(label = ..p.signif..), 
                             label.x = 1.8955, label.y = 290, size = 8) +
        stat_compare_means(method = "t.test", label.y = 320, label.x = 1.95)

ggsave("timetotargetriaice.pdf")

