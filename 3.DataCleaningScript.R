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
setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")


load('./CompleteDataSet.rda')

# 
data.DF <- data.DF %>%
        mutate(Assay.ID = paste(genotype, assay, sep = "."))
#plotting original data

Assay.Parameters.DF <- data.DF %>%
        dplyr::group_by(Assay.ID) %>%
        dplyr::summarize(maxY = max(y, na.rm = T)*0.9)
data.DF <- data.DF %>%
        left_join(Assay.Parameters.DF, by = "Assay.ID")
data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(Max.Diff = y-maxY) %>%
        filter(Max.Diff < 0)

all.tracks.DF <- data.DF
# 
# all.tracks.DF <- data.DF %>%
#         filter (y > 0, y < 450000) 
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


real.worms.data.DF <- real.worms.data.DF %>% 
        mutate(backward = recode(backward, "R" = 1, "F"=0, "I"=0))


save(real.worms.data.DF, file = ".//Realwormsforanalysis.rda")
break
