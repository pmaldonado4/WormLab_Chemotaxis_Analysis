##########################################RUN Path Lenght Analysis###########
##########################################
##########################################
##########################################
##########################################

library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(rowr)
setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")
rm(list=ls())
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
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

load('./databearing.rda')
data.DF$obs <- 1:nrow(data.DF)
data.DF <- data.DF %>% group_by(id) %>% filter(max(obs) != obs)


x <- data.frame(((data.DF$bearing.angle.smooth)))
x<- abs(x)

data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(bearing.abs = abs(bearing.angle.smooth),
               thresholdunder = case_when(bearing.abs < 60 ~1, bearing.abs > 60 ~0))
        
#x2<-x
#data[data > -1.5 & data < 1.5] <- 0
#x[x >= 60 & x <= 180] <- 1
# angle1 <- 0
angle <- 60
# x[x > angle1 & x < angle] <- 1 #####0 equals under threshold
# x[x > 1] <- 0
#x2[x2 < angle] <- 0
#x2[x2 > angle] <- 1 #####0 equals over threshold
#colnames(x2) <-"thresholdover"
# 
# colnames(x) <-"thresholdunder"
# data.DF <- rowr::cbind.fill(data.DF,x)

id <- data.DF$id

test <- data.DF %>%
        dplyr::group_by(id, rleid(thresholdunder)) %>% #advance value every time Signal changes and group by that
        dplyr::mutate(cum.sum = cumsum(replace_na(displacement, 0))) %>% #cumsum in each group
        ungroup() %>% #ungroup so you could remove the grouping column
        select(-4)
#test2 <- data.DF %>%
        #group_by(id, rleid(thresholdover)) %>% #advance value every time Signal changes and group by that
        #mutate(cum.sumover = cumsum(smooth.displacement)) %>% #cumsum in each group
        #ungroup() %>% #ungroup so you could remove the grouping column
        #select(-4)
data.DF <- cbind(data.DF$id,test)

# daiffy <- data.frame(diff(data.DF$thresholdunder))
# colnames(daiffy) <-"indexunder"
# #data.DF <- dplyr::select(data.DF, -c(rleid(thresholdunder)))
# data.DF <- cbind.fill(data.DF, daiffy)
data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(indexunder = thresholdunder-lag(thresholdunder))
#data.DF <- cbind(data.DF,id)
#colnames(data.DF)[20] <- "id"
data.DF.under <-data.DF[data.DF$indexunder <= -1, ] 
data.DF.over <-data.DF[data.DF$indexunder == 1, ] 
data.DF.over %>%
        group_by(id) %>%
        filter(row_number() != n() | n()==1)
df <- data.DF.under %>% 
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize (totaldist.under = sum (cum.sum,na.rm = TRUE),
                          totalnum.runsunder = sum(abs(indexunder), na.rm = TRUE),
                          meandist.under = mean(cum.sum, na.rm = TRUE)
                          )
df.2 <- data.DF.over %>% 
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize (totaldist.over = sum (cum.sum,na.rm = TRUE),
                          totalnum.runsover = sum(abs(indexunder), na.rm = TRUE),
                          meandist.over = mean(cum.sum, na.rm = TRUE)
        )

datasummary.Df <- cbind(df,df.2[,3],df.2[,4],df.2[,5])
datasummary.Df <- datasummary.Df %>%
        group_by(id,genotype) %>%
        mutate(totalnumruns = totalnum.runsover+totalnum.runsunder,
               totaldistance = totaldist.under+totaldist.over)
paste("./under",angle, "rda", sep = ".")
save(datasummary.Df, file = paste("./runlength",angle, "rda", sep = "."))
####
####
####


