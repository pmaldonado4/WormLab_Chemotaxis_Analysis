
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)

rm(list=ls())
setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")
load('./databearing.rda')
data.DF <- data.DF %>%
        select(c(time,x,y,id,genotype,Assay.ID,xref,yref,date,smoothx,
                 smoothy,backward.cor,RevCount,speed,bearing.angle,
                 bearing.angle.smooth,distance.target))

low <- 0
high <- 50
#data.DF <- data.complete.DF
#################################Determining angle thresholds for subsequent analysis
##############################################################
#################################
#################################
x <- abs(data.frame(data.DF$bearing.angle.smooth))

data.DF <- data.DF %>% 
        group_by(id,genotype) %>%
        mutate(Angle.Threshold = case_when(
                bearing.angle.smooth > high  ~ 0,
                bearing.angle.smooth < low  ~ 0,
                bearing.angle.smooth > 1  ~ 1))



data.DF <- data.DF %>%
        group_by(id, genotype) %>%
        mutate(RevCount = if_else(RevCount <= 0,0,1 )) 


# angle <- 45
# x[x < angle] <- 0 #####0 equals under threshold
# x[x > angle] <- 1
# x[x > 0 & x < 45] <- 1
# x[x > 45] <- 1
#nrow(data.frame((unique(data.DF$id))))
# data.DF <- cbind(data.DF,x)

numbering = function(v,k) {
        ## First, replacing stretches of less than k consecutive 0s by 1s
        r = rle(v);
        r$values[r$values==1 & r$lengths<=k] = 0; 
        v2 = inverse.rle(r); 
        
        
}

data.DF <- data.DF %>% 
        group_by(id,genotype) %>%
        mutate(Clean.Reversal = numbering(backward.cor, 24),
               RevCount = lag(Clean.Reversal)-Clean.Reversal) ####What duration in frames of reversals do you cut out?

data.DF <- data.DF %>%
        # Define the start of Reversal.ID (putting 1 at the start of Reversal.ID)
        mutate(Reversal.ID = case_when((RevCount)< 1  ~ 0, TRUE ~ 1)) %>%
        # Extend the events using cumsum()
        mutate(Reversal.ID = case_when(RevCount< 1 ~ cumsum(Reversal.ID)))



#################Total Reversal Frequency######################################
################# 
################# 
################# 
Total.Reversal.DF <- data.DF %>%
        group_by(id, genotype) %>%
        mutate(RevCount = if_else(RevCount <= 0,0,1 )) 

Total.Reversal.Frequency.DF <- Total.Reversal.DF %>%
        group_by(id, genotype,date) %>%
        dplyr::summarise(Time.Observed = max ((time) - min (time))/60,
                  Rev.Freq = sum(RevCount, na.rm = T)/Time.Observed)



inds = which(Total.Reversal.DF$RevCount == 1)
# We use lapply() to get all rows for all indices, result is a list
rows <- lapply(inds, function(x) (x-2):(x))
# With unlist() you get all relevant rows
Data.Before.Reversals.DF <- Total.Reversal.DF[unlist(rows),]

Before.Reversal.Summary.DF <- Data.Before.Reversals.DF %>%
        group_by(id, Reversal.ID, genotype) %>%
        dplyr::summarize(meanAngle = mean(bearing.angle.smooth,na.rm = T),
                  meanSpeed = mean(speed, na.rm = T))

ggplot(data= Before.Reversal.Summary.DF, aes(x= meanAngle, color = genotype)) +
        geom_density()

Before.Reversal.Summary.Genotype.DF <- Before.Reversal.Summary.DF %>%
        group_by(genotype) %>%
        dplyr::summarise(Angle =mean(meanAngle))

Between.25.and.50.DF <- Before.Reversal.Summary.DF %>%
        filter(meanAngle>low & meanAngle <high)

Between.25.and.50.DF <- Between.25.and.50.DF %>%
        group_by(id,Reversal.ID,genotype) %>%
        dplyr::mutate(numrev = n())

Between.25.and.50.DF <- Between.25.and.50.DF %>%
        group_by(id,genotype) %>%
        dplyr::summarise(numRev = sum(numrev))

Timebetween.DF <- data.DF %>%
        group_by(id,genotype) %>%
        dplyr::summarise(timebetween = (sum(Angle.Threshold,na.rm = T)/8)/60)

Between.25.and.50.DF <- inner_join(Between.25.and.50.DF, Timebetween.DF, by = c("id","genotype"))

Between.25.and.50.DF <- Between.25.and.50.DF %>%
        group_by(id,genotype) %>%
        mutate(AvgFreq = numRev/timebetween)

Between.25.and.50.Genotype.DF <- Between.25.and.50.DF %>%
        group_by(genotype) %>%
        dplyr::summarise(meanFreq = mean(AvgFreq))

break
# 
# Data.Before.Reversals.DF <- Data.Before.Reversals.DF %>%
#         # Define the start of Reversal.ID (putting 1 at the start of Reversal.ID)
#         mutate(Reversal.ID = case_when((RevCount)>=1  ~ 1, TRUE ~ 0)) %>%
#         # Extend the events using cumsum()
#         mutate(Reversal.ID = case_when(RevCount<1 ~ cumsum(Reversal.ID)))
#         
rev.Freq.DF <- Total.Reversal.Frequency.DF %>%
        group_by(genotype,date) %>%
        dplyr::summarize(Rev.Freq.M = mean(Rev.Freq, na.rm = T),
                         semRev.Freq = sd(Rev.Freq,na.rm = T)/sqrt(n()))

totalrev.plot <- ggplot(data=tgc, aes(x=genotype, y=reversal.frequency)) +
        geom_bar(stat="summary",fun.y = "mean", (aes(fill = genotype, color = "red"))) +
        scale_y_continuous(expand = c(0,0))+
        theme_classic()+
        geom_errorbar(aes(ymin=reversal.frequency-se, ymax=reversal.frequency+se),
                      width=.2,                    # Width of the error bars
                      position=position_dodge(.9))+
        theme(legend.position = "none",axis.title = element_text(size=20),axis.text = element_text(size=15))+
        ylab("Reversals per minute")

totalrev.plot






























#test <- (numbering(data.DF$backward.cor,1))
# data.DF <- data.DF %>%
#         group_by(id) %>%
#         mutate(diff.test. = backward.cor-lag(backward.cor))
#test <- data.DF$backward.cor
#test <- data.frame(diff(test))
#data.DF <- rowr::cbind.fill(data.DF,test)
backward.index <- (which(data.DF$diff.test.==1 & data.DF$data.DF.bearing.angle.smooth==1, arr.ind=TRUE))
backward.index1 <- (which(data.DF$diff.test.==1,arr.ind=TRUE))
backward.index.2sec <- (backward.index)

data.bearing.reversal.DF <- data.DF[backward.index,]
data.reversal.DF <- data.DF[backward.index1+1,]
data.reversal.DF$diff.test.[data.reversal.DF$diff.test. == -1] <- 0

#################Total Reversal Frequency######################################
################# 
################# 
################# 

time.DF <- data.DF %>%
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize(time_observed = max ((time) - min (time))/60,
                         time.seconds = max(time)-min(time))
        
data.rev <- data.reversal.DF %>% 
        dplyr::group_by(id) %>% 
        dplyr::summarize(numrev = sum(backward))

time.DF <- time.DF %>%
        left_join(data.rev, by = "id")
#time.DF <- cbind(time.DF,data.rev[,2])
rev.Freq.DF <- time.DF %>% 
        dplyr::group_by(id,genotype) %>%
        dplyr::summarize(reversal.frequency = numrev/time_observed )
reversalandtime.DF <- time.DF %>%
        left_join(rev.Freq.DF)

#reversalandtime.DF <- cbind.fill(time.DF, rev.Freq.DF[,3])
save(reversalandtime.DF, file = "./totalreversalfreq.rda")
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
tgc <- summarySE(rev.Freq.DF, measurevar="reversal.frequency", groupvars=c("genotype"))

totalrev.plot <- ggplot(data=tgc, aes(x=genotype, y=reversal.frequency)) +
        geom_bar(stat="summary",fun.y = "mean", (aes(fill = genotype, color = "red"))) +
        scale_y_continuous(expand = c(0,0))+
        theme_classic()+
        geom_errorbar(aes(ymin=reversal.frequency-se, ymax=reversal.frequency+se),
                      width=.2,                    # Width of the error bars
                      position=position_dodge(.9))+
        theme(legend.position = "none",axis.title = element_text(size=20),axis.text = element_text(size=15))+
        ylab("Reversals per minute")

totalrev.plot

######Reversal frequency at different zones of bearing angle####################
######
######
######
######
######
######
data.DF$bearing.angle <- abs(data.DF$bearing.angle.smooth)
x <- data.frame(data.DF$bearing.angle.smooth)
i <- 45
#angle1 <- 45
angle <- i
x[x > 0 & x < i] <- 1 #####0 equals under threshold
x[x > 1] <- 0


colnames(x) <- "timeunderangle"
data.DF <- cbind(data.DF,x)
angle.index1 <- (which(x==1,arr.ind=TRUE))
angle.index2 <- (which(x==0,arr.ind=TRUE))
angleatzone <- data.DF[angle.index1[,1],]
angleatzone2 <- data.DF[angle.index2[,1],]
#i <- 60

zone <- angleatzone %>%
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize(countrev =sum(diff.test. == 1, na.rm = TRUE),
                         timeunderangleframe = sum(timeunderangle == 1, na.rm = TRUE),
                          freq.at.zone = countrev/timeunderangleframe*(8*60))
zone <- zone[is.finite(zone$freq.at.zone) & is.finite(zone$freq.at.zone), ]
under60time <- zone[zone$freq.at.zone<i,] 


zone2 <- angleatzone2 %>%
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize(countrevover =sum(diff.test. == 1, na.rm = TRUE),
                         timeoverangleframe = sum(timeunderangle == 0, na.rm = TRUE),
                         freq.at.zoneover = countrevover/timeoverangleframe*(8*60))

zone2 <- zone2[is.finite(zone2$freq.at.zoneover) & is.finite(zone2$freq.at.zoneover), ]
over60time <- zone2[zone2$freq.at.zoneover<i,] 
dataundersumma.DF <- zone %>%
        dplyr::group_by(genotype) %>%
        dplyr::summarise(meanrev = mean(freq.at.zone, na.rm = TRUE))



reversalsandangle <- cbind(under60time,over60time[,3:5])

paste("./reversalangle",i, "rda", sep = ".")
save(reversalsandangle, file = paste("./reversalangle",i, "rda", sep = "."))

save(reversalsandangle, file = ".//reversalsandangle.rda")


rev <- reversalsandangle %>%
        group_by(genotype) %>%
        dplyr::summarise(meanrevunder = mean(freq.at.zone),
                         meanrevover = mean(freq.at.zoneover))
print(rev)
