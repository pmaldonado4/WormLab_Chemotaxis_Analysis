################################################################################################################
################################################################################################################
##############This script fixes the head/tail detection error that MBF Bioscience WormLab makes
##############Created on 08/30/2020 by Pablo Maldonado
##############
##############


library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(gridExtra)
library(data.table)
library(rowr)
rm(list=ls())
setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")
load('./datasmoothandfull.rda')
data.DF <- real.worms.data.DF
data.DF <- data.DF %>% 
    mutate(backward = recode(backward, "R" = 1, "F"=0, "I"=0))

#data.DF <- data.complete.DF
data.DF3 <- data.DF


WormID.DF <- data.frame(unique(data.DF$id))  ####Makes d.f with list of worm names
gentype <- data.frame(unique(data.DF$genotype))  ####DF with list of genotypes
wid <-2 ####Worm to be ploted in wormid.DF list

ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
       mapping = aes(x = x, y = y)) +
        geom_path() + 
        theme_minimal()

before <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                mapping = aes(x = time, y = backward)) +
        geom_path() + 
        theme_minimal()


before


flipit <- function(x){
        y<- data.frame(x)
        y <- (y[1:24,])
        #print(y)
        if (sum(y,na.rm = TRUE) >=24){
                #print(y)
                x<- ifelse(x==1, 0,1)
        } else {
                
                x<-(x)
        }
        
}


fliptit2 <- function(x){
        x<- ifelse(x==1, 0,1)
}


data.DF <- data.DF %>%  ####This step corrects the incorrect detection of head/tail that happens from the start of tracking
        group_by(id) %>%
        mutate(backward = flipit(backward))


corr <- list()
data.DF2 <- data.frame(data.DF$backward)
data.DF2[is.na(data.DF2)] <- 0
for (i in 1:nrow(data.DF2)) { #####this step corrects errors that happen during the tracking
        
    
        print(i)
        if (data.DF2[i,]==1){
            
                y <- sum(data.DF2[i:(i+100),], na.rm = T)
                
                if (y >= 40 & y >=70) { ###sets threshold for reversal correction, longer than 5 seconds and longer than 8.5 seconds WARNING DO NOT CHANGE UNLESS SURE, MUST BE APPLIED TO ALL GROUPS 
                        
                       corr[[i]] <- fliptit2(data.DF2[i:nrow(data.DF2),])
                       data.DF3 <- data.frame(corr[[i]])
                       nrow(data.DF3)
                       print(i)
                       data.DF4 <- data.frame(data.DF2[1:i-1,])
                       colnames(data.DF4)=colnames(data.DF3)
                       data.DF2 <- rbind(data.DF4,data.DF3)
                       print("yes")
                       
                       
                }
        }
        
}

data.new <- cbind(data.DF, data.DF2)

data.DF <- data.new %>%
        dplyr::rename(backward.cor = corr..i..)

for (i in 1:nrow(WormID.DF)) {
        
        wid <-i
        before <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                         mapping = aes(x = time, y = backward)) +
                geom_path() + 
                theme_minimal()
        

        
        after <- ggplot(subset(data.new,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                        mapping = aes(x = time, y = corr..i..)) +
                geom_path() + 
                theme_minimal()
        ggpubr::ggarrange(before, after, 
                  labels = c("before", "after correction"),
                  ncol = 1, nrow = 2)
        
        ggsave(paste(as.character(WormID.DF[wid,1]),"correctedmovement", "pdf", sep = "."))
} ###Plot before and after for quality check

data.DF <- data.DF %>%
    group_by(id, genotype) %>%
    mutate(RevCount = backward.cor-lag(backward.cor))

displacement <- function(x1,y1){
    x <- x1- lag(x1)
    y <- y1 - lag(y1)
    (sqrt((x^2)+(y^2)))
}

data.DF <-data.DF %>% group_by(id,genotype) %>%
    mutate(speed = displacement(x,y)/0.125)
revcount <- data.DF %>% 
    group_by(id,genotype) %>%
    dplyr::summarise(revcount = (sum((RevCount==1), na.rm =T)),
                     time_observed = (max(time))/60,
                     speed = mean(speed, na.rm = T),
                     rev_freq = revcount/time_observed)

revcount <- revcount %>%
    filter(speed > 200 & speed <350)


write.csv(revcount, "reversalandspeed.csv")
save(data.DF, file = ".//FullDataCor.rda")  

for (i in 1:nrow(WormID.DF)){
    
    WormID.DF <- data.frame(unique(data.DF$id))
    wid <-i
    print(paste(WormID.DF[wid,1]))
    tracks.plot <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), mapping = aes(x = lag(x), y = lag(y))) +
        #geom_point(aes(color = (steeringangle)),size =1 ) +
        #geom_path(aes(alpha= 0.002))+
        geom_path(data=subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
                  aes(x=x, y=y), color='black') + 
        geom_point(data=subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
                   aes(x=xref, y=yref), color='red')+
        ggtitle(paste(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL)))+
        coord_fixed(ratio=1)+
        theme_minimal() +
        geom_segment(aes(x = 2000, y = 10000, xend = 2000, yend = 20000)) +
        annotate("text", x=6000, y=15000, label= "1cm") +
        geom_segment(aes(x = 2000, y = 30000, xend = 2000, yend = 31000)) +
        annotate("text", x=6000, y=31000, label= "1mm")
    tracks.plot + theme(panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(), 
                        axis.line = element_blank(), 
                        axis.title = element_blank(), 
                        axis.text = element_blank())
    
    ggsave(paste(as.character(WormID.DF[wid,1]),"track", "pdf", sep = "."))
    
    
    
    
}

