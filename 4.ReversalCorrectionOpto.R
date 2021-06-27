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
setwd("~/Desktop/newfuckingdata/")
load('./CompleteDataSet.rda')
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

data.new <- cbind.fill(data.DF, data.DF2)

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
        ggarrange(before, after, 
                  labels = c("before", "after correction"),
                  ncol = 1, nrow = 2)
        
        ggsave(paste(as.character(WormID.DF[wid,1]), "pdf", sep = "."))
} ###Plot before and after for quality check


x <- diff(data.DF$backward.cor)

data.DF <- cbind.fill(data.DF,x)
revcount <- data.DF %>% 
    group_by(id,genotype) %>%
    dplyr::summarise(revcount = (sum((object==1))))
save(data.DF, file = ".//FullDataCor.rda")  

