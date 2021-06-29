#####################################3
#This script imports experiment data
#Created by Pablo Maldonado on 08/24/2019
######################################
rm(list=ls())
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringi)
library(ggforce)
setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")

genotypes <- data.frame(list.files(path = ".", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
#bind.data binds position and mobility data   
bind.data <- function (i, data.position, data.mobility, genotype, day){
    
        j <- 2*i + 1
        position <- data.position [, c(2, (j):(j+1))] %>% 
                dplyr::rename (time=V2, x=paste("V",j, sep=''), y=paste("V",(j+1), sep='')) %>%
                dplyr::mutate (id=paste(genotype, day, i, sep = '.'), genotype=genotype)
        mobility <- data.frame(backward = data.mobility [, (i+2)])
        assay <- rep(paste(n),times = nrow(position))
        data.frame(cbind(position, mobility, assay))
}

#import.experiment.by.genotpe imports data and combines them
import.experiment.by.genotpe <- function(genotype, day) {
      
        genotype.day.mobility.DF <- read.csv(paste('./', genotype,'/', day, '/Mobility.csv', sep = ''), header = F)
        genotype.day.position.DF <- read.csv(paste('./', genotype,'/', day, '/Position.csv', sep = ''), header = F)
        #i is the number of worms
        genotype.day.DF <- data.frame()
        for (i in 1:(as.numeric(ncol(genotype.day.position.DF) - 2)/2)){
                
                #i <- 1
                genotype.day.DF <- rbind(genotype.day.DF, bind.data(i, 
                                                                    genotype.day.position.DF, 
                                                                    genotype.day.mobility.DF, 
                                                                    genotype, 
                                                                    day))
        }
        genotype.day.DF %>% filter (!is.na(x), !is.na(y))
}
#importing ICE
res <- NULL
n1 <- nrow(genotypes)
n2 <- 20







for (i in 1:n1){
  
       
        
        tryCatch({
                for (n in 1:n2) {
                  
                  print(i)
                        print(n)
                        P <- import.experiment.by.genotpe (paste(as.character(genotypes[i,1]), sep = " ", collapse = NULL), 
                                                           paste(as.character(n), sep = " ", collapse = NULL))
                        #Light <- read.csv(paste("MovingAverageSpeed-1.csv"), header = F)
                        
                        assign(paste(as.character(genotypes[i,1]), paste(as.character(n), sep = " ", 
                                                                         collapse = NULL), sep=""),P)
                        res <- rbind(res,get(paste((genotypes[i,1]),paste(as.character(n)), sep = "")))
                        assign(paste(as.character(genotypes[i,1]), sep=""),res)
                        
                }
                
        },error=function(e){})
        data.DF <- rbind(get(paste(as.character(genotypes[i,1]), sep="")))
}

save(data.DF, file = "./CompleteDataSet.rda")

TheAllPlot <- ggplot(data = data.DF, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = genotype), alpha=0.4)+
  facet_wrap_paginate(~ assay + genotype, page = 4)
TheAllPlot


ggsave("all_assays.pdf")






break
for (i in 1:100){
  
  WormID.DF <- data.frame(unique(data.DF$id))
  wid <-i
  print(paste(WormID.DF[wid,1]))
  tracks.plot <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), mapping = aes(x = lag(x), y = lag(y))) +
    #geom_point(aes(color = (steeringangle)),size =1 ) +
    #geom_path(aes(alpha= 0.002))+
    geom_path(data=subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
              aes(x=x, y=y), color='black') + 
    coord_fixed(ratio = 1, xlim = (NULL), ylim = NULL, expand = TRUE,
                clip = "on")+
    ggtitle(paste(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL)))+
    theme_minimal()
  tracks.plot + theme(panel.grid.major = element_blank(), 
                                                                                                                                                                           panel.grid.minor = element_blank(),
                                                                                                                                                                           panel.background = element_blank(), 
                                                                                                                                                                           axis.line = element_blank(), 
                                                                                                                                                                           axis.title = element_blank(), 
                                                                                                                                                                           axis.text = element_blank())
  
  ggsave(paste(as.character(WormID.DF[wid,1]), "pdf", sep = "."))
  
  
  
  
}


#rm(list=ls())
