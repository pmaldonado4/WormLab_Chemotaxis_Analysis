#########This code analyzes the trajectory of the worm and measures the turning angle############
#########
#########Things to know: How much distance between each point to analyze angle###################
#########
#########
#########
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(rowr)
rm(list=ls())
setwd("~/Desktop/11222019/AIZ/")
load('./CompleteDataSet.rda')
#Chrimson Experiment

WormID.DF <- data.frame(unique(data.DF$id))
gentype <- data.frame(unique(data.DF$genotype))
wid <-20
data1 <- data.DF

for (k in 1) {
        setwd("~/Desktop/11222019/AIZ/")
        #print(i)
        #print(paste(as.character(gentype[k,1])))
        setwd(paste(as.character(gentype[k,1])))
        
        assays <- data.frame(list.files(path = ".", pattern = NULL, all.files = FALSE,
                                        full.names = FALSE, recursive = FALSE,
                                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
        
        #setwd(paste(as.character(gentype[3,1]),sep = "/",1))
        setwd("~/Desktop/11222019/AIZ/")
        for (n in 1) {
                print(n)
                setwd("~/Desktop/11222019/AIZ/")
                setwd(paste(as.character(gentype[k,1]),sep = "/", paste(as.character(assays[n,1]))))
                print(getwd())
                #print(n)
                Light <- read.csv(paste("Speed.csv"), header = F)
                #print(n)
                #toDelete <- seq(0, length(Light), 2)
                
                #Light <-  Light[-toDelete, ]
                Light <- Light[,3:4]
                colnames(Light) <- c("On","Off")
                #wid <- n
                #print(wid)
                #LightOnIndex.DF <- matrix(NA, nrow = 1000, ncol = 1)
                LightOnIndex.DF  <- list()
                for (i in 1:nrow(Light)){
                        LightOnIndex.DF[[i]] <- (seq(Light[i,1], Light[i,2],1))
                }
                
                LightOnIndex.DF <- do.call(rbind, lapply(LightOnIndex.DF, as.data.frame))
                LightOnIndex.DF <- as.matrix(LightOnIndex.DF)
                
                
                
                wormtoplot <- filter(data.DF, id == paste(paste(as.character(gentype[k,1]),sep = ".", 
                                                                paste(as.character(assays[n,1])),1)))
                Lightonpath <- wormtoplot[LightOnIndex.DF,]
                #print(n)
                #print(paste(as.character(WormID.DF[n,1]), sep = " ", collapse = NULL))
                
                tracks.plot <- ggplot(subset(data1,id %in% c(paste(as.character(gentype[k,1]),sep = ".", 
                                                                   paste(as.character(assays[n,1])),1))),
                                      mapping = aes(x = x, y = y)) +
                        #geom_point(aes(color = (w.angle2)),size =2 ) +
                        geom_path(data=subset(data.DF,id %in% c(paste(as.character(gentype[k,1]),sep = ".", 
                                                                      paste(as.character(assays[n,1])),1))), 
                                  aes(x=x, y=y), size=1 ,color='black')+
                        geom_point(data=Lightonpath, aes(x=x, y=y),shape = 15, fill = 'red', size = 2, color='red')+
                        coord_fixed(ratio = 1, xlim = (NULL), ylim = NULL,
                                    expand = TRUE, clip = "on")+
                        ggtitle(paste(as.character(gentype[k,1]),sep = ".", paste(as.character(assays[n,1])),1))+
                        
                        theme_minimal()
                
                tracks.plot + scale_color_gradient(low="yellow", high="red") +  theme(panel.grid.major = element_blank(), 
                                                                                      panel.grid.minor = element_blank(),
                                                                                      panel.background = element_blank(), 
                                                                                      axis.line = element_blank(), 
                                                                                      axis.title = element_blank(), 
                                                                                      axis.text = element_blank())
                
                ggsave(paste(as.character(WormID.DF[wid,1]), "pdf", sep = "."))
                tracks.plot
        }
        
        
        
}
