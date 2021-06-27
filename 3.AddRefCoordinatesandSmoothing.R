rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(rowr)
setwd("~/Desktop/R Code to Review/03112021")
load('./Realwormsforanalysis.rda')
cbind.fill<-function(...){
        nm <- list(...) 
        nm<-lapply(nm, as.matrix)
        n <- max(sapply(nm, nrow)) 
        do.call(cbind, lapply(nm, function (x) 
                rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

data.DF <- real.worms.data.DF

coordinates <- read.csv("CompleteIndex.csv", header = T)
xref <- matrix(data = NA, nrow = nrow(data.DF), ncol = 1, byrow = FALSE,
               dimnames = NULL)
yref <- matrix(data = NA, nrow = nrow(data.DF), ncol = 1, byrow = FALSE,
               dimnames = NULL)
tt <- data.frame(select(data.DF,id))
n<-1
for (i in 1:nrow(data.DF)) {
        
        if ((tt[i,] %like% (paste(coordinates[n,1])))==TRUE){
                
                xref[i,] <- coordinates[n,2] 
                yref[i,] <- coordinates[n,3] 
        }else{
                n<- n+1
                
                
                
        }
        
        
}
yref <- data.frame(yref)
xref <- data.frame(xref)
######Start here##############################################################

ex5 <- cbind(data.DF,xref,yref)  

save(ex5, file = ".//fullwithcoordinatesforangle.rda")

data.DF <- ex5 %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(smoothx = pracma::movavg(x, 45, "s")) %>%
        dplyr::mutate(smoothy = pracma::movavg(y, 45, "s"))
save(data.DF, file = ".//datasmoothandfull.rda")

break
WormID.DF <- data.frame(unique(data.DF$id))
for (i in 1:nrow(WormID.DF)){
        
        WormID.DF <- data.frame(unique(data.DF$id))
        wid <-i
        print(paste(WormID.DF[wid,1]))
        tracks.plot <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
                              mapping = aes(x = lag(x), y = lag(y))) +
                #geom_point(aes(color = (steeringangle)),size =1 ) +
                #geom_path(aes(alpha= 0.002))+
                geom_path(data=subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
                          aes(x=x, y=y), color='black') + 
                coord_fixed(ratio = 1, xlim = (NULL), ylim = NULL, expand = TRUE,
                            clip = "on")+
                ggtitle(paste(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL)))+
                theme_minimal() +
                geom_point(data=subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
                           aes(x=xref, y=yref), color='red') 
                tracks.plot + theme(panel.background = element_blank())
        
        ggsave(paste(as.character(WormID.DF[wid,1]),"track", "pdf", sep = "."))
        
        
        
        
}

