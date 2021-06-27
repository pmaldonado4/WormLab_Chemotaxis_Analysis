#####################################3
#This script imports experiment data
#Created by Pablo Maldonado on 08/24/2019
#######################################
rm(list=ls())
library(dplyr)
library(ggplot2)
library(gridExtra)
setwd("~/Desktop/glr-3 Project Files/GLCs/07222020/")
#Light <- read.csv("Speed.csv", header=F)
#####Things to fix
#####the first folder number must have a complete data set in length of time
genotypes <- data.frame(list.files(path = ".", pattern = NULL, all.files =  FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
#genotypes <- data.frame(genotypes[-1,1])

#bind.data binds position and mobility data
bind.data <- function (i, data.position, data.mobility, genotype, day, Light,statusch,head,bending){
    
        j <- 2*i + 1
        position <- data.position [, c(2, (j):(j+1))] %>% 
                dplyr::rename (time=V2, x=paste("V",j, sep=''), y=paste("V",(j+1), sep='')) %>%
                dplyr::mutate (id=paste(genotype, day, i, sep = '.'), genotype=genotype)
        mobility <- data.frame(backward = data.mobility [, (i+2)])
        bending <- data.frame(bending.angle = bending [, (i+2:4)]) 
        j <- 2*i +1
        head <- head [, c((j):(j+1))] %>% 
          dplyr::rename (xhead=paste("V",j, sep=''), yhead=paste("V",(j+1), sep=''))
        data.frame(cbind(position, mobility,Light,head,bending,statusch))
}

#import.experiment.by.genotpe imports data and combines them
import.experiment.by.genotpe <- function(genotype, day) {
      
        genotype.day.mobility.DF <- read.csv(paste('./', genotype,'/', day, '/Mobility.csv', sep = ''), header = F)
        genotype.day.position.DF <- read.csv(paste('./', genotype,'/', day, '/Position.csv', sep = ''), header = F)
        genotype.day.light.DF <- read.csv(paste('./', genotype,'/', day, '/Speed.csv', sep = ''), header = F)
        genotype.day.head.DF <- read.csv(paste('./', genotype,'/', day, '/PositionHead.csv', sep = ''), header = F)
        genotype.day.bending.DF <- read.csv(paste('./', genotype,'/', day, '/BendingAngles-Multiple.csv', sep = ''), header = F)
        #i is the number of worms
        genotype.day.DF <- data.frame()
        
        Light <- genotype.day.light.DF
        
        Light <- Light[,3:4]+1
        
        
        Light <- Light[-nrow(Light),]
        LightOff <- data.frame(Light[,1]-1)
        
        cero <- 0
        cero<- data.frame(cero)
        colnames(cero) <-"Light...2."
        
        
        Lightoff2<- data.frame(Light[,2])
        Lightoff2 <- Lightoff2+1
        Lightoff2 <-rbind(cero,Lightoff2)
        LightOff.DF <- rowr::cbind.fill(Lightoff2,LightOff, fill=0)
        
        break_row <- 0
        for (i in 1:nrow(LightOff.DF)){
          value1 <- LightOff.DF[i,1]
          value2 <- LightOff.DF[i+1, 1]
          if (value1 > value2){
            break_row = i
            break
          } 
        }
        LightOff.DF <- LightOff.DF[1:break_row,]
        names(LightOff.DF)
        LightOff.DF <- LightOff.DF %>%
          dplyr::mutate (Light...1....1 = ifelse(Light...1....1 < Light...2., nrow(genotype.day.position.DF), Light...1....1))
        names(LightOff.DF)
        LightOff.DF <- LightOff.DF %>%
          dplyr::mutate(Light...1....1)
        d <- which.max(Light[,1])
        
        colnames(Light) <- c("On","Off")
        colnames(LightOff.DF) <- c("Off","On")
        
        LightOnIndex.DF  <- list()
        LightOffIndex.DF  <- list()
        for (i in 1:nrow(LightOff.DF)){
          LightOnIndex.DF[[i]] <- (seq(Light[i,1], Light[i,2],1))
          LightOffIndex.DF[[i]] <- (seq(LightOff.DF[i,1], LightOff.DF[i,2],1))

                  }
        l<-1
        
        LightOnIndex.DF.2 <-LightOnIndex.DF
        LightOffIndex.DF.2 <-LightOffIndex.DF
        LightOnIndex.DF.3 <-LightOnIndex.DF
        LightOffIndex.DF.3 <-LightOffIndex.DF
        long <- length(LightOnIndex.DF)
        for (i in 1:length(LightOnIndex.DF)) {
          
          prefix <- "pulse"
          prefix2 <- "nopulse"
          
          suffix <-data.frame(seq(1:long))
          print(length(LightOnIndex.DF[[i]]))
          for (j in 1:length(LightOnIndex.DF[[i]])) {
            
            LightOnIndex.DF[[i]][[j]]<-(paste(prefix,suffix[l,], sep = ""))
            LightOnIndex.DF.3[[i]][[j]]<-(paste(prefix, sep = ""))
            
          }
          l<-l+1
            
            
            
          
          
          
        }
        l<-1
        for (i in 1:length(LightOffIndex.DF)) {
          
          prefix <- "pulse"
          prefix2 <- "nopulse"
          suffix <-data.frame(seq(1:length(LightOffIndex.DF)))
          for (j in 1:length(LightOffIndex.DF[[i]])) {
            
            LightOffIndex.DF[[i]][[j]]<-(paste(prefix2,suffix[l,], sep = ""))
            LightOffIndex.DF.3[[i]][[j]]<-(paste(prefix2, sep = ""))
            
          }
          l<-l+1
          
          
          
          
          
          
        }
        LightOffIndex.DF <- do.call(rbind, lapply(LightOffIndex.DF, as.data.frame))
        LightOffIndex.DF <- as.matrix(LightOffIndex.DF)
       
        
        LightOnIndex.DF <- do.call(rbind, lapply(LightOnIndex.DF, as.data.frame))
        LightOnIndex.DF <- as.matrix(LightOnIndex.DF)
        
        
        LightOffIndex.DF.2 <- do.call(rbind, lapply(LightOffIndex.DF.2, as.data.frame))
        LightOffIndex.DF.2 <- as.matrix(LightOffIndex.DF.2)
        
        
        LightOnIndex.DF.2 <- do.call(rbind, lapply(LightOnIndex.DF.2, as.data.frame))
        LightOnIndex.DF.2 <- as.matrix(LightOnIndex.DF.2)
        
        LightOffIndex.DF.3 <- do.call(rbind, lapply(LightOffIndex.DF.2, as.data.frame))
        LightOffIndex.DF.3 <- as.matrix(LightOffIndex.DF.2)
        
        
        LightOnIndex.DF.3 <- do.call(rbind, lapply(LightOnIndex.DF.2, as.data.frame))
        LightOnIndex.DF.3 <- as.matrix(LightOnIndex.DF.2)
        
        
       
        statuson <- matrix("", nrow = nrow(genotype.day.position.DF), ncol= 1)
        statuson2 <- matrix("", nrow =nrow(genotype.day.position.DF), ncol= 1)
        for (i in 1:which.max(LightOnIndex.DF.2)) {
          
          
          statuson[LightOnIndex.DF.2[i,],] <- LightOnIndex.DF[i,]
          statuson2[LightOnIndex.DF.3[i,],] <- LightOnIndex.DF[i,]
        }
        statusoff <- matrix("", nrow = nrow(genotype.day.position.DF), ncol= 1)
        statusoff2 <- matrix("", nrow = nrow(genotype.day.position.DF), ncol= 1)
        for (i in 1:which.max(LightOffIndex.DF.2)) {
          
          
          statusoff[LightOffIndex.DF.2[i,],] <- LightOffIndex.DF[i,]
          statusoff2[LightOffIndex.DF.3[i,],] <- LightOffIndex.DF[i,]
        }
        statuson <-data.frame(statuson[,1])
        statuson2 <-data.frame(statuson2[,1])
        colnames(statuson)<- "B"
        colnames(statuson2)<- "B"
        statusoff <- data.frame(statusoff[,1])
        statusoff2 <- data.frame(statusoff2[,1])
        colnames(statusoff)<- "A"
        colnames(statusoff2)<- "A"
        stats <- cbind(statusoff,statuson)
        stats2 <- cbind(statusoff2,statuson2)
        df <- stats
        df2 <- stats2
        statusch <- as.matrix(paste(df$A,df$B, sep = ""))
        statusch[statusch==""]<-NA
        
        
       
        LightOnIndex.DF  <- list()
        for (i in 1:nrow(Light)){
          LightOnIndex.DF[[i]] <- (seq(Light[i,1], Light[i,2],1))
        }
        LightOnIndex.DF <- do.call(rbind, lapply(LightOnIndex.DF, as.data.frame))
        LightOnIndex.DF <- as.matrix(LightOnIndex.DF)
        
        
        print(nrow(statusch))
        print(nrow(genotype.day.position.DF))
        status <- matrix(0L, nrow = nrow(genotype.day.position.DF), ncol= 1)
        status[LightOnIndex.DF,] <- 1
        Light <- status
        
        
        for (i in 1:(as.numeric(ncol(genotype.day.position.DF) - 2)/2)){
                
                
                genotype.day.DF <- rbind(genotype.day.DF, bind.data(i, 
                                                                    genotype.day.position.DF, 
                                                                    genotype.day.mobility.DF, 
                                                                    genotype, 
                                                                    day,Light,statusch,
                                                                    genotype.day.head.DF,
                                                                    genotype.day.bending.DF))
        }
        genotype.day.DF %>% filter (!is.na(x), !is.na(y))
}
i<-1
n<-1

res <- NULL
n1 <- nrow(genotypes)
n2 <- 30

for (i in 1:n1){
  
       
        
        tryCatch({
                for (n in 1:n2) {
                  
                  print(n)
                        
                        
                        P <- import.experiment.by.genotpe (paste(as.character(genotypes[i,1]), sep = " ", collapse = NULL), 
                                                           paste(as.character(n), sep = " ", collapse = NULL))
                        
                        
                        assign(paste(as.character(genotypes[i,1]), paste(as.character(n), sep = " ", 
                                                                         collapse = NULL), sep=""),P)
                        res <- rbind(res,get(paste((genotypes[i,1]),paste(as.character(n)), sep = "")))
                        assign(paste(as.character(genotypes[i,1]), sep=""),res)
                        
                        
                }
                
        },error=function(e){})
        data.DF <- rbind(get(paste(as.character(genotypes[i,1]), sep="")))
}

TheAllPlot <- ggplot(data = data.DF, mapping = aes(x = x, y = y)) +
        geom_point(aes(color = genotype),alpha=0.4)
TheAllPlot


save(data.DF, file = "./CompleteDataSet.rda")

WormID.DF <- data.frame(unique(data.DF$id))
wid<-1
suma <- data.DF %>%
  dplyr::group_by(statusch) %>%
  dplyr::summarize(nums = max(time, na.rm= TRUE))

before <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                 mapping = aes(x = time, y = backward)) +
  geom_path() + 
  theme_minimal()

data.DF3 <- data.DF

data.DF <- data.DF %>% replace(.=="F", 0) %>%
  replace(.=="I", 0) %>%
  replace(.=="R", 1) %>%
  replace(.=="FALSE", 0)



flipit <- function(x){
  y<- data.frame(x[,6])
  y <- (y[1:24,])
  print(y)
  if (sum(y,na.rm = TRUE) >=42){
    print(y)
    x<- ifelse(x[,6]==1, 0,1)
  } else {
    
    x<-(x[,6])
  }
  
}

dat_split <- split(data.DF, f = data.DF$id) #splits data by worm
results = lapply(dat_split, flipit) ###applies function to flip if head is incorrectly identified from the begining of tracking
data.DF <- subset(data.DF, select = -c(backward) )
flipped.DF <- plyr::ldply(results, data.frame) ####converts list to rows to cbind to data frame

data.DF1 <- cbind(data.DF,flipped.DF[,2])
data.DF1 <-plyr::rename(data.DF1, c("flipped.DF[, 2]"="backward"))



ggplot(subset(data.DF1,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
       mapping = aes(x = time, y = backward)) +
  geom_path() + 
  theme_minimal()

ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
       mapping = aes(x = xhead, y = yhead)) +
  geom_path() + 
  theme_minimal()





#test12 <- findidxrev(data.DF$backward)

displacement <- function(x1,y1){
  x <- x1- lag(x1)
  y <- y1 - lag(y1)
  (sqrt((x^2)+(y^2)))
} ###Function to calculate displacement

magnitude <- function(x,y){
  x<- x^2
  y <- y^2
  a<- x+y
  sqrt(a)
  
}

data.DF <- data.DF1 %>% ##Make DF with smoothedspeed, position 
  dplyr::group_by(id) %>%
  dplyr::mutate(displacement.smooth = displacement(x,y),
                vx = xhead-x,
                vy = yhead-y,
                speed.smooth = displacement.smooth/0.125,
                displacement = displacement(x,y),
                speed = displacement/0.125,
                magnitude1 = magnitude(vx,vy)
  )




ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
       mapping = aes(x = time, y = backward)) +
  geom_path() + 
  theme_minimal()


findidxrev <- function(x){
  runs = rle(x ==1)
  
  myruns = (which(runs$values == TRUE & runs$lengths >= 60))
  
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  starts <- data.frame(starts)
  ends <- data.frame(ends)
  
  long.rev.index <- cbind(starts,ends)
}
x<- data.DF$magnitude1
findidxdist <- function(x){
  runs = rle(x <150)
  
  myruns = (which(runs$values == TRUE & runs$lengths >=1))
  
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  starts <- data.frame(starts)
  
}




findidxrev2 <- function(x){
  runs = rle(x ==1)
  
  myruns = (which(runs$values == TRUE & runs$lengths >= 3))
  
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  starts <- data.frame(starts)
  ends <- data.frame(ends)
  
  long.rev.index <- cbind(starts,ends)
}
test<- findidxdist(data.DF$magnitude1)
#idx = data.frame(which(test <200))
test2 <- findidxrev2(data.DF3$backward)

test3 <- findidxrev(data.DF3$backward)
k<-1

for (i in 1:nrow(test)) {
  
  for (l in 1:nrow(test2)) {
    print(k)
    if (is.na(test2[k,1])){
      k <-k+1
     
      
      
      
    } else if ((test[i,1]-test2[k,1]>0)) {
      test2[i,1:2] <- NA
      
      k <-k+1
    }
    
  }
  
}







k <-1
test2 <-na.omit(test2)
i<-1

y<- data.frame(data.DF3$backward)  
x <- data.frame(y)
for (i in 1:nrow(test2)) {
  y<- data.frame(data.DF3$backward)  
  
  y <- data.frame(y[test2[i,1]:test2[i,2],])
  
  print(sum(y))
  if (sum(y,na.rm = TRUE) >=40){

    x[test2[i,1]:test2[i,2],] <- 0
    
  } else {
    
    
  }
  
}
data.DFCW <- data.DF3



colnames(x) <- "backward2"

testy <-  rowr::cbind.fill(data.DF,x)
wid<-1

bxp <-ggplot(subset(testy,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
             mapping = aes(x = time, y = backward2)) +
  geom_path() + 
  theme_minimal()

bxp2 <-ggplot(subset(testy,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
              mapping = aes(x = time, y = magnitude1)) +
  geom_path() + 
  theme_minimal()

bxp3 <-ggplot(subset(data.DF1,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
              mapping = aes(x = time, y = backward)) +
  geom_path() + 
  theme_minimal()


figure <- ggpubr::ggarrange(bxp, bxp2,bxp3,
                            labels = c("A", "B","C"),
                            ncol = 1, nrow = 3)


figure


#save(data.DF, file = "./CompleteDataSet.rda")

####Data cleanup, slow worms and short tracks removed
####
####
wormstoremove.DF <- data.DF %>% 
  group_by(id,genotype) %>%
  dplyr::summarize(meanspeed= mean(speed, na.rm = TRUE),
                   time = max(time, na.rm = TRUE))
m <- max(data.DF$time, na.rm = TRUE)

wormstokeep.DF <- wormstoremove.DF %>%
  group_by(id)  %>%
  filter (
    #were observed for at least 2 mins and started at the buttom
    meanspeed > 100,
    time > m*0.50
  )


data.DF <- data.DF %>%
  filter (id %in% wormstokeep.DF$id)
for (i in 1:nrow(WormID.DF)){
  
  WormID.DF <- data.frame(unique(data.DF$id))
  wid <-i
  print(paste(WormID.DF[wid,1]))
  tracks.plot <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", 
                                                       collapse = NULL))), mapping = aes(x = lag(x), y = lag(y), group = id)) +
    
    geom_path(data=subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
              aes(x=x, y=y, group = id), color='black', size = 1) + 
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


for (i in 1:nrow(WormID.DF)){
  
  WormID.DF <- data.frame(unique(data.DF$id))
  wid <-i
  print(paste(WormID.DF[wid,1]))
  bxp <-ggplot(subset(testy,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
               mapping = aes(x = time, y = backward2)) +
    geom_path() + 
    theme_minimal()
  
  bxp2 <-ggplot(subset(testy,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                mapping = aes(x = time, y = magnitude1)) +
    geom_path() + 
    theme_minimal()
  
  bxp3 <-ggplot(subset(data.DF3,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                mapping = aes(x = time, y = backward)) +
    geom_path() + 
    theme_minimal()
  
  
  figure <- ggpubr::ggarrange(bxp, bxp2,bxp3,
                              labels = c("A", "B","C"),
                              ncol = 1, nrow = 3)
  
  
  ggsave(paste(as.character(WormID.DF[wid,1]),"correction", "pdf", sep = "."))
  
  
  
  
}

#save(data.DF, file = "./CompleteDataSet.rda")

