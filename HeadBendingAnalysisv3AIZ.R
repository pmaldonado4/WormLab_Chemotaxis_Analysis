library(tidyverse)
#library(dplyr)
#library(plyr)
#library(ggplot2)
library(reshape2)

library(gridExtra)
library(data.table)
library(rowr)
rm(list=ls())
setwd("~/Desktop/newfuckingdata/")
load('./FullDataCor.rda')

# data.DF1 <- data.DF %>%
#   filter(Light==1)
# 
# chek <- data.DF1 %>%
#   group_by(id,statusch,genotype) %>%
#   summarize(mean(bending.angle.V3))
# 
# datate <- data.DF1 %>%
#   group_by(id, statusch,genotype) %>%
#   summarise(num_pos = sum(bending.angle.V3 > 0), 
#             num_neg = sum(bending.angle.V3 < 0),
#             total = sum(bending.angle.V3 > 0)+ sum(bending.angle.V3 < 0),
#             per_pos = num_pos/(num_pos+num_neg))
# 
# 
# side.DF <- datate %>%
#    group_by(id) %>%
#    summarize(meanside_one = mean(per_pos))
#  
# side_one.DF <- side.DF %>%
#   group_by(id) %>%
#   filter(meanside_one > 0.70)
# 
# side_two.DF <- side.DF %>%
#   group_by(id) %>%
#   filter(meanside_one < 0.70)
# 
# side_one_worms.DF <- datate %>%
#   filter (id %in% side_one.DF$id)
# 
# side_one_worms.DF <- side_one_worms.DF %>%
#   filter(!per_pos > 0.6)
# 
# side_two.worms.DF <- datate %>%
#   filter (id %in% side_two.DF$id)
# 
# side_two_worms.DF <- side_two.worms.DF %>%
#   filter(per_pos > 0.2)
# 
# ##########################################
# side_one_worms.DF <- side_one_worms.DF %>%
#   mutate (id_statusch = paste (id, statusch))
# 
# 
# data.DF.Polina <- data.DF %>%
#   mutate (id_statusch = paste (id, statusch)) %>%
#   filter (!id_statusch %in% side_one_worms.DF$id_statusch)
# 
# side_two_worms.DF <- side_two_worms.DF %>%
#   mutate (id_statusch = paste (id, statusch))
# 
# 
# data.DF.Polina.2 <- data.DF %>%
#   mutate (id_statusch = paste (id, statusch)) %>%
#   filter (!id_statusch %in% side_two_worms.DF$id_statusch)
# ##########################################
# 
# 
# data.DF <-data.DF.Polina


data.DF <- data.DF %>%
        data.frame(status = case_when(
                data.DF$statusch %like% "^puls" == 0 ~ "nopulse",
                data.DF$statusch %like% "^nopuls" == 0 ~ "pulse",
                TRUE ~ as.character(data.DF$statusch)
        ))

WormID.DF <- data.frame(unique(data.DF$id))

lidx <-which(data.DF$Light == 1)
Light <- data.frame(data.DF[lidx,8],data.DF[lidx,9])
colnames(Light) <- c("xhead","yhead")
ggplot(data = data.DF, x = time, y = bending.angle.V3) +
        geom_line(aes(x = time, y = bending.angle.V3))
df <- data.DF %>% group_by(id,genotype) %>%
        dplyr:: select(starts_with("bending"))
what <- group_split(df)
tplist <- list()
tplist2 <- list()
first.bending.angle.a<- list()
second.bending.angle.a<- list()
third.bending.angle.a<- list()
first.bending.angle.b<- list()
second.bending.angle.b<- list()
third.bending.angle.b<- list()
id.names <- unique(data.DF$id)
for (i in 1:length(what)) {
        x <- what[[i]]
        
        y <- x[,1]
        
        x<- x[,3:ncol(x)]
        
        tplist[[i]] <-(((lapply(x, IDPmisc::peaks, minPW=2, thr=-50, stepF = 0.49))))
        
        tplist2[[i]] <-(((lapply(x*-1, IDPmisc::peaks, minPW=2, thr=-50, stepF = 0.49))))
        
        first.bending.angle.a[[i]] <-tplist[[i]][[1]]
        second.bending.angle.a[[i]] <-tplist[[i]][[1+1]]
        third.bending.angle.a[[i]] <-tplist[[i]][[1+2]]
        
        first.bending.angle.b[[i]] <-tplist2[[i]][[1]]
        second.bending.angle.b[[i]] <-tplist2[[i]][[1+1]]
        third.bending.angle.b[[i]] <-tplist2[[i]][[1+2]]
}
#may need to apply thisall twice for the second side of the wave
#sum((first.bending.angle[[seq(1,10,1)]][[1]]))
limits1st.a <- matrix(data= NA, ncol=1, nrow = 1000 )
limits2nd.a <- matrix(data= NA, ncol=1, nrow = 1000 )
limits3rd.a <- matrix(data= NA, ncol=1, nrow = 1000 )

limits1st.b <- matrix(data= NA, ncol=1, nrow = 1000 )
limits2nd.b <- matrix(data= NA, ncol=1, nrow = 1000 )
limits3rd.b <- matrix(data= NA, ncol=1, nrow = 1000 )
for (i in 1:length(id.names)) {
       
        limits1st.a[i]<- length(first.bending.angle.a[[i]][[1]])
        limits2nd.a[i]<- length(second.bending.angle.a[[i]][[1]])
        limits3rd.a[i]<- length(third.bending.angle.a[[i]][[1]])
        
        limits1st.b[i]<- length(first.bending.angle.b[[i]][[1]])
        limits2nd.b[i]<- length(second.bending.angle.b[[i]][[1]])
        limits3rd.b[i]<- length(third.bending.angle.b[[i]][[1]])
      
}
limits.a <- cbind.fill(limits1st.a,limits2nd.a,limits3rd.a)
limits.a <- data.frame(cumsum(limits.a))

limits.b <- cbind.fill(limits1st.b,limits2nd.b,limits3rd.b)
limits.b <- data.frame(cumsum(limits.b))

limits1st.a <-data.frame(cumsum(limits1st.a))
limits2nd.a <- data.frame(cumsum(limits2nd.a))
limits3rd.a <-data.frame(cumsum(limits3rd.a))

limits1st.b <-data.frame(cumsum(limits1st.b))
limits2nd.b <- data.frame(cumsum(limits2nd.b))
limits3rd.b <-data.frame(cumsum(limits3rd.b))
x1<-1

limits.a <- cbind.fill(limits1st.a,limits2nd.a,limits3rd.a)
limits.b <- cbind.fill(limits1st.b,limits2nd.b,limits3rd.b)
name.DF1.a <- matrix(data= NA, ncol=1, nrow = 1000)
name.DF2.a <- matrix(data= NA, ncol=1, nrow = 1000)
name.DF3.a <- matrix(data= NA, ncol=1, nrow = 1000 )

name.DF1.b <- matrix(data= NA, ncol=1, nrow = 1000)
name.DF2.b <- matrix(data= NA, ncol=1, nrow = 1000)
name.DF3.b <- matrix(data= NA, ncol=1, nrow = 1000 )
for (i in 1:length(id.names)) {
  
  for (l in x1:limits.a[i,1]) {
    #print(l)
    name.DF1.a[l] <- id.names[i]
    
  }
  x1<-limits.a[i,1]
  
}
x1<-1
for (i in 1:length(id.names)) {
  
  for (l in x1:limits.a[i,2]) {
    print(x1)
    
    name.DF2.a[l] <- id.names[i]
    
    
  }
  
  x1<-limits.a[i,2]
  
}
x1<-1
for (i in 1:length(id.names)) {
  
  for (l in x1:limits.a[i,3]) {
    #print(l)
    name.DF3.a[l] <- id.names[i]
    
  }
  x1<-limits.a[i,3]
  
}
bendingangle.v3.a <- rlist::list.stack(first.bending.angle.a)
bendingangle.v4.a <- rlist::list.stack(second.bending.angle.a)
bendingangle.v5.a <- rlist::list.stack(third.bending.angle.a)

bendingangle.v3.a <- cbind.fill(bendingangle.v3.a,name.DF1.a,fill = NA)
bendingangle.v4.a <- cbind.fill(bendingangle.v4.a,name.DF2.a, fill = NA)
bendingangle.v5.a <- cbind.fill(bendingangle.v5.a,name.DF3.a, fill = NA)


#fr other side (b)
x1<-1
for (i in 1:length(id.names)) {
       
        for (l in x1:limits.b[i,1]) {
                #print(l)
                name.DF1.b[l] <- id.names[i]
                
        }
        x1<-limits.b[i,1]
        
}
x1<-1
for (i in 1:length(id.names)) {
        
        for (l in x1:limits.b[i,2]) {
          print(x1)
                
                name.DF2.b[l] <- id.names[i]
               
                
        }
  
        x1<-limits.b[i,2]
        
}
x1<-1
for (i in 1:length(id.names)) {
        
        for (l in x1:limits.b[i,3]) {
                #print(l)
                name.DF3.b[l] <- id.names[i]
                
        }
        x1<-limits.b[i,3]
        
}

bendingangle.v3.b <- rlist::list.stack(first.bending.angle.b)
bendingangle.v4.b <- rlist::list.stack(second.bending.angle.b)
bendingangle.v5.b <- rlist::list.stack(third.bending.angle.b)

bendingangle.v3.b <- cbind.fill(bendingangle.v3.b,name.DF1.b,fill = NA)
bendingangle.v4.b <- cbind.fill(bendingangle.v4.b,name.DF2.b, fill = NA)
bendingangle.v5.b <- cbind.fill(bendingangle.v5.b,name.DF3.b, fill = NA)









allangles.DF <- bendingangle.v3.a
allangles.DF2 <- bendingangle.v3.b
peks <- allangles.DF %>%
        dplyr::mutate(x = x/8,
                      w = w/8) %>%
        dplyr::rename(time.peak = x,
                      width.seconds.peak = w,
                      peak.value = y,
                      angle.id = object)

peks2 <- allangles.DF2 %>%
        dplyr::mutate(x = x/8,
                      w = w/8) %>%
        dplyr::rename(time.peak2 = x,
                      width.seconds.peak2 = w,
                      peak.value2 = y,
                      angle.id2 = object)


AngID <- data.frame(unique(peks$angle.id))

data.DF.Light <- data.DF[lidx,]
WormID<- data.frame(unique(data.DF$id))


data.complete.DF <- cbind.fill(peks,peks2, fill = NA)

save(data.complete.DF, file = "./CompleteBendingAngleDataSet1.rda")
worm <-1
angid<-worm
wid<-worm
x <- subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL)))
x1 <- x[1,8]
y1 <- x[1,9]


for (i in 1:nrow(WormID.DF)){
  worm <-i
  angid<-worm
  wid<-worm
  x <- subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL)))
  x1 <- x[1,8]
  y1 <- x[1,9]
  track.plot <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), 
                       mapping = aes(x = xhead, y = yhead))+
    
    geom_path() +
    
    geom_point(subset(data.DF.Light,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))),
               mapping = aes(x=xhead, y= yhead), color= "red") +
    
    coord_fixed(ratio = 1, xlim = (NULL), ylim = NULL, expand = TRUE,
                clip = "on") +
    
    theme_classic() +
    
    annotate(geom="text", x=x1, y=y1+300, label="Start",
             color="red", size = 6) 
  
  track.plot
  
  
  ggsave(paste(as.character(WormID.DF[wid,1]), "pdf", sep = "."))
  peak.plot <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL))), 
                      mapping = aes(x =time, y = (bending.angle.V3))) +
    geom_ribbon(aes(ymin=0, ymax=(bending.angle.V3)), fill="red", color="lightpink3") +
    geom_line() +
    geom_point(subset(peks,angle.id %in% c(paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL))),
               mapping = aes(x = time.peak-0.100, y = peak.value), color="red") +
    geom_point(subset(peks2,angle.id2 %in% c(paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL))),
               mapping = aes(x = time.peak2-0.10, y = -peak.value2), color= "blue") +
    #geom_point(subset(data.DF.Light,id %in% c(paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL)))
    #           , mapping = aes(x =time, y = abs(bending.angle.V3)), color="yellow") +
    theme_classic() + 
    theme(legend.position = "none") +
    geom_hline(aes(yintercept= 90, linetype= "dashed", color= "red")) +
    geom_hline(aes(yintercept= -90, linetype= "dashed", color= "blue")) +
    geom_hline(aes(yintercept= 0), linetype= "dashed", color= "black") +
    geom_line(subset(data.DF,id %in% c(paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL))), 
              mapping = aes(x =time, y = 100*Light)) +
    geom_line(subset(data.DF,id %in% c(paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL))), 
              mapping = aes(x =time, y = -100*Light)) +
    theme_classic() + 
    theme( legend.position = "none") +
    annotate(geom="text", x=5, y=120, label=paste(as.character(WormID[wid,1]), sep = " ", collapse = NULL),
             color="red", size = 6) 
  
  
  peak.plot
  
  ggsave(paste(as.character(WormID.DF[wid,1]), "peak", "pdf", sep = "."))
  
}
displacement <- function(x1,y1){
  x <- x1- lag(x1)
  y <- y1 - lag(y1)
  (sqrt((x^2)+(y^2)))
}

data.DF <- data.DF %>%
  group_by(id) %>%
  mutate(displacement = displacement(x,y), 
         speed = displacement/0.125)

meanspeed.DF <- data.DF %>% 
  group_by(id) %>%
  dplyr::summarise(meanspeed = mean(speed, na.rm = T))

peakdif <- data.frame(peks2$peak.value2-peks $peak.value)
#####Finding peak bending angle value during presentation of light and absence of light
#####
#####
#####


Lightoff.DF <- data.DF %>%
  group_by(id,statusch,status) %>%
  dplyr::summarize(maximum = max(abs(bending.angle.V3)),
            minimum = min(abs(bending.angle.V3)),
            meanval = mean(abs(bending.angle.V3)))

suma <- Lightoff.DF %>%
  group_by(id,status) %>%
  dplyr::summarize(ma = mean(maximum),
            mi = mean(minimum),
            mm = mean(meanval))

#######Evaluation of reversals after lightoffset with customizable range

data.DF <- data.DF %>%
  group_by(id) %>%
  mutate(Light = Light - lag(Light))
data.norev.DF <- data.DF %>%
  filter(!backward ==1)

Lightoffidx.DF<- data.frame(which(data.norev.DF$Light==-1))
Lightoffidxrange.DF<- data.frame(which(data.norev.DF$Light==-1) +4)

LightOnIndex.DF  <- list()
LightOffIndex.DF  <- list()
for (i in 1:nrow(data.DF)){
  LightOnIndex.DF[[i]] <- (seq(Lightoffidx.DF[i,1], Lightoffidxrange.DF[i,1],1))
  
  
}

LightOnIndex.DF <- do.call(rbind, lapply(LightOnIndex.DF, as.data.frame))
LightOnIndex.DF <- as.matrix(LightOnIndex.DF)




data.LightOff.DF <- data.norev.DF[LightOnIndex.DF,]

MeanAngle.DF <- data.LightOff.DF %>%
  group_by(id,statusch,genotype) %>%
  dplyr::summarise(meanang = mean(abs(bending.angle.V4)))

summary.DF <- MeanAngle.DF %>%
  group_by(id,genotype)%>%
  dplyr::summarise(meanperworm = mean(meanang))


summary2.DF <- summary.DF%>%
  group_by(genotype) %>%
  dplyr::summarise(meanang = mean(meanperworm))


#####Quantifying responses to light that are reversals (reversals after light comes on)
#####
Lightonidx.DF<- data.frame(which(data.DF$Light==1))
Lightonidxrange.DF<- data.frame(which(data.DF$Light==1) +8)

LightOnIndex.DF  <- list()

for (i in 1:nrow(data.DF)){
  LightOnIndex.DF[[i]] <- (seq(Lightonidx.DF[i,1], Lightonidxrange.DF[i,1],1))
  
  
}

LightOnIndex.DF <- do.call(rbind, lapply(LightOnIndex.DF, as.data.frame))
LightOnIndex.DF <- as.matrix(LightOnIndex.DF)

data.LightOn.DF <- data.DF[LightOnIndex.DF,]

data.LightOn.DF <- data.LightOn.DF %>%
  group_by(id) %>%
  mutate(backward = backward.cor - lag(backward.cor))

data.LightOn.DF$backward[data.LightOn.DF$backward == -1] <- 0
data.LightOn.DF$Light[data.LightOn.DF$Light == -1] <- 0 
time.DF <- data.LightOn.DF %>%
  dplyr::group_by (id,genotype) %>%
  dplyr::summarize(time_observed = max ((time) - min (time))/60,
                   time.seconds = max(time)-min(time),
                   numrev = sum(backward,na.rm =T),
                   numpulsesreceived = sum(Light, na.rm = T)/2)

