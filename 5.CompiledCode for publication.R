##################This code analyzes data extracted from MBF Bioscience WormLab###########################
##################Data required: Position (x,y) coordinates###############################################
##################Data required: Mobility (Forward/Backward Movement)#####################################
##################Created by Pablo J Maldonad 2019########################################################
##########################################################################################################

##################
rm(list=ls()) ####Cleans global environment

####################Load R Packages#######################################################################
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(rowr)


setwd("~/Desktop/R Code to Review/03112021")
load('./datasmoothandfull.rda') #Loads cleaned data
#position <- select(data.DF, c("x","y"))
#position1 <- select(data.DF, c("smoothx","smoothy"))
##################User made functions
displacement <- function(x1,y1){
        x <- x1- lag(x1)
        y <- y1 - lag(y1)
        (sqrt((x^2)+(y^2)))
}

wvector <- function(x){
        (x)-lag(x)
}
rvector <- function(x1,x2){
        x2-x1
}
anglecalc <- function(x1,y1,x2,y2){
        cbind(acos((x1*(x2)+y1*(y2))/(sqrt((x1^2)+(y1^2))*sqrt((x2^2)+(y2^2))))*(180/pi))
        
}

anglecalc2 <- function(x1,y1,x2,y2){
        
        theta <- (x1*(x2)+y1*(y2))/(sqrt((x1^2)+(y1^2))*sqrt((x2^2)+(y2^2)))
        cbind(acos(pmin(pmax(theta,-1.0),1.0)))*(180/pi)
        
}



###############Start of analysis
data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(displacement = displacement(x,y),
               wormx = wvector(x),
               wormy = wvector(y),
               refx = rvector(x,xref),
               refy = rvector(y,yref),
               bearing.angle = anglecalc2(wormx,wormy,refx,refy),
               speed = displacement/0.125)

datasum <- data.DF %>%
        group_by(id,genotype) %>%
        dplyr::summarize(totaldist = sum(displacement, na.rm = TRUE),
                         timetotarget = (n()/8)/60,
                         speed = mean(speed,na.rm = TRUE))

genotype <- datasum %>%
        group_by(genotype) %>%
        dplyr::summarize(meandist = mean(totaldist, na.rm = TRUE),
                         meantime = mean(timetotarget),
                         avgspeed = mean(speed))




###################g#################################################################
#################################################################################################
#############                                                           #########################
##############                  Weathervaning                            #########################
##############                                                          #########################
#################################################################################################
#################################################################################################

xvector <- function(x){
        (x)-lag(x)
}
displacementf <- function(x1,y1){
        (sqrt((x1^2)+(y1^2)))
}
cumSkipNA <- function(x, FUNC)
{
        d <- deparse(substitute(FUNC))
        funs <- c("max", "min", "prod", "sum")
        stopifnot(is.vector(x), is.numeric(x), d %in% funs)
        FUNC <- match.fun(paste0("cum", d))
        x[!is.na(x)] <- FUNC(x[!is.na(x)])
        x
}
data.DF2 <-data.DF %>% 
        dplyr::group_by(id) %>%
        dplyr::mutate(xv = xvector(x),
                      yv = xvector(y),
                      displacement =(displacementf(x1= xv, y1 = yv)),
                      cumdisp = cumSkipNA(displacement,sum))


data.DF <- data.DF2
data.DF <- select(data.DF, "x","y","id", "time","bearing.angle","cumdisp")
data.DF <- data.DF %>% replace(is.na(.), 0)
library(magicfor)               # load library
magic_for(print, silent = TRUE)
for (i in seq(0,max(data.DF$cumdisp, na.rm =TRUE),10)){
        
        print(data.DF %>% dplyr::group_by(id) %>% dplyr::filter(abs(cumdisp - i) == min(abs(cumdisp - i))))
        
}
test <- magic_result_as_dataframe()
magic_free()
n <- length(unique(data.DF$id))
ro<- nrow(test)*n  ###NEED Number of worms here
x <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
            dimnames = NULL)
y <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
            dimnames = NULL)
id <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
             dimnames = NULL)
#gen <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
#dimnames = NULL)
time <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
               dimnames = NULL)
#backward <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
# dimnames = NULL)
bearing.angle <- matrix(data = NA, nrow = ro, ncol = 1, byrow = FALSE,
                        dimnames = NULL)
m <- matrix(rep(c(1:ro)), ncol=n, nrow=nrow(test))
for (n in 1:n) {
        
        for (i in 1:nrow(test)) {
                
                x[m[i,n]] <- as.numeric((test[[i,2]][n,1]))
                y[m[i,n]] <- as.numeric((test[[i,2]][n,2]))
                id[m[i,n]] <- data.frame((test[[i,2]][n,3]))
                #gen[m[i,n]] <- data.frame((test[[i,2]][n,5]))
                time[m[i,n]] <- data.frame((test[[i,2]][n,4]))
                #backward[m[i,n]] <- data.frame((test[[i,2]][n,6]))
                bearing.angle[m[i,n]] <- data.frame((test[[i,2]][n,5]))
                
                
        }
        
        
        
}
wv.coord <- cbind.fill(x,y,id,time,bearing.angle)   
wv.coord2 <- data.frame(cbind(x,y,id,time,bearing.angle)) 
wv.coord2[,1] <- as.numeric(wv.coord2[,1])
wv.coord2[,2] <- as.numeric(wv.coord2[,2])
wv.coord2[,3] <- as.numeric(wv.coord2[,3])
wv.coord2[,4] <- as.numeric(wv.coord2[,4])
wv.coord2[,5] <- as.numeric(wv.coord2[,5])
load("./dataforcont.rda")
colnames(wv.coord) <- c("x","y","id","time","bearingangle")
#wv.coord <- wv.coord2

magnitude <- function(x,y){
        x<- x^2
        y <- y^2
        a<- x+y
        sqrt(a)
        
}

dotprod <- function(x1,y1,x2,y2){
        (x1*x2)+(y1*y2)
}
anglecalc <- function(x1,y1,x2,y2){
        (cbind(acos((x1*(x2)+y1*(y2))/(sqrt((x1^2)+(y1^2))*sqrt((x2^2)+(y2^2))))*(180/pi)))
        
}
wv.coord <- dplyr::distinct(wv.coord,time, .keep_all= TRUE)
data1 <- wv.coord %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(v1x = lag(((x))-lag(x)),
                      v1y = lag((y)-lag(y)),
                      v2x = (lag(x)-x),
                      v2y = (lag(y)-y),
                      magnitude1 = magnitude(v1x,v1y),
                      magnitude2 = magnitude(v2x,v2y),
                      dot = dotprod(v1x,v1y,v2x,v2y),
                      beforecos = dot/(magnitude1*magnitude2),
                      steeringangle =180- (180/pi)*acos(pmin(pmax(beforecos,-1.0),1.0)))

data.complete.DF <- data.DF2 %>%
        select("time","x","y","id","genotype","backward","bearing.angle","speed","cumdisp")



steering.angle <- data.frame(data1$steeringangle)
colnames(steering.angle) <- "steering.angle"

data.complete.DF <- cbind.fill(data.complete.DF,steering.angle)
save(data.complete.DF, file = ".//completedata.rda")  


good <- ggplot(data1, aes(x = (x),y = (y)))+
        geom_point(aes(color = (steeringangle)),size =1 ) +coord_fixed(ratio=1)
good




good + scale_color_gradient(low="yellow", high="red", limits = c(0,180)) 

+ 
        ggrepel::geom_text_repel(data = subset(data1, steeringangle > 100),
                                 aes(label = round(steeringangle), direction  = "y"))
ggsave("weathervanevalidation.pdf")
plot(data1$time,data1$steeringangle, type = "l")
save(data1, file = ".//datasampledat10um.rda")  

