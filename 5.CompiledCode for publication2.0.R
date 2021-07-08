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
library(DescTools)

setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")
load('./FullDataCor.rda') #Loads cleaned data
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

cumSkipNA <- function(x, FUNC)
{
        d <- deparse(substitute(FUNC))
        funs <- c("max", "min", "prod", "sum")
        stopifnot(is.vector(x), is.numeric(x), d %in% funs)
        FUNC <- match.fun(paste0("cum", d))
        x[!is.na(x)] <- FUNC(x[!is.na(x)])
        x
}
displacementf <- function(x1,y1){
        (sqrt((x1^2)+(y1^2)))
}
xvector <- function(x){
        (x)-lag(x)
}

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
###############Start of analysis
data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(displacement = displacement(smoothx,smoothy),
               cumdisp = cumSkipNA(displacement,sum),
               wormx = wvector(x),
               wormy = wvector(y),
               refx = rvector(x,xref),
               refy = rvector(y,yref),
               bearing.angle = anglecalc2(wormx,wormy,refx,refy),
               wormxsmooth = wvector(smoothx),
               wormysmooth = wvector(smoothy),
               smoothxref = rvector(smoothx, xref),
               smoothyref = rvector(smoothy, yref),
               bearing.angle.smooth = anglecalc2(wormxsmooth,wormysmooth,smoothxref,smoothyref),
               speed = displacement/0.125,
               distance.target = magnitude(smoothxref,smoothyref))
save(data.DF, file = ".//databearing.rda")
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


data.DF <-data.DF %>% 
        dplyr::group_by(id) %>%
        dplyr::mutate(xv = xvector(smoothx),
                      yv = xvector(smoothy),
                      displacement =(displacementf(x1= xv, y1 = yv)),
                      cumdisp = cumSkipNA(displacement,sum))
FindMultiple <- function(x) {
        x%%1000
}
data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(RoundedDisp = RoundTo(cumdisp, multiple = 100, FUN = round))

data.DF <- data.DF %>% 
        group_by(id) %>%
        mutate(Divisor = FindMultiple(RoundedDisp))

WeatherVaning.Coord.DF <- data.DF %>%
        group_by(id) %>%
        filter(Divisor==0)


WeatherVaning.Coord.DF <- WeatherVaning.Coord.DF %>%
        group_by(id) %>%
        mutate(testy = lag(RoundedDisp)-RoundedDisp) %>%
        filter(testy==-1000)


data1 <- WeatherVaning.Coord.DF %>%
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

data1<- data1 %>%
        select(time, id, genotype,Assay.ID, steeringangle)
data.DF <- data.DF %>%
        select(c(time,x,y,id,genotype,Assay.ID,smoothx,smoothy,xref,yref,date,backward.cor,
                 RevCount,speed,bearing.angle,bearing.angle.smooth,
                 distance.target,cumdisp))
testy <- data.DF %>%
        select(c(id,Assay.ID))
# df3 <- merge(testy, data1, by=("id"))
###################g#################################################################
#################################################################################################
#############                                                           #########################
##############                  Weathervaning                            #########################
##############                                                          #########################
#################################################################################################
#################################################################################################

save(data1, file = ".//completedata.rda")  
break

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
