#####################################3
#This script load and explores worm data
#Created by Pablo Maldonado on 08/24/2019
######################################
rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(ggpubr)

setwd("~/Desktop/R Code to Review/MutantsUpdated/")


load('./CompleteDataSet.rda')

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
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
displacement <- function(x1,y1){
        x <- x1- lag(x1)
        y <- y1 - lag(y1)
        (sqrt((x^2)+(y^2)))
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
# 
data.DF <- data.DF %>%
        mutate(Assay.ID = paste(genotype, assay, sep = "."),
               Speed = displacement(x,y)/0.125)
#plotting original data
data.DF <- data.DF %>%
        mutate(Assay.ID = paste(genotype, assay, sep = "."))

data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(Speed = displacement(x,y)/0.125,
               cumdisp = cumSkipNA(displacement(x,y),sum))

Assay.Parameters.DF <- data.DF %>%
        dplyr::group_by(Assay.ID) %>%
        dplyr::summarize(maxY = max(y, na.rm = T)*0.9,
                         maxX = max(x)-1000,
                         minX = min(x)-1000,
                         speed = mean(Speed,na.rm = T))
data.DF <- data.DF %>%
        left_join(Assay.Parameters.DF, by = "Assay.ID")

#####Removing short tracks and worms that are impossibly fast and did not move
#####
#####
all.worms.DF <- data.DF %>%
        dplyr::group_by (id, genotype) %>%
        dplyr::summarize (y_min = min(y), 
                          y_max = max(y), 
                          y_distance = max (y) - min (y),
                          time_min = min (time), 
                          time_max = max (time), 
                          time_observed = max (time) - min (time),
                          maxX = max(x),
                          minX = min(x),
                          meanspeed = mean(Speed, na.rm=T),
                          maxdistance = max(cumdisp, na.rm = T)) %>%
        mutate (rate.change.y = y_distance/time_observed,)

real.worms.DF <- all.worms.DF %>%
        filter (
                #were observed for at least 1 mins and 30 and started at the bottom
                time_observed > 120,
                meanspeed > 100,
                meanspeed < 500)

real.tracks.DF <- data.DF %>%
        filter (id %in% real.worms.DF$id)
####Filter worms that crawled to the sides and out of the field of view
####

out.of.view <- real.tracks.DF %>%
        group_by(id) %>%
        dplyr::summarise(minx = min(x),
                         maxx = max(x)) 
print("presumed number of presumed worms")
length(unique(out.of.view$id))

real.tracks.DF <- left_join(real.tracks.DF, out.of.view, by = "id") 

out.of.view.worms <- real.tracks.DF %>%
        group_by(id) %>%
        filter(maxx < maxX)
length(unique(out.of.view.worms$id))
# out.of.view.DF <- real.tracks.DF %>%
#         filter(id %in% out.of.view.worms$id)


all.tracks.DF <- out.of.view.worms

all.worms.DF <- all.tracks.DF %>%
        dplyr::group_by (id, genotype) %>%
        dplyr::summarize (y_min = min(y), 
                          y_max = max(y), 
                          y_distance = max (y) - min (y),
                          time_min = min (time), 
                          time_max = max (time), 
                          time_observed = max (time) - min (time),
                          maxX = max(x),
                          minX = min(x),
                          meanspeed = mean(Speed, na.rm=T),
                          maxdistance = max(cumdisp, na.rm = T)) %>%
        mutate (rate.change.y = y_distance/time_observed,)

real.worms.DF <- all.worms.DF %>%
        filter (
                
                y_distance > 20000
        )

test.worm <- real.tracks.DF %>%
        filter(id == "glr1.9.194")
out.of.view.plot <- ggplot(data = test.worm, mapping = aes(x = x, y = y)) +
        geom_path(aes(color = id), alpha=0.4)+
        facet_wrap( ~ genotype, ncol=2) +
        theme(legend.position = "none") +
        labs(title = 'All worms') +
        coord_fixed(ratio=1)

out.of.view.plot
table (all.worms.DF$genotype)
table (real.worms.DF$genotype)

tgc <- summarySE(real.worms.DF, measurevar="time_observed", groupvars=c("genotype"))
h <- ggplot(real.worms.DF, aes(x=time_observed, color=genotype)) +
        geom_density(fill="white", alpha=0.02,) +
        geom_density()+
        facet_wrap(~genotype)+
        geom_vline(data=tgc, aes(xintercept=time_observed, color=genotype),
                   linetype="dashed") 



h
ggsave("timetotargetdistribution.pdf")


rel.worm.DF <- all.tracks.DF %>%
        dplyr::group_by (id,genotype) %>%
        dplyr::summarize (y_min = min(y), 
                          y_max = max(y), 
                          y_distance = max (y) - min (y),
                          time_min = min (time), 
                          time_max = max (time), 
                          time_observed = max (time) - min (time)) %>%
        mutate (rate.change.y = y_distance/time_observed)

save(rel.worm.DF, file = "./completedatawithdetailsofdetection.rda")

AllWorms.DF <- data.DF %>%
        filter ((id %in% real.worms.DF$id))
save(AllWorms.DF, file = ".//full.position.rda")
real.worms.data.DF <- AllWorms.DF %>% 
        mutate(backward = recode(backward, 'F' = 0, 'R' = 1, 'I' = 0, 'FALSE' = 0, '1'= 1, '0' =0))

save(real.worms.data.DF, file = ".//Realwormsforanalysis.rda")

###############Adding Odorant Coordinates and smoothing tracks

data.DF <- real.worms.data.DF

coordinates <- read.csv("CompleteIndex.csv", header = T)
coordinates <- coordinates %>%
        dplyr::rename(Assay.ID= id)

data.DF <- data.DF %>%
        select(c(time,x,y,id,backward,Assay.ID))

data.DF <- data.DF %>%
        left_join(coordinates, by = "Assay.ID")

data.DF <- data.DF %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(smoothx = pracma::movavg(x, 45, "s")) %>%
        dplyr::mutate(smoothy = pracma::movavg(y, 45, "s"))
save(data.DF, file = ".//datasmoothandfull.rda")


load(".//datasmoothandfull.rda")

#################Correction of head/tail errors
#################
#################


WormID.DF <- data.frame(unique(data.DF$id))  ####Makes d.f with list of worm names
gentype <- data.frame(unique(data.DF$genotype))  ####DF with list of genotypes
wid <-2 ####Worm to be ploted in wormid.DF list

ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
       mapping = aes(x = x, y = y)) +
        geom_path() + 
        theme_minimal() +
        coord_fixed(ratio = 1) 

test.worm <- data.DF %>%
        filter(id == "double.6.55")
before <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))),
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
break

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

save(file = "rawestdata.rda", data.DF)
for (i in 1:nrow(WormID.DF)) {
        
        wid <-i
        before <- ggplot(subset(data.DF,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                         mapping = aes(x = time, y = backward)) +
                geom_path() + 
                theme_minimal() +
                ylim(c(0,1))
        
        
        
        after <- ggplot(subset(data.new,id %in% c(paste(as.character(WormID.DF[wid,1]), sep = " ", collapse = NULL))), ###Plots track of sample worm, quality check #1
                        mapping = aes(x = time, y = corr..i..)) +
                geom_path() + 
                theme_minimal() +
                ylim(c(0,1))
        ggpubr::ggarrange(before, after, 
                          labels = c("before", "after correction"),
                          ncol = 1, nrow = 2)
        
        ggsave(paste(as.character(WormID.DF[wid,1]),"correctedmovement", "pdf", sep = "."))
} ###Plot before and after for quality check


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




#####Bearing angle and steering angle
#####
#####

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

rm(list=ls())

load(".//databearing.rda")
FindMultiple <- function(x) {
        x%%500
}
data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(RoundedDisp = DescTools::RoundTo(cumdisp, multiple = 100, FUN = round))

data.DF <- data.DF %>% 
        group_by(id) %>%
        mutate(Divisor = FindMultiple(RoundedDisp))

WeatherVaning.Coord.DF <- data.DF %>%
        group_by(id) %>%
        filter(Divisor==0)


WeatherVaning.Coord.DF <- WeatherVaning.Coord.DF %>%
        group_by(id) %>%
        mutate(testy = lag(RoundedDisp)-RoundedDisp) %>%
        filter(testy==-500)


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
save(data1, file = ".//completedata.rda") 


data.DF <- left_join(data.DF,data1, by = c("id","time"))

save(data.DF, file = ".//datadf.rda")



# data.DF <- data.DF %>%
#         filter(id == "N2.10.234")

test.worm2 <- data.DF %>%
        filter(steeringangle >0)

# good <- ggplot(test.worm2, aes(x = lag(x,2),y = lag(y,2)))+
#         geom_path(aes(color = (steeringangle),size =1 )) +
#         coord_fixed(ratio=1) +
#         geom_path(data = data.DF, aes(x=(smoothx),y=(smoothy)))
# good+ scale_color_gradient(low="yellow", high="red", limits = c(0,180)) 
# 
# ggsave("test.pdf")

###Run Length Analysis
###
rm(list=ls())


load(".//datadf.rda")
#data.DF$obs <- 1:nrow(data.DF)
#data.DF <- data.DF %>% group_by(id) %>% filter(max(obs) != obs)


colnames(data.DF)[1] <- "time"
data.DF <- data.DF %>%
        select(c(time, id,x,y,backward.cor,Assay.ID.x,xref,yref,genotype.x, 
                 displacement,cumdisp,smoothx,smoothy,
                 bearing.angle, bearing.angle.smooth,
                 speed,distance.target,steeringangle))


data.DF <- data.DF %>%
        dplyr::rename("genotype" = "genotype.x",
                      "Assay.ID" = "Assay.ID.x")

data.DF <- data.DF %>%
        mutate(genotype = sub("[.].*", "", Assay.ID))



data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(bearing.abs = abs(bearing.angle.smooth),
               thresholdunder = case_when(bearing.abs < 60 ~1, bearing.abs > 60 ~0))

id <- data.DF %>%
        select(2)
test <- data.DF %>%
        dplyr::group_by(id,) %>% #advance value every time Signal changes and group by that
        dplyr::mutate("rl" = rleid(thresholdunder)) 
test <- test %>%
        dplyr::group_by(id,rl) %>% #advance value every time Signal changes and group by that
        dplyr::mutate(cum.sum = sum(replace_na(displacement, 0))) %>% #cumsum in each group
        ungroup() %>% #ungroup so you could remove the grouping column
        select(-2)

data.DF <- cbind(id,test)
# data.DF <- data.DF %>%
#         filter(id == "N2.10.234")


data.DF <- data.DF %>%
        group_by(id) %>%
        mutate(indexunder = thresholdunder-lag(thresholdunder),
               Run.ID = paste(id,rl, sep = ".Run."))

data.DF.under <-data.DF %>% 
        filter(thresholdunder == 1)


testy <- data.DF %>%
        group_by(id, grp = with(rle(thresholdunder), rep(seq_along(lengths), lengths))) %>%
        mutate(Counter = seq_along(grp)) %>%
        ungroup() %>%
        select(-grp)
Runs.Under.DF <- testy %>%
        filter(thresholdunder ==1)


Run.Durations.Under.DF <- Runs.Under.DF %>%
        group_by(id,rl) %>%
        dplyr::summarise(run.length.under = max(Counter)) %>%
        mutate(Run.ID.Under = paste(id,rl, sep = ".Run.")) %>%
        select(-rl)
# 
# Run.Durations.Under.DF<- Run.Durations.Under.DF %>%
#         group_by(id) %>%
#         mutate(test = seq(1,nrow(Run.Durations.Under.DF),1))

Runs.Over.DF <- testy %>%
        filter(thresholdunder == 0)


Run.Durations.Over.DF <- Runs.Over.DF %>%
        group_by(id,rl) %>%
        dplyr::summarise(run.length.over = max(Counter)) %>%
        mutate(Run.ID.Over = paste(id,rl, sep = ".Run.")) %>%
        select(-rl)

# Run.Durations.Over.DF<- Run.Durations.Over.DF %>%
#         group_by(id) %>%
#         mutate(test = seq(1,nrow(Run.Durations.Over.DF),1))
# 
# Run.Durations.DF <- left_join(Run.Durations.Over.DF,Run.Durations.Under.DF, by = "test") %>%
#         select(-test)


Run.Durations.Under.DF <- Run.Durations.Under.DF %>%
        filter(run.length.under >1)

data.DF.under <- data.DF.under %>%
        filter(Run.ID %in% Run.Durations.Under.DF$Run.ID.Under)

data.DF.over <-data.DF %>% 
        filter(thresholdunder == 0)

Run.Durations.Over.DF <- Run.Durations.Over.DF %>%
        filter(run.length.over >1)

data.DF.over <- data.DF.over %>%
        filter(Run.ID %in% Run.Durations.Over.DF$Run.ID.Over)


# good <- ggplot(data.DF, aes(x = smoothx,y = smoothy))+
#         geom_path() +
#         coord_fixed(ratio=1) +
#         geom_point(data = data.DF.under, aes(x=(smoothx),y=(smoothy)), color = "green", size =0.01) +
#         geom_point(data = data.DF.over, aes(x=(smoothx),y=(smoothy)), color = "red", size =0.01) +
#         geom_point(aes(x = xref, y = yref)) +
#         theme_classic()
# good+ scale_color_gradient(low="yellow", high="red", limits = c(0,180)) 

data.DF.under <- data.DF.under %>%
        group_by(id) %>%
        distinct(rl, .keep_all = T)
Runlength.Under.DF<- left_join(data.DF.under, Run.Durations.Under.DF, by = c("Run.ID" = "Run.ID.Under"))

data.DF.over <- data.DF.over %>%
        group_by(id) %>%
        distinct(rl, .keep_all = T)
Runlength.Over.DF<- left_join(data.DF.over, Run.Durations.Over.DF, by = c("Run.ID" = "Run.ID.Over"))
write.csv(file = "runlengthdataover.csv", Runlength.Over.DF)
write.csv(file = "runlengthdataunder.csv", Runlength.Under.DF)
data.DF.under.summary.DF  <- data.DF.under %>%
        group_by(id) %>%
        dplyr::summarise(totaldist.under = sum(cum.sum),
                         totalnum.runsunder = sum(abs(indexunder), na.rm = TRUE),
                         meandist.under = mean(cum.sum, na.rm = TRUE))
data.DF.over <-data.DF %>% 
        filter(thresholdunder == 0)

data.DF.over <- data.DF.over %>%
        group_by(id) %>%
        distinct(rl, .keep_all = T)

data.DF.over.summary.DF  <- data.DF.over %>%
        group_by(id) %>%
        dplyr::summarise(totaldist.over = sum(cum.sum),
                         totalnum.runsover = sum(abs(indexunder), na.rm = TRUE),
                         meandist.over = mean(cum.sum, na.rm = TRUE))
datasummary.Df <- left_join(data.DF.under.summary.DF,data.DF.over.summary.DF, by = "id")
datasummary.Df <- datasummary.Df %>%
        group_by(id) %>%
        mutate(totalnumruns = totalnum.runsover+totalnum.runsunder,
               totaldistance = totaldist.under+totaldist.over)
angle <- 60
paste("./under",angle, "rda", sep = ".")
save(datasummary.Df, file = paste("./runlength",angle, "rda", sep = "."))
save(data.DF, file = ".//dataafterrunlength.rda")
####
####
####
#load(".//datadf.rda")

# data.DF <- data.DF %>%
#         filter(id == "N2.10.234")
rm(list=ls())

load(".//dataafterrunlength.rda")
low <- 0
high <- 50
#data.DF <- data.complete.DF
#################################Determining angle thresholds for subsequent analysis
##############################################################
#################################
#################################
#x <- abs(data.frame(data.DF$bearing.angle.smooth))
data.DF <- data.DF %>%
        group_by(id, genotype) %>%
        mutate(RevCount = backward.cor-lag(backward.cor))

data.DF <- data.DF %>% 
        group_by(id,genotype) %>%
        mutate(Angle.Threshold = case_when(
                bearing.angle.smooth > high  ~ 0,
                bearing.angle.smooth < low  ~ 0,
                bearing.angle.smooth > 1  ~ 1))


data.DF <- data.DF %>%
        group_by(id, genotype) %>%
        mutate(RevCount = if_else(RevCount <= 0,0,1 )) 



numbering = function(v,k) {
        ## First, replacing stretches of less than k consecutive 1s by 0s
        r = rle(v);
        r$values[r$values==1 & r$lengths<=k] = 0; 
        v2 = inverse.rle(r); 
        
        
}

data.DF <- data.DF %>% 
        group_by(id,genotype) %>%
        mutate(Clean.Reversal = numbering(backward.cor, 8),
               RevCount = (lag(Clean.Reversal)-Clean.Reversal)*-1) ####What duration in frames of reversals do you cut out?

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
        group_by(id, genotype) %>%
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
                         meanSpeed = mean(speed, na.rm = T),
                         ReversalplusID = paste(id,Reversal.ID, sep = ".R."))
Before.Reversal.Summary.DF <- Before.Reversal.Summary.DF %>%
        filter(!Reversal.ID == "NA")

Before.Reversal.Summary.DF <- Before.Reversal.Summary.DF %>%
        filter(meanAngle <60)
Reversals.Location.DF <- data.DF %>%
        filter(RevCount == -1)

good <- ggplot(test.worm2, aes(x = lag(x,2),y = lag(y,2)))+
        #geom_path(aes(color = (steeringangle),size =1 )) +
        coord_fixed(ratio=1) +
        geom_path(data = data.DF, aes(x=(smoothx),y=(smoothy))) +
        geom_point(data = Reversals.Location.DF, aes(x = smoothx, y = smoothy), color = "red", shape = 1, size = 3) +
        geom_point(aes(x = xref, y = yref))
good+ scale_color_gradient(low="yellow", high="red", limits = c(0,180)) 
ggsave("confirmationofreversalangle.pdf")
ggplot(data= Before.Reversal.Summary.DF, aes(x= meanAngle, color = genotype)) +
        geom_density()

Before.Reversal.Summary.Genotype.DF <- Before.Reversal.Summary.DF %>%
        group_by(genotype) %>%
        dplyr::summarise(Angle =mean(meanAngle))
testy <- Total.Reversal.DF %>%
        group_by(id, grp = with(rle(backward.cor), rep(seq_along(lengths), lengths))) %>%
        mutate(Counter = seq_along(grp)) %>%
        ungroup() %>%
        select(-grp)
testy <- testy %>%
        filter(backward.cor ==1)
Reversal.length.DF <- testy %>%
        group_by(id, Reversal.ID) %>%
        dplyr::summarise(reversal.length = max(Counter)) %>%
        mutate(Reversal.ID =Reversal.ID -1,
               ReversalplusID = paste(id,Reversal.ID, sep = ".R."))

Reversal.Angle.Duration.DF <- left_join(Before.Reversal.Summary.DF,Reversal.length.DF)

Reversal.Angle.Duration.DF <- distinct(Reversal.Angle.Duration.DF)

write.csv(file = "reversalangleandduration.csv", Reversal.Angle.Duration.DF)
save(data.DF,file =  ".//datareversalsdone.rda")

