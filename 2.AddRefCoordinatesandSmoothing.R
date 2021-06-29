rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(data.table)
setwd("~/Desktop/R Code to Review/glr-3 Project Files/Raw Data for Figures/Figure 1/Figure 1 Spreadsheets Data/ICE Diacetyl copy")
load('./Realwormsforanalysis.rda')


data.DF <- real.worms.data.DF

coordinates <- read.csv("CompleteIndex.csv", header = T)
coordinates <- coordinates %>%
        dplyr::rename(Assay.ID= id)

data.DF <- data.DF %>%
        select(c(time,x,y,id,genotype,backward,Assay.ID))

data.DF <- data.DF %>%
        left_join(coordinates, by = "Assay.ID")

data.DF <- data.DF %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(smoothx = pracma::movavg(x, 45, "s")) %>%
        dplyr::mutate(smoothy = pracma::movavg(y, 45, "s"))
save(data.DF, file = ".//datasmoothandfull.rda")


