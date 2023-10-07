#stuwells
#IAT data analysis
#9-20-21



rm(list=ls())

iatdata<- read.csv("d score breakdown.csv ", header = TRUE)

head(x=iatdata)


library(dplyr)
library(ggplot2)
library(ggdendro)
library (extrafont)
library(tidyverse)
#plotdzoo5 with zoo5
plot(formula=wild1~dwild1,data=iatdata)
 

