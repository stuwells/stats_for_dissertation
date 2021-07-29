#setwd(C:\Users\stuwe\OneDrive - University of Arizona\1-Doctoral chapters\Stats for dissertation\Physiology only stat files
rm(list=ls())
femcort<-read.csv("female fremonti-phys-combined.csv")
library(ggplot2)
library(date)
summary(femcort)
#head(fephy)
head(femcort)
femcort$cort=as.numeric(femcort$cort)
#femcort$tes=as.numeric(femcort$tes)
femcort$dat=as.Datefemcort$dat, format= "%d/%m/%y")

femcort$id=as.factor((femcort$id))
help(as.Date)
#hist(femcort$tes)
hist(femcort$cort)
str(femcort)
line.plot+
  
  line.plot<- ggplot(data=femcort, aes(x=date, y=tes, col=id))
