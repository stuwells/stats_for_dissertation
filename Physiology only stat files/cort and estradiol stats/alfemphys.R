rm(list=ls())
femcort<-read.csv("csv-female fremonti-phys-combined-2a.csv")
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
