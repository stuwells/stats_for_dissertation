##Below is code to examine periodicy in estradiol and progesterone
rm(list=ls())  
setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
fephyts<-read.csv("femphysforRa.csv")
library(ggplot2)
library(mixtools)
library(xts)
summary(fephyts)
head(fephyts)
fephyts$est=as.numeric(fephyts$est)
fephyts$date=as.Date(fephy$date, format= "%m/%d/%y")
fephyts$INDIV=as.factor((fephyts$INDIV))
help(as.Date)
hist(fephyts$est)
str(fephyts)
str(sphys)
fephyts$est=as.numeric(as.character(fephyts$est))
fephyts$pro=as.numeric(as.character(fephyts$pro))
fephyts$cort=as.numeric(as.character(fephyts$cort))
fephyts$date=as.Date(fephyts$date,format="%d/%m/%y")
#as.Date(dates, "%m/%d/%y")
periodicity(fephyts$est)
list(sphys$est)
str(fephyts)

library(mixtools)
#wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55, 80), sigma = 5)

y1y2<-normalmixEM(fephyts$s_est,lambda=0.5,mu=c(500,2000),sigma=500)

summary(y1y2)
plot(y1y2,density=T)

hist(fephyts["yeara","est",sphys["yearb","est"]],draw = TRUE, ...)
#ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE,
#Below is code used to plot for two separate linear regressions models
#plot(CO2reg, density = TRUE, alpha = 0.01, cex.main = 1.5, cex.lab = 1.5,
#+ cex.axis = 1.4)

prog1<-normalmixEM(fephyts$pro[!is.na(fephyts$pro)],lambda=0.5,mu=c(10,600),sigma=300)
summary(prog1)
plot(prog1,density=T)




prohist<-hist(fephyts$pro)





dev.off()
jpeg("fig1.jpg",width=12.5, height=6.5, units="in",res=600)
line.plot<- ggplot(data=fephyts, aes(x=date, y=est, col=INDIV))
  
line.plot+
  geom_line()
dev.off()

