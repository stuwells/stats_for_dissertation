rm(list=ls()) 
setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
sphys<-read.csv("sphys-ra.csv")
str(sphys)
sphys$est=as.numeric(as.character(sphys$est))
sphys$pro=as.numeric(as.character(sphys$pro))
sphys$cort=as.numeric(as.character(sphys$cort))
sphys$date=as.Date(sphys$date,format="%d/%m/%y")
#as.Date(dates, "%m/%d/%y")
str(sphys)
list(sphys$est)
str(sphys)

library(mixtools)
#wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55, 80), sigma = 5)

y1y2<-normalmixEM(sphys$s_est,lambda=0.5,mu=c(500,2000),sigma=500)

summary(y1y2)
plot(y1y2,density=T)

hist(sphys["yeara","s_est",sphys["yearb","s_est"]],draw = TRUE, ...)
#ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE,
#Below is code used to plot for two separate linear regressions models
#plot(CO2reg, density = TRUE, alpha = 0.01, cex.main = 1.5, cex.lab = 1.5,
     + cex.axis = 1.4)

prog1<-normalmixEM(sphys$pro[!is.na(sphys$pro)],lambda=0.5,mu=c(100,500),sigma=1)
summary(prog1)
plot(prog1,density=T)



estpro<-hist(fephy$est)




summary(lm(fephy$pro~fephy$est))

## fit yesterdays progesterone todays estradiol
summary(lm(fephy$pro[-1]~fephy$pro[-length(fephy$est)]))

## fit todays progesterone on yesterdays estradiol
summary(lm(fephy$pro[-length(fephy$pro)]~fephy$est[-1]))

plot(fephy$pro~fephy$est)
abline(lm(fephy$pro~fephy$est))
