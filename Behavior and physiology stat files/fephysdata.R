
setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
fephy<-read.csv("femphys_for_R.csv")
library(ggplot2)
library(mixtools)
summary(fephy)
head(fephy)
fephy$est=as.numeric(fephy$est)
fephy$date=as.Date(fephy$date, format= "%m/%d/%y")
fephy$INDIV=as.factor((fephy$INDIV))
help(as.Date)
hist(fephy$est)
str(fephy)
str(sphys)
fephy$est=as.numeric(as.character(fephy$est))
fephy$pro=as.numeric(as.character(fephy$pro))
fephy$cort=as.numeric(as.character(fephy$cort))
fephy$date=as.Date(fephy$date,format="%d/%m/%y")
#as.Date(dates, "%m/%d/%y")

list(sphys$est)
str(fephy)

library(mixtools)
#wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55, 80), sigma = 5)

y1y2<-normalmixEM(fephy$s_est,lambda=0.5,mu=c(500,2000),sigma=500)

summary(y1y2)
plot(y1y2,density=T)

hist(fephy["yeara","est",sphys["yearb","est"]],draw = TRUE, ...)
#ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE,
#Below is code used to plot for two separate linear regressions models
#plot(CO2reg, density = TRUE, alpha = 0.01, cex.main = 1.5, cex.lab = 1.5,
+ cex.axis = 1.4)

prog1<-normalmixEM(fephy$pro[!is.na(fephy$pro)],lambda=0.5,mu=c(10,600),sigma=300)
summary(prog1)
plot(prog1,density=T)






prohist<-hist(fephy$pro)





dev.off()
jpeg("fig1.jpg",width=12.5, height=6.5, units="in",res=600)
line.plot<- ggplot(data=fephy, aes(x=date, y=est, col=INDIV))
  
line.plot+
  geom_line()
dev.off()
