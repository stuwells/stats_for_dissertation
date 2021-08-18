
setwd("C:/Users/stuwe/OneDrive - University of Arizona/1-Doctoral chapters/Stats for dissertation/Behavior and physiology stat files")
fephy<-read.csv("femphys_for_R.csv")
library(ggplot2)
summary(fephy)
head(fephy)
fephy$est=as.numeric(as.character(fephy$est))
fephy$pro=as.numeric(as.character(fephy$pro))
fephy$date=as.Date(fephy$date, format= "%d/%m/%y")
fephy$INDIV=as.factor((fephy$INDIV))
help(as.Date)
hist(fephy$pro)
str(fephy)

library(mixtools)
#wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55, 80), sigma = 5)

est1<-normalmixEM(fephy$est[!is.na(fephy$est)],lambda=0.5,mu=c(1000,2000),sigma=500)
summary(est1)
plot(est1,density=T)



#draw = TRUE, ...)
#ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE,
        

prog1<-normalmixEM(fephy$pro[!is.na(fephy$pro)],lambda=0.5,mu=c(1000,2500),sigma=500)
summary(prog1)
plot(prog1,density=T)



esthist<-hist(fephy$est)




summary(lm(fephy$pro~fephy$est))

## fit yesterdays progesterone todays estradiol
summary(lm(fephy$pro[-1]~fephy$pro[-length(fephy$est)]))

## fit todays progesterone on yesterdays estradiol
summary(lm(agrphy$pro[-length(agrphy$pro)]~agrphy$Est[-1]))

plot(agrphy$pro~agrphy$Est)
abline(lm(agrphy$pro~agrphy$Est))

## agression
agrphy$lmagg<-as.numeric(as.character(agrphy$lmagg))
agrphy$Est<-as.numeric(as.character(agrphy$Est))
agrphy$in.tun<-as.numeric(as.character(agrphy$in.tun))
agrphy$Est<-agrphy$Est/1000
agrphy<-Est[order(Est$Est),]

agr2<-with(Baest[Baest$in.tun!=0,],glm(lmagg~Baest+I(Baest^2),family="poisson",na.action="na.exclude"))
summary(agr2)


predictagr<-predict(agr2,se.fit=T)


with(phybe[phybe$in.tun!=0,],plot(exp(predictagr$fit)~Est,ylim=c(0,4),type="l",lty=2))
with(phybe[phybe$in.tun!=0,],lines(Est,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(phybe[phybe$in.tun!=0,],lines(Est,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(phybe[phybe$in.tun!=0,],points(Est,lmagg))


dev.off()
jpeg("fig1.jpg",width=12.5, height=6.5, units="in",res=600)
line.plot<- ggplot(data=agrphy, aes(x=date, y=Est, col=ID))
  
line.plot+
  geom_line()
dev.off()
