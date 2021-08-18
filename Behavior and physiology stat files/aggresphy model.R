
#setwd("C:/Users/stuwe/OneDrive - University of Arizona/1-Doctoral chapters/Stats for dissertation/Behavior and physiology stat files")
agrphy <- read.csv("behavwest71921.csv")
class(agrphy$Date)
library(ggplot2)
library(mixtools)
summary(agrphy)
head(agrphy)
#agrphy$Date<-strptime(agrphy$Date,format="%d-%b-%y")             
agrphy$Est=as.numeric(agrphy$Est)
lEst <- log(agrphy$Est)
agrphy$lEst=as.numeric(agrphy$lEst)
lpro <- log(agrphy$pro)
agrphy$pro=as.numeric(agrphy$pro)
agrphy$lpro=as.numeric(agrphy$lpro)
agrphy$lmagg=as.numeric(agrphy$lmagg)
agrphy$maleAgg=as.numeric(agrphy$maleAgg)
agrphy$in.tun=as.numeric(agrphy$in.tun)
as.character(as.factor(agrphy$Date))
agrphy$date=as.Date(agrphy$Date, format= "%d/%b/%y")
agrphy$ID=as.factor(agrphy$ID)
agrphy$Est=as.numeric(as.character(agrphy$Est))
agrphy$pro=as.numeric(as.character(agrphy$pro))
agrphy$cort=as.numeric(as.character(agrphy$cort))
help(as.Date)
hist(agrphy$Est)

#wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55, 80), sigma = 5)

Est1<-normalmixEM(agrphy$Est[!is.na(agrphy$Est)],lambda=0.5,mu=c(1000,2000),sigma=500)
#Est1<-normalmixEM(agrphy$Est[!is.na(agrphy$Est)],lambda=0.5,mu=c(1000,2000),sigma=500)
summary(Est1)
plot(Est1,density=T)






#draw = TRUE, ...)
#ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE,


prog1<-normalmixEM(agrphy$pro[!is.na(agrphy$pro)],lambda=0.5,mu=c(1000,2500),sigma=500)
summary(prog1)
plot(prog1,density=T)



Esthist<-hist(agrphy$Est)




summary(lm(agrphy$pro~agrphy$Est))

## fit yEsterdays progEsterone todays Estradiol
summary(lm(agrphy$pro[-1]~agrphy$pro[-length(agrphy$Est)]))

## fit todays progEsterone on yEsterdays Estradiol
summary(lm(agrphy$pro[-length(agrphy$pro)]~agrphy$Est[-1]))

plot(agrphy$pro~agrphy$Est)
abline(lm(agrphy$pro~agrphy$Est))

## agression
phybe$m_Est<-as.numeric(as.character(phybe$m_Est))
phybe$b_agr<-as.numeric(as.character(phybe$b_agr))
phybe$in.tun<-as.numeric(as.character(phybe$in.tun))
phybe$m_Estug<-phybe$m_Est/1000
phybe<-phybe[order(phybe$m_Est),]

agr1<-with(phybe[phybe$in.tun!=0,],glm(b_agr~m_Estug+I(m_Estug^2),family="poisson",na.action="na.exclude"))
summary(agr1)


predictagr<-predict(agr1,se.fit=T)

#changed in.tun to in.tun to match data
with(phybe[phybe$in.tun!=0,],plot(exp(predictagr$fit)~m_Est,ylim=c(0,4),type="l",lty=2))
with(phybe[phybe$in.tun!=0,],lines(m_Est,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(phybe[phybe$in.tun!=0,],lines(m_Est,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(phybe[phybe$in.tun!=0,],points(m_Est,m_agr))


dev.off()
jpeg("fig1.jpg",width=12.5, height=6.5, units="in",res=600)
line.plot<- ggplot(data=agrphy, aes(x=date, y=Est, col=INDIV))

line.plot+
  geom_line()
dev.off()


library(zoo)
