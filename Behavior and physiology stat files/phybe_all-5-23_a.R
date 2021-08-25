rm(list=ls())  
#setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
estagr<-read.csv("behavphy_all-checking.csv ")
summary (estagr)
str(estagr)
library (ggplot2)
## aggression
estagr$fe_est<-as.numeric(as.character(estagr$fe_est))
estagr$mal_agr<-as.numeric(as.character(estagr$mal_agr))
estagr$mal_tun<-as.numeric(as.character(estagr$mal_tun))
estagr$date=as.Date(estagr$date, format= "%d/%m/%y")
estagr$fe_estug<-estagr$fe_est/1000
#estagr$fe_estug1<-estagr$fe_est/100
estagr<-estagr[order(estagr$fe_est),]
str(estagr)

warnings()
ggplot(data = estagr, mapping = aes(x = fe_estug, y = mal_agr)) +
  geom_ribbon(aes(ymin = 0, ymax = 5, fill = id ), alpha = 0.2) +
  geom_line(mapping = aes(colour = id), size = 1)

#ggplot(agr1, aes(x = fe_estug,
#                y = mal_agr)) + geom_ribbon(aes(ymin = 0,
#                alpha = 0.2) + geom_line(aes(colour = id),
#                size = 1)                                                              
                
agr1<-with(estagr[estagr$bointun!=0,],glm(mal_agr~fe_estug),family="poisson",na.action="na.exclude"))

predictagr<-predict(agr1,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr)

with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~fe_est,
                                    ylim=c(0,4),col="blue",cex=5,type="l",
                                     lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))

with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~fe_estug,
                                     ylim=c(0,4),col="blue",cex=5,type="l",
                                     lty=2, xlab="Estradiol ng/gram",ylab="Aggression",main="Aggression and Estradiol Relationship"))                                     
with(estagr[estagr$bointun!=0,],lines(fe_estug,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],lines(fe_estug,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],points(fe_estug,mal_agr))
with(estagr[estagr$bointun!=0,],plot(fe_estug,mal_agr),ylab="mal_agr",xlab="fe_est",pch=1,ylim=0,5)


#ggplot(estagr,aes(estagr$bointun),ylim=4 (x=fe_est, color= mal_agr+geom_density(),method=glm()))
ggplot(estagr, aes(x = fe_estug, y = mal_agr)) +
         geom_smooth(method="glm",formula="y~x+I(x^2)",
                     method.args=list(family = "poisson", na.action = "na.exclude")) 
                                                                    

#with(estagr[estagr$bointun!=0,],xlab="Estradiol ng/gram",ylab="Aggression")
#plot(y~x, main=" ", xlab=" ", ylab=" ")
#abline(lm, col-"blue", lwd=1)
#legend(x,y, c(" ", " "), title=" ", fill= "color", col="black")
#text(x,y, label=" ", col="black", cex=1)

#ggplot(estagr,aes(x=day,y=est,color=INDIV))+geom_line()
#est1<-ts.plot() 
#est1<-ggplot(ftseries,aes(x=day,y=est,color=INDIV))+geom_line()+facet_grid(.~INDIV)
#est1<-est1+ylim(500,3500)
#est1<-est1+ ggtitle("Individual Estradiol Levels")

##below contains code where mal_agr and fe_estug are reversed

#agr1 <- with(estagr[estagr$bointun!= 0,],glm(mal_agr~fe_estug+I(fe_estug^2),family="quasipoisson",na.action ="na.exclude"))
summary(agr1)
dev.off()

#agr1<-with(phybe[phybe$bointun!=0,],glm(b_agr~m_estug+I(m_estug^2),family="poisson",na.action="na.exclude"))
agr1<-with(estagr,glm(fe_est~mal_agr+I(fe_estug^2),family="poisson",na.action="na.exclude"))
agr1a<-with(estagr,glm(fe_est~mal_agr),family="poisson",na.action="na.exclude")

summary(agr1)
plot(agr1)
summary(agr1a)
plot(agr1a)

agr2 <- with(estagr,glm(mal_agr~fe_estug+I(fe_estug^2),family="poisson",na.action ="na.exclude"))
agr2a <- with(estagr,glm(mal_agr~fe_estug,family="poisson",na.action ="na.exclude"))
summary(agr2)
plot(agr2)
summary(agr2a)
plot(agr2a)

#jpeg("fig1.jpg",width = 12.5, height = 7.5, units="in",res = 600)
line.plot<- ggplot(data=estagr, aes(y="mal_agr",x ="fe_est"))
line.plot
line.plot+
geom_point()
line.plot
#dev.off()
ggsave(plot=line.plot,filename = "fig1.jpg",width = 12.5, height = 7.5, units="in",dpi = 600)

