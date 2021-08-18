rm(list=ls())  
#setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
estagr<-read.csv("behavphy_all-checking.csv ")
summary (estagr)
str(estagr)
library (ggplot2)
## aggression
estagr$fe_est<-as.numeric(as.character(estagr$fe_est))
estagr$mal_agr<-as.numeric(as.character(estagr$mal_agr))
estagr$bointun<-as.numeric(as.character(estagr$bointun))
estagr$date=as.Date(estagr$date, format= "%d/%m/%y")
estagr$fe_estug<-estagr$fe_est/1000
estagr<-estagr[order(estagr$fe_est),]
str(estagr)
#agr1<-with(phybe[phybe$bointun!=0,],glm(b_agr~m_estug+I(m_estug^2),family="poisson",na.action="na.exclude"))
agr1<-with(estagr[estagr$bointun!=0,],glm(fe_est~mal_agr+I(fe_estug^2),family="poisson",na.action="na.exclude"))

summary(agr1)
plot(agr1)

warnings()
ggplot(agr1, aes(x = fe_estug,
                y = mal_agr)) + geom_ribbon(aes(ymin = 0,
                ymax = 5, fill = id ),
                alpha = 0.2) + geom_line(aes(colour = id),
                size = 1)                                                              
                
predictagr<-predict(agr1,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr)

with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~fe_est,
                                    ylim=c(0,4),col="blue",cex=5,type="l",
                                     lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))

with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~mal_agr,
                                     ylim=c(0,4),col="blue",cex=5,type="l",
                                     lty=2, xlab="Estradiol ng/gram",ylab="Aggression",main="Aggression and Estradiol Relationship"))                                     
with(estagr[estagr$bointun!=0,],lines(mal_agr,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],lines(mal_agr,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],points(mal_agr,fe_est))
with(estagr[estagr$bointun!=0,],plot(mal_agr,fe_est),ylab="mal_agr",xlab="fe_est",pch=1,ylim=0,5)


#ggplot(estagr,aes(estagr$bointun),ylim=4 (x=fe_est, color= mal_agr+geom_density(),method=glm()))
ggplot(estagr, aes(x = fe_est, y = mal_agr)) +
         geom_smooth()methods = glm(formula = mal_agr ~ mal_est_estug + I(mal_estug^2), family = "poisson", na.action = "na.exclude") 
                                                                    

#with(estagr[estagr$bointun!=0,],xlab="Estradiol ng/gram",ylab="Agression")
#plot(y~x, main=" ", xlab=" ", ylab=" ")
#abline(lm, col-"blue", lwd=1)
#legend(x,y, c(" ", " "), title=" ", fill= "color", col="black")
#text(x,y, label=" ", col="black", cex=1)

#ggplot(estagr,aes(x=day,y=est,color=INDIV))+geom_line()
#est1<-ts.plot() 
#est1<-ggplot(ftseries,aes(x=day,y=est,color=INDIV))+geom_line()+facet_grid(.~INDIV)
#est1<-est1+ylim(500,3500)
#est1<-est1+ ggtitle("Individual Estradiol Levels")

 

agr1<-with(estagr[estagr$bointun!=0,],glm(mal_agr~fe_estug+I(fe_estug^2),family="poisson",na.action="na.exclude"))
summary(agr1)
#dev.off()
#jpeg("fig1.jpg",width=12.5, height=7.5, units="in",res=600)
line.plot<- ggplot(data=estagr, aes(y="Aggression",x="Estradiol ng/gram"))
line.plot
line.plot+
 geom_line()
dev.off()


