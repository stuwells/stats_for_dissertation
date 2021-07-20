rm(list=ls())  
setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
estagr<-read.csv("behavphy_all.csv")
summary (estagr)
library (ggplot2)
## agression
estagr$fe_est<-as.numeric(as.character(estagr$fe_est))
estagr$mal_agr<-as.numeric(as.character(estagr$mal_agr))
estagr$bointun<-as.numeric(as.character(estagr$bointun))


estagr$fe_estug<-estagr$fe_est/1000
estagr<-estagr[order(estagr$fe_est),]
#agr1<-with(phybe[phybe$bointun!=0,],glm(b_agr~m_estug+I(m_estug^2),family="poisson",na.action="na.exclude"))
agr1<-with(estagr[estagr$bointun!=0,],glm(mal_agr~fe_estug+I(fe_estug^2),family="poisson",na.action="na.exclude"))
summary(agr1)
warnings()

predictagr<-predict(agr1,se.fit=T)

summary (predictagr)

with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~fe_est,
                                     ylim=c(0,4),col="blue",cex=5,type="l",
                                     lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))
                                     
with(estagr[estagr$bointun!=0,],lines(fe_est,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],lines(fe_est,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],points(mal_agr,fe_est))
with(estagr[estagr$bointun!=0,],xlab="Estradiol ng/gram",ylab="Agression")
plot(y~x, main=" ", xlab=" ", ylab=" ")
abline(lm, col-"blue", lwd=1)
legend(x,y, c(" ", " "), title=" ", fill= "color", col="black")
text(x,y, label=" ", col="black", cex=1)






#glm(formula = mal_agr ~ mal_estug + I(mal_estug^2), family = "poisson", 
    na.action = "na.exclude")
dev.off()
jpeg("fig1.jpg",width=12.5, height=7.5, units="in",res=600)
line.plot<- ggplot(data=estagr, aes(y="Agression",x="Estradiol ng/gram"))

line.plot+
  geom_line()
dev.off()
