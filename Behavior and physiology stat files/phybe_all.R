rm(list=ls())  
#setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")
estagr<-read.csv("behavphy_all.csv")
summary (estagr)
library (ggplot2)
## agression
estagr$fe_est<-as.numeric(as.character(estagr$fe_est))
estagr$mal_agr<-as.numeric(as.character(estagr$mal_agr))
estagr$bointun<-as.numeric(as.character(estagr$bointun))

estagr$mal_estug<-estagr$fe_est/1000
estagr<-estagr[order(estagr$fe_est),]

phybe$m_Est<-as.numeric(as.character(phybe$m_Est))
phybe$b_agr<-as.numeric(as.character(phybe$b_agr))
phybe$in.tun<-as.numeric(as.character(phybe$in.tun))
phybe$m_Estug<-phybe$m_Est/1000
phybe<-phybe[order(phybe$m_Est),]

agr1<-with(phybe[phybe$in.tun!=0,],glm(b_agr~m_Estug+I(m_Estug^2),family="poisson",na.action="na.exclude"))
summary(agr1)


predictagr<-predict(agr1,se.fit=T)





with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~fe_est,ylim=c(0,4),type="l",lty=2))
with(estagr[estagr$bointun!=0,],lines(fe_est,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],lines(fe_est,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(estagr[estagr$bointun!=0,],points(mal_agr,fe_est))
glm(formula = estagr$mal_agr ~ estagr$mal_estug + I(estagr$mal_estug^2), family = "poisson", 
    na.action = "na.exclude")
dev.off()
jpeg("fig1.jpg",width=12.5, height=6.5, units="in",res=600)
line.plot<- ggplot(data=estagr, aes(y=mal_agr,x=fe_est))

line.plot+
 geom_line()
#dev.off()
##code from brett
#ggplot(estagr, aes(estagr$fe_est, estagr$mal_agr)) +
#  geom_smooth() + geom_point(alpha=0.5)


 
