rm(list=ls()) 
estagr<-read.csv("behavphy_all-checking.csv ")
summary (estagr)
str(estagr)
library (ggplot2)
# aggression
estagr$fe_est<-as.numeric(as.character(estagr$fe_est))
estagr$mal_agr<-as.numeric(as.character(estagr$mal_agr))
estagr$mal_tun<-as.numeric(as.character(estagr$mal_tun))
estagr$date=as.Date(estagr$date, format= "%d/%m/%y")
estagr$fe_estug<-estagr$fe_est/1000
estagr<-estagr[order(estagr$fe_est),]
str(estagr)

warnings()
ggplot(data = estagr, mapping = aes(x = fe_estug, y = mal_agr)) +
  geom_ribbon(aes(ymin = 0, ymax = 5, fill = id ), alpha = 0.2) +
  geom_line(mapping = aes(colour = id), size = 1)


agr1<-with(estagr,glm(fe_est~mal_agr+I(fe_estug^2),family="poisson",na.action="na.exclude"))
agr1a<-with(estagr,glm(fe_est~mal_agr),family="poisson",na.action="na.exclude")

summary(agr1)
plot(agr1)
summary(agr1a)
plot(agr1a)




predictagr1<-predict(agr1,se.fit=T)
predictagr1a<-predict(agr1,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr1)
with(estagr,plot(exp(predictagr1$fit)~fe_est,
                 ylim=c(0,4),col="blue",cex=5,type="l",
                 lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))

with(estagr,plot(exp(predictagr1a$fit)~fe_estug,
                 ylim=c(0,4),col="blue",cex=5,type="l",
                 lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))

with(estagr,lines(fe_est,exp(predictagr1$fit-1.96*predictagr1$se.fit)))
with(estagr,lines(fe_estug,exp(predictagr1a$fit+1.96*predictagr1a$se.fit)))
with(estagr,lines(fe_est,exp(predictagr2$fit-1.96*predictagr2$se.fit)))
with(estagr,lines(fe_estug,exp(predictagr2a$fit+1.96*predictagr2a$se.fit)))
with(estagr,points(fe_estug,mal_agr))
with(estagr,plot(fe_estug,mal_agr),ylab="mal_agr",xlab="fe_est",pch=1,ylim=0,5)

predictagr2<-predict(agr1a,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr2)

predictagr2a<-predict(agr1a,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr2a)

with(estagr,plot(exp(predictagr2$fit)~fe_est,
                 ylim=c(0,4),col="blue",cex=5,type="l",
                 lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))

with(estagr,plot(exp(predictagr2$fit)~fe_estug,
                 ylim=c(0,4),col="blue",cex=5,type="l",
                 lty=2, xlab="Estradiol ng/gram",ylab="Aggression",main="Aggression and Estradiol Relationship"))                                     


with(estagr,plot(exp(predictagr2a$fit)~fe_est,
                 ylim=c(0,4),col="blue",cex=5,type="l",
                 lty=2, xlab="Estradiol ng/gram",ylab="Aggression",main="Aggression and Estradiol Relationship"))  

with(estagr,plot(exp(predictagr2a$fit)~fe_estug,
                 ylim=c(0,4),col="blue",cex=5,type="l",
                 lty=2, xlab="Estradiol ng/gram",ylab="Aggression",main="Aggression and Estradiol Relationship"))  



agr2 <- with(estagr,glm(mal_agr~fe_estug+I(fe_estug^2),family="poisson",na.action ="na.exclude"))
agr2a <- with(estagr,glm(mal_agr~fe_estug,family="poisson",na.action ="na.exclude"))

predictagr2<-predict(agr2,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr2)

summary(agr2)
plot(agr2)

#testing the better of two models
agr2a <- with(estagr,glm(mal_agr~fe_estug,family="poisson",na.action ="na.exclude"))

agr3a <- with(estagr,glm(mal_agr ~ fe_estug + I(fe_estug^2),
                         family="poisson",
                         na.action ="na.exclude"))
#anova to compare the better of the two models
anova(agr3a, agr2a, test = "Chisq")

predictagr2a<-predict(agr2a,se.fit=T)
#ggtitle("Aggression Estradiol Relationship")
summary (predictagr2a)

summary(agr2a)
plot(agr2a)

#for specific female - Bob vs Merrill
agr_fb <- with(estagr[estagr$id == "bm", ],
               glm(mal_agr ~ fe_estug,
                   family = "poisson",
                   na.action = "na.exclude"))
#I want to include the squared value of estradiol in the model:
agr_fb2 <- with(estagr[estagr$id == "bm", ],
               glm(mal_agr ~ fe_estug + I(fe_estug^2),
                   family = "poisson",
                   na.action = "na.exclude"))
summary(agr_fb)
plot(agr_fb)

summary(agr_fb2)
plot(agr_fb2)

#comparing the second female with male aggression Al vs Spruce

agr_fba <- with(estagr[estagr$id == "as", ],
               glm(mal_agr ~ fe_estug,
                   family = "poisson",
                   na.action = "na.exclude"))

#iwant to include the squared value of estradiol in the model:
agr_fba2 <- with(estagr[estagr$id == "as", ],
               glm(mal_agr ~ fe_estug + I(fe_estug^2),
                   family = "poisson",
                   na.action = "na.exclude"))

summary(agr_fba)
plot(agr_fba)

summary(agr_fba2)
plot(agr_fba2)


#jpeg("fig1.jpg",width = 12.5, height = 7.5, units="in",res = 600)
line.plot<- ggplot(data=estagr, aes(y="mal_agr",x ="fe_est"))
line.plot
line.plot+
geom_point()
line.plot
#dev.off()
ggsave(plot=line.plot,filename = "fig1.jpg",width = 12.5, height = 7.5, units="in",dpi = 600)
