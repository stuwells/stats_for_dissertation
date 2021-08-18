
##aggression
rm(list=ls())
options(warn=-1)
#estagr<-read.csv("behavphy_all.csv")
summary (estagr)
library (ggplot2)
## agression
##estagr$fe_est<-as.numeric(as.character(estagr$fe_est))
##estagr$mal_agr<-as.numeric(as.character(estagr$mal_agr))
##estagr$bointun<-as.numeric(as.character(estagr$bointun))


##estagr$fe_estug<-estagr$fe_est/1000
##estagr<-estagr[order(estagr$fe_est),]
#agr1<-with(phybe[phybe$bointun!=0,],glm(b_agr~m_estug+I(m_estug^2),family="poisson",na.action="na.exclude"))
##agr1<-with(estagr[estagr$bointun!=0,],glm(mal_agr~fe_estug+I(fe_estug^2),family="poisson",na.action="na.exclude"))
##summary(agr1)
##warnings()

##predictagr<-predict(agr1,se.fit=T)

##summary (predictagr)

##with(estagr[estagr$bointun!=0,],plot(exp(predictagr$fit)~fe_est,
##                                     ylim=c(0,4),col="blue",cex=5,type="l",
##                                     lty=2, xlab="Estradiol ng/gram",ylab="Aggression"))

##with(estagr[estagr$bointun!=0,],lines(fe_est,exp(predictagr$fit-1.96*predictagr$se.fit)))
##with(estagr[estagr$bointun!=0,],lines(fe_est,exp(predictagr$fit+1.96*predictagr$se.fit)))
##with(estagr[estagr$bointun!=0,],points(mal_agr,fe_est))
##with(estagr[estagr$bointun!=0,],xlab="Estradiol ng/gram",ylab="Agression")
##plot(y~x, main=" ", xlab=" ", ylab=" ")
##abline(lm, col-"blue", lwd=1)
##legend(x,y, c(" ", " "), title=" ", fill= "color", col="black")
##text(x,y, label=" ", col="black", cex=1)






#glm(formula = mal_agr ~ mal_estug + I(mal_estug^2), family = "poisson", 
##na.action = "na.exclude")
##dev.off()
##jpeg("fig1.jpg",width=12.5, height=7.5, units="in",res=600)
##line.plot<- ggplot(data=estagr, aes(y="Agression",x="Estradiol ng/gram"))
library(ggplot2)
#question-What is the code below doing?
##commenting out to change read file
#phybe<-read.table(file.choose(), header=T, sep=",")phybe<-read.csv("behavphy_all.csv")
#phybe$Est<-as.numeric(as.character(phybe$Est))
#phybe$maleAgg<-as.numeric(as.character(phybe$maleAgg))
#phybe$intun<-as.numeric(as.character(phybe$intun))
##commenting out because the code below is not accurate
#phybe$maleAggEstug<-phybe$Est/1000
#phybe<-phybe[order(phybe$Est),]
##agr1<-with(phybe[phybe$bointun!=0,],glm(b_agr~m_estug+I(m_estug^2)
##glm(formula = mal_agr ~ mal_estug + I(mal_estug^2), family = "poisson", 
##+     na.action = "na.exclude")
phybe<-read.csv("behavwest-transformed-6-19-bh.csv")
summary(phybe)
##subset data to those cases where both animals are in tunnel
phybe_intun<-phybe[phybe$intun != 0,]
##plot maleagg vs estradiol
plot(x=phybe_intun$Est, y=phybe_intun$M_Agg_Intense)
# agr1<-with(phybe[phybe$intun!=0,],
#            glm(maleAgg~Est + I(Est^2),
#                family="poisson",
#                na.action="na.exclude"))

agr1<-glm(M_Agg_Intense~Est + I(Est^2),
          family="poisson",
          na.action="na.exclude",
          data=phybe_intun)
#glm(formula = maleAgg ~ phybeEstug + I(phybeEstug^2),family="poisson",
#    na.action="na.exclude")
summary(agr1)
length(phybe$Est)
length(phybe$M_Agg_Intense)
length(phybe$Est^2)



predictagr<-predict(agr1,se.fit=T)


with(phybe[phybe$intun!=0,],plot(exp(predictagr$fit)~Est,ylim=c(0,4),type="l",lty=2))
with(phybe[phybe$intun!=0,],lines(Est,exp(predictagr$fit-1.96*predictagr$se.fit)))
with(phybe[phybe$intun!=0,],lines(Est,exp(predictagr$fit+1.96*predictagr$se.fit)))
with(phybe[phybe$intun!=0,],points(Est,M_Agg_Intense))


dev.off()
jpeg("fig1.jpg",width =12.5, height = 6.5, units="in",res = 600)
line.plot <- ggplot(data = phybe, aes( x = Est , y = M_Agg_Intense,
                                     col =id))

line.plot+
  geom_line()
dev.off()

