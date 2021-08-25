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
