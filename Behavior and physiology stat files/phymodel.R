rm(list=ls()) 
setwd("C:/Users/stuwe/OneDrive - University of Arizona/mgrsdatafor r/mydatamgrs")

phymod     
names(phymod)             
#mymodel<-with(mydata,lm(snow_depth~water_content,na.action="na.exclude"))

Phy$b_agrl<-Phy$b_agr>4
Phy$m_estl<-Phy$m_est>1000

myphy<-with(Phy,glm(b_agrl~m_estl))
summary(myphy)

plogis(-1.9095+0.6858)

plogis(-1.9095)
hist(Phy$m_est)

myphy$coefficients
getwd

