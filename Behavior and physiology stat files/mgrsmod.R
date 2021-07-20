getwd()
mgrs<-read.csv("mgrsdata.csv")
#jansnow<-snow$scdepth[snow$scmonth==1&snow$courseid=="09n04.txt"]
summary(mgrs$m_est)
summary(mgrs$s_est)
summary(mgrs$b_agr)
