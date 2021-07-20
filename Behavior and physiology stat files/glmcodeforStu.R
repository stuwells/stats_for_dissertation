### Dataset 1

# Counts of tortoises on 45 plots in Rincon and Tucson Mtns 2010 and 2011

# Variables: plot, tortoises, and percent cover of buffelgrass, janusia, prickly, rock, subshrubs, 
#            shrubs, trees, vine, ngrass, bareground

# Import data into a data frame, check structure and view first few observations

 tc <- read.csv("http://cals.arizona.edu/~steidl/TortoiseCounts2010-11.csv", row.names=1)
 str(tc)
 head(tc)



#distribution is poisson, or count data, therefore you use the log link function:

mcount <- glm(tortoises~buffelgrass+shrubs+rock,data=tc,family='poisson')
summary(mcount)
betas <- coef(mcount)[2:4]
#backtransform betas to get more interpretable values:
backtransformed <- exp(betas)
pvalues <- summary(mcount)$coef[2:4,4]
cbind(betas,backtransformed,pvalues)

#interpretation example:
  #for every 1-unit increase in rock cover, tortoise counts are expected to increase by 1.023 times 
  #for every 1-unit increase in rock cover, tortoise counts are expected to increase by 2.3% 


plot(tortoises~rock, data=tc, ylab="No. tortoises", xlab="Rock (%)", pch = 16)
plot(tortoises~buffelgrass, data=tc, ylab="No. tortoises", xlab="Buffelgrass (%)", pch = 16)
plot(tortoises~shrubs, data=tc, ylab="No. tortoises", xlab="Shrub (%)", pch = 16)


###### Dataset 2

# Tortoise surveys on 40 plots in Rincon and Tucson Mtns 2005 and 2006
# presAbs: 1 = tortoise observed, 0 = tortoise not observed

presAbs <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,1,1,1,0,0,0,0,1,1,0,0,1,0,0,0,1,0,0,0,1,0,1,0)

elev <- c(1014,884,974,882,899,855,1016,830,856,1106,1101,1036,946,932,899,937,1018,1013,938,944,
          735,852,841,700,861,880,1046,845,830,707,768,927,1046,904,925,733,915,767,1008,711)

slope <- c(23.93,11.57,15.09,6.44,3.46,1.99,16.8,2.59,3.81,16.08,22.43,23.38,7.02,11.22,3.2,3.79,
           11.25,12.55,3.55,16.01,4.93,8.59,7.64,2.41,14.01,18.06,20.19,3.22,17.53,7.46,2.61,12.16,
           23.22,7.41,4.1,1.82,15.74,4.84,17.76,8.41)

plantCover <- c(27,25,18,37,38,51,20,31,31,17,21,26,32,32,65,30,20,19,54,20,25,26,26,27,15,20,28,22,
                21,19,21,42,11,21,22,20,27,17,17,39)

#Response variable has binomial distribution,therefore we use the logit link function

mpres1 <- glm(presAbs~1,family='binomial')
summary(mpres1)
(logodds <- coef(mpres1))
(prob <- exp(logodds)/(1+exp(logodds)))
#check:
sum(presAbs)/length(presAbs)
#probability of a tortoise occupying any given area is about 37.5%

mpres2 <- glm(presAbs~elev+slope+plantCover,family='binomial')
summary(mpres2)
logodds <- coef(mpres2)[2:4]
odds <- exp(logodds)
pvalues <- summary(mpres2)$coef[2:4,4]
cbind(logodds,odds,pvalues)

#interpretation example:
  #for every 1-unit increase in slope, odds of tortoise presence are expected to increase by 1.24 times 
  #for every 1-unit increase in slope, odds of tortoise presence are expected to increase by 24% 


#### 2018 Dataset 2
### Dataset 2

# Survival of 45 individuals in the Donner Party

survive <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

age <- c(23,30,28,40,45,62,65,45,25,28,23,47,57,25,60,15,50,25,30,25,25,25,30,35,
         24,40,40,28,22,23,28,15,20,18,25,20,32,32,24,30,21,46,32,23,25)

male <- c(1,1,1,1,0,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,
          1,0,0,1,0,0,1,1,1,1,0,0,1,0,1,0,1,0)
hist(survive)
datadon <- data.frame(survive, age, male)
head(datadon)

model2 <- glm(survive~1, family="binomial")
summary(model2)
back2 <- exp(coef(model2))/(1+exp(coef(model2)))
back2
mean <- sum(survive)/length(survive)
mean

model3 <- glm(survive~age+male, family="binomial")
summary(model3)
back3 <- exp(coef(model3))/(1+exp(coef(model3)))
back3

##Graduate question: 
pre <- seq(min(datadon$age),max(datadon$age),by=1)

newfdata <- data.frame(age=pre,male=0)
ypref <- predict(model3,type="response",newdata=newfdata)

newmdata <- data.frame(age=pre,male=1)
yprem <- predict(model3,type="response",newdata=newmdata)

newdata2 <- data.frame(age=pre,male=mean(male))
ypre <- predict(model3,type="response",newdata=newdata2)

plot(datadon$age,datadon$survive, xlab="Age", ylab="Probability of Survival", main="Survival",
     xlim=c(15,65), col=ifelse(datadon$male==0,"red","blue"), pch=16)
lines(pre,ypref, col="red", lwd=2)
lines(pre,yprem, col="blue", lwd=2)
lines(pre,ypre, lwd=2)
legend("topright", c("Female","Male","Overall"), col=c("red","blue","black"), lwd=2,inset=.05)

