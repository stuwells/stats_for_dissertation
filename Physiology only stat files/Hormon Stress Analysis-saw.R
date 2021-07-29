#set directory (where the data are)
rm(list=ls())
#setwd('C:/Users/marim/OneDrive/Documents/Phd/Analysis')

setwd C:\Users\stuwe\OneDrive - University of Arizona\Desktop\MGRS stats results#read the data
getwd
metadata <- read.csv("HormoneDatasetworkingwithseasonscsv.csv", header = TRUE, na.strings = c("na"))
summary(metadata)
#I want to make sure that the hormons level are read as numeric and not characters
metadata$prog.10 <- as.numeric(metadata$prog.10)
metadata$est.10<- as.numeric(metadata$est.10)
metadata$cort.10<- as.numeric(metadata$cort.10)
metadata$Test.10<- as.numeric(metadata$Test.10)
summary(metadata)
#I am going to scale all the data--Will run with and without scaling
metadata$prog.10<- scale(metadata$prog.10,center = FALSE)
metadata$est.10<- scale(metadata$est.10,center = FALSE)
metadata$cort.10<- scale(metadata$cort.10,center = FALSE)
metadata$cort.10<- scale(metadata$Test.10,center = FALSE)
summary()
#Compute summary statistics by groups - count, mean, sd
library(dplyr)
library(ggplot2)
library(ggdendro)
library (extrafont)
library(tidyverse)
library(gapminder)
#I want to use aes to visualize data
estdat <- ggplot(metadata, aes(estdat))
+
####T = subset(testosterone, testosterone > "")
# Or > 0

# I want to see values by location and Season
# L <- Location %>%
 # group_by(Location) %>% 
 # summarize(min_gdp = min(gdpPercap), max_gdp = max(gdpPercap)) %>%
 # arrange(min_gdp, max_gdp)  
   
 library(dplyr)
group_by(metadata, Location) %>%
  summarise(
    count = n(),
    max =  max(prog.10, na.rm = TRUE),
    min = min(prog.10, na.rm = TRUE),
    mean = mean(prog.10, na.rm = TRUE),
    sd = sd(prog.10, na.rm = TRUE)
  )
group_by(metadata, Season) %>%
  summarise(
    count = n(),
    max =  max(prog.10, na.rm = TRUE),
    min = min(prog.10, na.rm = TRUE),
    mean = mean(prog.10, na.rm = TRUE),
    sd = sd(prog.10, na.rm = TRUE)
  )

group_by(metadata, Location) %>%
  summarise(
    count = n(),
    max =  max(est.10, na.rm = TRUE),
    min = min(est.10, na.rm = TRUE),
        mean = mean(est.10, na.rm = TRUE),
    sd = sd(est.10, na.rm = TRUE)
  ) 
group_by(metadata, Season) %>%
  summarise(
    count = n(),
    max =  max(est.10, na.rm = TRUE),
    min = min(est.10, na.rm = TRUE),
    mean = mean(est.10, na.rm = TRUE),
    sd = sd(est.10, na.rm = TRUE)
  )
group_by(metadata, Location) %>%
  summarise(
    count = n(),
    max =  max(cort.10, na.rm = TRUE),
    min = min(cort.10, na.rm = TRUE),
    mean = mean(cort.10, na.rm = TRUE),
    sd = sd(cort.10, na.rm = TRUE)
  )

group_by(metadata, Season) %>%
  summarise(
    count = n(),
    max =  max(cort.10, na.rm = TRUE),
    min = min(cort.10, na.rm = TRUE),
    mean = mean(cort.10, na.rm = TRUE),
    sd = sd(cort.10, na.rm = TRUE)
  )
group_by(metadata, Location) %>%
  summarise(
    count = n(),
    max =  max(Test.10, na.rm = TRUE),
    min = min(Test.10, na.rm = TRUE),
    mean = mean(Test.10, na.rm = TRUE),
    sd = sd(Test.10, na.rm = TRUE) 
  )
group_by(metadata, Season) %>%
  summarise(
    count = n(),
    max =  max(Test.10, na.rm = TRUE),
    min = min(Test.10, na.rm = TRUE),
    mean = mean(Test.10, na.rm = TRUE),
    sd = sd(Test.10, na.rm = TRUE) 
  )
############################################### 
       
#Visualize the data

# Box plot for prog- Location and Season
boxplot(prog.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Progesterone",ylim=c(-0,300),
        frame = FALSE, col = c("blue", "orange", "red"))

boxplot(prog.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Progesterone",ylim=c(-0,100),
        frame = FALSE, col = c("blue", "orange", "grey", "red"))

# Box plot for est--Location and Season
boxplot(est.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Estradiol", ylim=c(-0,400),
        frame = FALSE, col = c("blue", "orange", "red")) 

boxplot(est.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Estradiol", ylim=c(-0,300),
        frame = FALSE, col = c("blue", "orange", "grey","red")) 
# Box plot for cort
boxplot(cort.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Cortisol", ylim=c(-0,200),
        frame = FALSE, col = c("blue", "orange", "red"))

boxplot(cort.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Cortisol", ylim=c(-0,200),
        frame = FALSE, col = c("blue", "orange", "grey", "red"))

# Boxplot for Test-location and season
boxplot(Test.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Testosterone", ylim=c(-0,200),
        frame = FALSE, col = c("blue", "orange", "red"))

boxplot(Test.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Testosterone", ylim=c(-0,200),
        frame = FALSE, col = c("blue", "orange", "grey","red"))
# plotmeans
library("gplots")
plotmeans(prog.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Progesterone Level", 
          main="Mean Plot for Progesterone with 95% CI")

plotmeans(prog.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Progesterone Level", 
          main="Mean Plot for Progesterone with 95% CI") 

plotmeans(est.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Estradiol Level", 
          main="Mean Plot for Estradiol with 95% CI") 

plotmeans(prog.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Estradiol Level", 
          main="Mean Plot for Estradiol with 95% CI") 


plotmeans(cort.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Cortisol Level-ng/g", 
          main="Mean Plot for Cortisol with 95% CI")

plotmeans(prog.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Cortisol Level-ng/g", 
          main="Mean Plot for Cortisol with 95% CI") 

plotmeans(Test.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Testosterone Level-ng/g", 
          main="Mean Plot for Testosterone with 95% CI") 

plotmeans(Test.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Testosterone Level-ng/g", 
          main="Mean Plot for Testosterone with 95% CI") 

est.10 <- ggplot(est.10, aes(Location,)) + 
  geom_abline() +
  geom_boxplot(width = 0.1, height = 0.1) 
p + facet_wrap(~cyl)


########################################################################
# Compute the analysis of variance for Prog
prog.aov <- aov(prog.10 ~ Location, data = metadata)
progs.aov <- aov(prog.10 ~ Season, data = metadata)

# Summary of the analysis
summary(prog.aov)
summary(progs.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(prog.aov)

#Test for assumption of anova 
# 1. Homogeneity of variances
plot(prog.aov, 1)
library(car)
leveneTest(prog.10 ~ Location, data = metadata)
# 2. Normality
plot(prog.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = prog.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

########################################################################
# Compute the analysis of variance for Est
est.aov <- aov(est.10 ~ Location, data = metadata)
ests.aov <- aov(est.10 ~ Season, data = metadata)

# Summary of the analysis
summary(est.aov)
summary(ests.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(est.aov)
TukeyHSD(ests.aov)


#Test for assumption of anova for est,10 
# 1. Homogeneity of variances
plot(est.aov, 1)
plot(ests.aov, 1)
library(car)
leveneTest(est.10 ~ Location, data = metadata)
# 2. Normality
plot(ests.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = est.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

########################################################################
# Compute the analysis of variance for Cort
cort.aov <- aov(cort.10 ~ Location, data = metadata)
corts.aov <- aov(cort.10 ~ Season, data = metadata)
# Summary of the analysis
summary(cort.aov)
summary(corts.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(cort.aov)

#Test for assumption of anova for cort.10
# 1. Homogeneity of variances
plot(cort.aov, 1)
plot(corts.aov, 1)
library(car)
leveneTest(cort.10 ~ Location, data = metadata)
# 2. Normality
plot(cort.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = cort.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

cort.aov <- aov(cort.10 ~ Location, data = metadata)
# Summary of the analysis
summary(cort.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(cort.aov)
#########################################################################
#Test for assumption of anova for Test.10
# 1. Homogeneity of variances
test.aov <- aov(Test.10 ~ Location, data = metadata)
tests.aov <- aov(Test.10 ~ Season, data = metadata)
summary(test.aov)
summary(tests.aov)
plot(test.aov, 1)
plot(tests.aov, 1)
library(car)
leveneTest(Test.10 ~ Location, data = metadata)
# 2. Normality
plot(test.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = test.aov )
summary(test.aov)
summary(tests.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(test.aov)
shapiro.test(x = aov_residuals)
