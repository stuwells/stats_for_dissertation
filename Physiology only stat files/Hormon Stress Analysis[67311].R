#set directory (where the data are)
rm(list=ls())
setwd('C:/Users/marim/OneDrive/Documents/Phd/Analysis')

#read the data
metadata <- read.csv("female fremonti-hormons.csv", header = TRUE, sep = ",")

#I want to make sure that the hormone levels are read as numeric and not characters
metadata$prog.10<- as.numeric(metadata$prog.10)
metadata$est.10<- as.numeric(metadata$est.10)
metadata$cort.10<- as.numeric(metadata$cort.10)
metadata$Test.10<- as.numeric(metadata$Test.10)

#I am going to scale all the data
metadata$prog.10<- scale(metadata$prog.10)
metadata$est.10<- scale(metadata$est.10)
metadata$cort.10<- scale(metadata$cort.10)
metadata$Test.10<- scale(metadata$Test.10)

#Compute summary statistics by groups location and season - count, mean, sd
library(dplyr)
group_by(metadata, Location) %>%
  summarise(
    count = n(),
    mean = mean(prog.10, na.rm = TRUE),
    sd = sd(prog.10, na.rm = TRUE)
  )
group_by(metadata, Season) %>%
  summarise(
    count = n(),
    mean = mean(prog.10, na.rm = TRUE),
    sd = sd(prog.10, na.rm = TRUE)
  )

group_by(metadata, Location) %>%
    summarise(
    count = n(),
    mean = mean(est.10, na.rm = TRUE),
    sd = sd(est.10, na.rm = TRUE)
  ) 
group_by(metadata, Season) %>%
summarise(
    count = n(),
    mean = mean(est.10, na.rm = TRUE),
    sd = sd(est.10, na.rm = TRUE)
)
group_by(metadata, Location) %>%
  summarise(
    count = n(),
    mean = mean(cort.10, na.rm = TRUE),
    sd = sd(cort.10, na.rm = TRUE)
  )

group_by(metadata, Season) %>%
  summarise(
    count = n(),
    mean = mean(cort.10, na.rm = TRUE),
    sd = sd(cort.10, na.rm = TRUE)
  )
group_by(metadata, Location) %>%
  summarise(
    count = n(),
    mean = mean(Test.10, na.rm = TRUE),
    sd = sd(Test.10, na.rm = TRUE)
  )
group_by(metadata,Season) %>%
  summarise(
    count = n(),
    mean = mean(Test.10, na.rm = TRUE),
    sd = sd(Test.10, na.rm = TRUE)
  )
########################################


#Visualize the data

# Box plot for prog
boxplot(prog.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Progesterone",ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey"))

boxplot(prog.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Progesterone",ylim=c(-0,5),
        frame = FALSE, col = c("blue", "orange", "grey"))

# Box plot for est--Location and Season
boxplot(est.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Estradiol", ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey")) 

boxplot(est.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Estradiol", ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey")) 
# Box plot for cort
boxplot(cort.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Cortisol", ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey"))

boxplot(cort.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Cortisol", ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey"))

# Boxplot for Test-location and season
boxplot(Test.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Testosterone", ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey"))

boxplot(Test.10 ~ Season, data = metadata,
        xlab = "Season", ylab = "Testosterone", ylim=c(-0,10),
        frame = FALSE, col = c("blue", "orange", "grey"))
# plotmeans
# plotmeans
library("gplots")
plotmeans(prog.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Progesterone Level", 
          main="Mean Plot for Progesteron with 95% CI") 

plotmeans(prog.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Progesterone Level", 
          main="Mean Plot for Progesteron with 95% CI") 
#######################################

plotmeans(est.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Estradiol Level", 
          main="Mean Plot for Estradiol with 95% CI") 

plotmeans(est.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Estradiol Level", 
          main="Mean Plot for Estradiol with 95% CI") 


plotmeans(cort.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Cortisol Level", 
          main="Mean Plot for Cortisol with 95% CI") 

plotmeans(cort.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Cortisol Level", 
          main="Mean Plot for Cortisol with 95% CI") 

plotmeans(Test.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Testosterone Level", 
          main="Mean Plot for Testosterone with 95% CI") 

plotmeans(Test.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Testosterone Level", 
          main="Mean Plot for Testosterone with 95% CI") 

########################################################################
# Compute the analysis of variance for Prog
prog.aov <- aov(prog.10 ~ Location, data = metadata)
# Summary of the analysis
summary(prog.aov)
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
prog.aov <- aov(prog.10 ~ Location, data = metadata)
# Summary of the analysis
summary(prog.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(prog.aov)

#Test for assumption of anova 
# 1. Homogeneity of variances
plot(prog.aov, 1)
library(car)
leveneTest(prog.10 ~ Season, data = metadata)
# 2. Normality
plot(prog.aov, 2)
summary(prog.aov)
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

#Test for assumption of anova 
# 1. Homogeneity of variances
plot(est.aov, 1)
plot(ests.aov,1)
library(car)
leveneTest(est.10 ~ Location, data = metadata)
# 2. Normality
plot(est.aov, 2)
plot(ests.aov,2)
# Extract the residuals
aov_residuals <- residuals(object = est.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
#seasonAOV
 
# Summary of the analysis
summary(est.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(est.aov)

#Test for assumption of anova 
# 1. Homogeneity of variances
plot(est.aov, 1)
library(car)
leveneTest(est.10 ~ Season, data = metadata)
# 2. Normality
plot(est.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = est.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)


########################################################################
# Compute the analysis of variance for Cort
cort.aov <- aov(cort.10 ~ Location, data = metadata)
# Summary of the analysis
summary(cort.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(cort.aov)

#Test for assumption of anova 
# 1. Homogeneity of variances
plot(cort.aov, 1)
library(car)
leveneTest(cort.10 ~ Location, data = metadata)
# 2. Normality
plot(cort.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = cort.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)

