#set directory (where the data are)
rm(list=ls())
#setwd('C:/Users/marim/OneDrive/Documents/Phd/Analysis')

#setwd C:\Users\stuwe\OneDrive - University of Arizona\Desktop\MGRS stats results#read the data
getwd
metadata <- read.csv("HormoneDatasetworkingwithseasonscsv.csv", header = TRUE, na.strings=c("NA"))
#metadata <- read.csv("female fremonti-hormons.csv", header = TRUE, sep = ",")

                    # summary)(metadata)
#I want to make sure that the hormone level are read as numeric and not characters
metadata$prog.10 <- as.numeric(metadata$prog.10)
metadata$est.10 <- as.numeric(metadata$est.10)
metadata$cort.10 <- as.numeric(metadata$cort.10)
metadata$Test.10 <- as.numeric(metadata$Test.10)
summary
#I am going to scale all the data--Will run with and without scaling
metadata$prog.10 <- scale(metadata$prog.10,center = FALSE)
metadata$est.10 <- scale(metadata$est.10,center = FALSE)
metadata$cort.10 <- scale(metadata$cort.10,center = FALSE)
metadata$Test.10 <- scale(metadata$Test.10,center = FALSE)
summary
#Compute summary statistics by groups - count, mean, sd
library(dplyr)
library(ggplot2)
library(ggdendro)
library (extrafont)
library(tidyverse)

#I want to use aes to visualize data
estdat <- ggplot(metadata, aes(estdat))
#This code is from brett on how to use tydyverse
#dataframe %>% group_by(variable1, variable2) %>% summarise(mean(variable3))
#activ %>% group_by(Order, V_Class) %>% summarise(mean(Latitude))
metadata %>% group_by("Location,Season,prog.10,est.10,Test.10,cort.10") %>% 
summarise sd = sd(prog.10, count = n(),mean = mean(est.10, Test.10, sd = sd(est.10,Test.10 )) 
#I want to see values by location adn Season
group_by(metadata,Location) %>%
  summarise(count = n(),mean = mean(prog.10, na.rm = TRUE),sd = sd(prog.10, na.rm = TRUE)

group_by(metadata,Location) %>% summarise(count = n(),mean = mean(est.10, na.rm = TRUE,sd = sd(est.10, na.rm = TRUE
count = n(),mean = mean(cort.10, na.rm = TRUE,sd = sd(cort.10, na.rm = TRUE))))
  
  
group_by(metadata,Location) %>% summarise(
group_by(metadata,Location) %>% summarise(count = n(),mean = mean(Test.10, na.rm = TRUE),sd = sd(Test.10, na.rm = TRUE))

#Visualize the data

# Box plot for prog
boxplot(prog.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Progesterone",ylim = c(-0,300),
        frame = FALSE, col = c("blue", "orange", "grey")

# Box plot for est
boxplot(est.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Estradiol", ylim=c(-0,500),
        frame = FALSE, col = c("blue", "orange", "grey")) 

# Box plot for cort
boxplot(cort.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Cortisol", ylim=c(-0,500),
        frame = FALSE, col = c("blue", "orange", "grey"))

boxplot(Test.10 ~ Location, data = metadata,
        xlab = "Location", ylab = "Testosterone", ylim=c(-0,500),
        frame = FALSE, col = c("blue", "orange", "grey"))
# plotmeans
library("gplots"),
plotmeans(prog.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Progesterone Level", 
          main = "Mean Plot for Progesterone with 95% CI"),

plotmeans(prog.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Progesterone Level", 
          main = "Mean Plot for Progesterone with 95% CI"), 


plotmeans(est.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Estradiol Level", 
          main = "Mean Plot for Estradiol with 95% CI"), 

plotmeans(prog.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Progesterone Level", 
          main = "Mean Plot for Estradiol with 95% CI"), 


plotmeans(cort.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Cortisol Level", 
          main = "Mean Plot for Cortisol with 95% CI"),

plotmeans(cort.10 ~ Season, data = metadata, frame = FALSE,
          xlab = "Season", ylab = "Cortisol Level", 
          main = "Mean Plot for Progesterone with 95% CI"), 

plotmeans(Test.10 ~ Location, data = metadata, frame = FALSE,
          xlab = "Location", ylab = "Testosterone Level", 

plotmeans(Test.10 ~ Season, data = metadata, frame = FALSE,
         xlab = "Season", ylab = "Testosterone Level", 
         main ="Mean Plot for Testosterone with 95% CI")         
          

# Facet by two variables: dose and supp.
# Rows are dose and columns are supp
plotmeans + facet_grid(prog.10 ~ est.10)
# Facet by two variables: reverse the order of the 2 variables
# Rows are supp and columns are dose
bp + facet_grid(supp ~ dose)
bp + facet_grid(dose ~ supp, labeller=label_both)
#p <- ggplot(mpg, aes(displ, cty)) + geom_point()
# Use vars() to supply variables from the dataset:
#p + facet_grid(vars(drv), vars(cyl))
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

########################################################################
# Compute the analysis of variance for Est
est.aov <- aov(est.10 ~ Location, data = metadata)
# Summary of the analysis
summary(est.aov)
#compute Tukey HSD for performing multiple pairwise-comparison between the means of groups
TukeyHSD(est.aov)

#Test for assumption of anova 
# 1. Homogeneity of variances
plot(est.aov, 1)
library(car)
leveneTest(est.10 ~ Location, data = metadata)
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

