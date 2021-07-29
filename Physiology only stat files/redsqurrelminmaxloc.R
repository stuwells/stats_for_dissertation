metadata <- read.csv("redsqurrelminmaxlocatp-csv.csv", header = TRUE, na.strings = c("na"))
summary

#I am going to scale all the data--Will run with and without scaling
metadata$prog.min<- scale(metadata$prog.min.,center = FALSE)
metadata$prog.max<- scale(metadata$prog.max,center = FALSE)
metadata$cort.min<- scale(metadata$cort.min,center = FALSE)
metadata$cort.max<- scale(metadata$cort.max,center = FALSE)
metadata$est.min<- scale(metadata$est.min,center = FALSE)
metadata$est.max<- scale(metadata$est.max,center = FALSE)
metadata$test.min<- scale(metadata$test.min,center = FALSE)
metadata$test.max<- scale(metadata$test.max,center = FALSE)
summary(metadata)
library(dplyr)
library(ggplot2)
library(ggdendro)
library (extrafont)
library(tidyverse)
locdat <- ggplot(metadata, aes(locdat))

boxplot(prog.min ~ location, data = metadata,
        xlab = "Location", ylab = "Progesterone",ylim=c(-0,20),
        frame = FALSE, col = c("blue", "orange", "red"))

boxplot(prog.max ~ location, data = metadata,
        xlab = "Location", ylab = "Progesterone",ylim=c(-0,600),
        frame = FALSE, col = c("blue", "orange", "red"))

## I want to plot min max on the same y axis
library(ggplot2)
library(gapminder)

ggplot(gapminder, aes(x = location, data = locdat, y = "Progesterone"))
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
  stat_summary(prog.min = min, colour = "blue", geom = "point", size = 5) +
  stat_summary(prog.max = max, colour = "orange", geom = "point", size = 5)+
  stat_summary(aes(label=round(..y..,2)), prog.min=min,geom="text",prog.max=max"text" size=6, hjust = -0.3)
