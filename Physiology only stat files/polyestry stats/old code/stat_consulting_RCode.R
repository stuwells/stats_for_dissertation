### Load and clean Data 

require(data.table)
require(lubridate)
require(ggplot2)
setwd("C:/Users/stuwe/Desktop/Stat analysis from stat lab")
squirrel <- fread('sphys_ra528 -ovulation excel data.csv',data.table = F)
squirrel$date <- as.Date(squirrel$date,format = '%m/%d/%Y')
squirrel$year <- year(squirrel$date)
squirrel$months <- month(squirrel$date, label = T)
squirrel$day <- day(squirrel$date)
names(squirrel)[c(4,6)] <- c('Estradiol','Progesterone')

# Data Exploration and Statistical Analysis

## Log Transformations
# plot(log(squirrel$Progesterone), type='l', ylim=c(4,9), lwd=2, col='green',
#      main='Estradiol ~ Progesterone\nAfter Log Transformation', xlab='Day',ylab='Log Concentration')
# lines(log(squirrel$Estradiol), type='l', lwd=2, col='blue')
# 
# 
# par(mfrow=c(1,2))
# hist(squirrel$Progesterone, main='Progesterone', xlab='Progesterone', 20)
# hist(log(squirrel$Progesterone), main='Log (Progesterone)', xlab='log(Progesterone)', 20)

squirrel$Estradiol <- log(squirrel$Estradiol)
squirrel$Progesterone <- log(squirrel$Progesterone)

### Split year to year 
m1 = squirrel[squirrel$year == 2015, -5]
m2 = squirrel[squirrel$year == 2016, -5]

################################
## Functions to analyze Data
##################################

##################################
## localMaxima : identifies peaks
##               in the data
##################################

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

##################################
## analyze_yearly_trends :
## - compares yearly estradiol
##   and progesterone values
## - plots peaks and events 
##################################


analyze_yearly_trends <- function(m1, lag=0, cutoff_estradiol=2000,
                                  cutoff_progesterone=400){
  ### Detect events 
  estra_max <- numeric(nrow(m1))
  estra_max[localMaxima(m1$Estradiol)] <- 1 
  estra_max[which(m1$Estradiol < log(cutoff_estradiol))] <- 0
  
  proge_max <- numeric(nrow(m1))
  proge_max[localMaxima(m1$Progesterone)] <- 1
  proge_max[which(m1$Progesterone < log(cutoff_progesterone))] <- 0
  
  event <- numeric(nrow(m1))
  for(i in 1:length(event)){
    if(estra_max[i]==1 & proge_max[i+lag]==1){
      event[i] <- 1
    } else if(estra_max[i]==1 & proge_max[i]==1){
      event[i] <- 1
    }
  }
  
  max_mat <- data.frame(estra_max, proge_max, event)
  
  ### Plot all events
  plot(m1$date, m1$Progesterone, type='l', ylim=c(4,9), lwd=2, col='green', 
       main='Estradiol ~ Progesterone', xlab='Day',ylab='Log Concentration')
  lines(m1$date, m1$Estradiol, type='l', lwd=2, col='blue')
  event <- m1$Estradiol * max_mat$event
  event2 <- m1$Progesterone * max_mat$event
  
  points(m1$date, event, col='red', lwd=5)
  points(m1$date, event2, col='red',lwd=5)
  
  segments(x0 =m1$date , y0 = event2, x1 = m1$date, y1=event, lwd=2, lty=2, col='red' )
  legend('topright', c('Estradiol','Progesterone','Ovulation'),
         col=c('blue','green','red'), lwd=2, pch=c(1,1,1))
  
}

##################################
## analyze_monthly_trends :
## - compares monthly estradiol
##   and progesterone values
## - plots peaks and events 
##################################

analyze_monthly_trends <- function(m1, lag=0, cutoff_estradion=2000, 
                                   cutoff_progesterone=400){
  ### Detect events 
  estra_max <- numeric(nrow(m1))
  estra_max[localMaxima(m1$Estradiol)] <- 1 
  estra_max[m1$Estradiol < log(cutoff_estradiol)] <- 0
  
  proge_max <- numeric(nrow(m1))
  proge_max[localMaxima(m1$Progesterone)] <- 1
  proge_max[m1$Progesterone < log(cutoff_progesterone)] <- 0
  
  
  event <- numeric(nrow(m1))
  for(i in 1:length(event)){
    if(estra_max[i]==1 & proge_max[i+lag]==1){
      event[i] <- 1
    } else if(estra_max[i]==1 & proge_max[i]==1){
      event[i] <- 1
    }
  }
  event <- m1$Estradiol * event
  
  
  max_mat <- data.frame(estra_max, proge_max, event)
  
  ### Do monthly panel plots
  m1$event <- event
  newM1 = melt(m1,id.vars = c('id','day','date','months','year'))
  p = ggplot(newM1, aes(x = day, value, col=variable)) + geom_line() + facet_grid(months~.) + 
    theme_bw() +    theme(
      plot.title = element_text(color="black", size=26, face="bold", hjust = 0.5),
      axis.title.x = element_text(color="black", size=20, face="bold"),
      axis.title.y = element_text(color="black", size=20, face="bold"),
      axis.text.x = element_text(color="black", size=14, face="bold")) +
    ggtitle('Monthly Log Concentrations') + xlab('Date') + ylab('Log Concentration')
  
  print(p)
  ### Look at time between peaks
  time_between_peaks <- diff(which(event > 0 ))
  return(time_between_peaks)
  
}
require(knitr)

##################################
## summary_peaks :
## - provides tabular summary of 
##   peaks
##################################

summary_peaks <- function(time_between_peaks){
  numPeaks <- length(time_between_peaks)+1
  AvgLength<- mean(time_between_peaks)
  AvgStd <- sqrt(var(time_between_peaks))
  
  summary_stats <- data.frame(Total_Events = numPeaks,
                              Mean_Time = AvgLength,
                              Median_Time = median(time_between_peaks),
                              # Max_Time_Between_Peaks = max(time_between_peaks),
                              # Min_Time_Between_Peaks = min(time_between_peaks),
                              Std_Deviation = AvgStd)
  
  return(summary_stats)
}

##################################
## test_between_peaks_distribution :
## - conducts KS test
##################################

test_between_peaks_distribution <- function(time_between_peaks){
  
  cat('\nTo determine the strength of the distribution of the data we run a KS test 
from fitting an exponential distribution to the observed data\n')
  require(MASS)
  fit1 <- fitdistr(time_between_peaks, "exponential")
  KS.res <- ks.test(time_between_peaks, "pexp", fit1$estimate)
  print(KS.res)
  
  if(KS.res$p.value > 0.05){
    cat(paste('The p-value > 0.05 suggests that the spontaneous ovulation events
are occuring at regular intervals following an exponential distribution 
with rate parameter =', round(fit1$estimate,2)))
  } else {
    cat('The p-value < 0.05 suggests that the spontaneous ovulation events are not 
occuring at regular intervals, and thus do not follow an exponential distribution 
of equal-peak intervals')
  }
  
  true_Exp <- rexp(10000, fit1$estimate)
  
  plot(ecdf(time_between_peaks), xlim=range(c(time_between_peaks, true_Exp)), col="dodgerblue", main='KS Test Visualization', 
       ylab=paste("Wait-time b/w Events CDF"), xlab= "")
  plot(ecdf(true_Exp), add=TRUE, lty="dashed", col="purple", ylab="", xlab="")
  legend("right", legend=c("2016 observed", "Exponential Dist'n"), col=c("dodgerblue", "purple"), 
         lty="dashed", lwd=2 )
  return(KS.res)
}

# Results 

## 2015

### Identifying Candidate Ovulation Events
analyze_yearly_trends(m1=m1, lag = 0,cutoff_estradiol = 1500,
                      cutoff_progesterone = 200)

analyze_yearly_trends(m1=m2, lag = 0,cutoff_estradiol = 1500,
                      cutoff_progesterone = 200)



### Monthly Examination and KS Test

a = analyze_monthly_trends(m1=m1, lag = 0,cutoff_estradiol = 1500,
                           cutoff_progesterone = 200)
p1 = test_between_peaks_distribution(a)


## 2016

### Identifying Candidate Ovulation Events
analyze_yearly_trends(m1=m1, lag = 0,cutoff_estradiol = 1500,
                      cutoff_progesterone = 200)

analyze_yearly_trends(m1=m2, lag = 0,cutoff_estradiol = 1500,
                      cutoff_progesterone = 200)


### Monthly Examination and KS Test
b = analyze_monthly_trends(m1=m2, lag = 0)
p2 = test_between_peaks_distribution(b)

