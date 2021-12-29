## Remove any existing dataset
## or analysis from your 
## workspace 
rm(list=ls())
sd
#options(warn=-1)

### Load libraries
require(data.table)
require(lubridate)
require(ggplot2)

### Change your directory
#setwd("C:/Users/stuwe/OneDrive - University of Arizona/Desktop/MGRS stats results/Stat analysis from stat lab")
 


###  Read different squirrel file"
#Start multifemale approach
#readdata
squirrel <- fread("all_females_a.csv  ",data.table = F,)


#squirrel <- fread( "Physiology only stat files/mphys_ra.csv",data.table = F)
#squirrel<- read.csv(file="mphys_ra.csv")
### manipulate date 
squirrel$date <- as.Date(squirrel$date,format = '%m/%d/%Y')
squirrel$year <- year(squirrel$date)
squirrel$months <- month(squirrel$date, label = T)
squirrel$day <- day(squirrel$date)
summary(squirrel)
summary(other)
#Adjusted names for code
names(squirrel)[c(3,4)] <- c('Progesterone','Estradiol')

#copy of code from original document to see if it runs correctly--changed ylim to 8,10 from 3,4
plot(log(squirrel$Progesterone), type='l', ylim=c(0,8), lwd=2, col='green',
     main='Estradiol ~ Progesterone\nAfter Log Transformation', xlab='Day',ylab='Log Concentration')
lines(log(squirrel$Estradiol), type='l', lwd=2, col='blue')
par(mfrow=c(1,2))
hist(squirrel$Progesterone, main='Progesterone', xlab='Progesterone', 20)
hist(log(squirrel$Progesterone), main='Log (Progesterone)', xlab='log(Progesterone)', 20)
hist(squirrel$Estradiol, main='Estradiol', xlab='Estradiol', 20)
hist(log(squirrel$Estradiol), main='Log (Estradiol)', xlab='log(Estradiol)', 20)
#Added this to just save the original dataset ~ Brandon
squirrelRAW <- squirrel

## Log Transformation
squirrel$Estradiol <- log(squirrel$Estradiol)
squirrel$Progesterone <- log(squirrel$Progesterone)

### Split year to year 
year2015 = squirrel[squirrel$year == 2015, -5]
year2016 = squirrel[squirrel$year == 2016, -5]
#year2017 = squirrel[squirrel$year == 2017, -5]

################################################################################################
################################################################################################
################################################################################################
##
##
## Functions to analyze Data
##

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

analyze_yearly_trends <- function(m1, lag=1, cutoff_Estradiol=250,
                                  cutoff_Progesterone=50, year='2015',
                                  up_pro=50,
                                  bot_pro=10,
                                  up_est=250,
                                  bot_est=20){
  
  ####------##### Test
  m1 <- year2016
  ####______##### Test
  ### Detect events 
  estra_max <- numeric(nrow(m1))
  estra_max[localMaxima(m1$Estradiol)] <- 0 
  estra_max[which(m1$Estradiol < log(cutoff_Estradiol))] <- 0
  
  proge_max <- numeric(nrow(m1))
  proge_max[localMaxima(m1$Progesterone)] <- 0
  proge_max[which(m1$Progesterone < log(cutoff_Progesterone))] <- 0
  
  event <- numeric(nrow(m1))
  
  for(i in 1:length(event)){
      if( estra_max[i]==1 & sum(proge_max[i:(i+lag)])>0 &
          day(m1$date[i+lag]) - day(m1$date[i]) <= lag){
        #event[i:(i+lag)] <- 1
        event[i] <- 1
      }
    }
    
  max_mat <- data.frame(estra_max, proge_max, event)
  
  ### Plot all events
  plot(m1$date, m1$Progesterone, type='l', ylim=c(3,4), lwd=2, col='green', 
       main=paste('Progesterone ~ Estradiol',year,sep=':'), xlab='Day',ylab='Log Concentration')
  lines(m1$date, m1$Estradiol, type='l', lwd=2, col='blue')
  event <- m1$Estradiol * max_mat$event
  event2 <- m1$Progesterone * max_mat$event
  
  points(m1$date, event, col='red', lwd=5)
  points(m1$date, event2, col='red',lwd=5)
  
  segments(x0 =m1$date , y0 = event2, x1 = m1$date, y1=event, lwd=2, lty=2, col='red' )
  legend('topright', c('Progesterone','Estradiol','Ovulation'),
         col=c('blue','green','red'), lwd=2, pch=c(1,1,1), cex=0.8)
  
  abline(h=log(up_est), lty=2, lwd=2)
  abline(h=log(bot_est), lty=2, lwd=2)
  
  abline(h=log(up_pro), lty=2, col='brown', lwd=2)
  abline(h=log(bot_pro), lty=2, col='brown', lwd=2)
  
  legend('topleft', c(paste('Estr', c(up_est,bot_est),sep='=')), lty=2, col='black', lwd=2)
  legend('bottomleft',c(paste('Proge', c(up_pro,bot_pro),sep='=')), lty=2, col='brown', lwd=2)
  
}

##################################
## analyze_monthly_trends :
## - compares monthly estradiol
##   and progesterone values
## - plots peaks and events 
##################################

analyze_monthly_trends <- function(m1, lag=0, cutoff_Estradiol=200, 
                                   cutoff_Progesterone=30){
  ### Detect events -a third line of code was added to detect progesterone for two days
  estra_max <- numeric(nrow(m1))
  estra_max[localMaxima(m1$Estradiol)] <- 1 
  estra_max[which(m1$Estradiol < log(cutoff_Estradiol))] <- 0
  
  proge_max <- numeric(nrow(m1))
  proge_max[localMaxima(m1$Progesterone)] <- 1
  proge_max[which(m1$Progesterone < log(cutoff_Progesterone))] <- 0
  
  event <- numeric(nrow(m1))
  #cat("starting vaule of event",event,"\n") debugging code
  df = data.frame(estra_max,
                  proge_max,
                  m1$date)
  for(i in 1:length(event)){
    date_search = min(i+lag, length(event))
    
    if( estra_max[i]==1 & sum(proge_max[i:(i+lag)])>0 &
        day(m1$date[date_search]) - day(m1$date[i]) <= lag){
      event[i] <- 1
      event[i:(i+lag)] <- 1
      
    }
  }
  
  df$event <- event
  
  event <- m1$Estradiol * event
#  cat("event after multiplication",event,"\n") debugging code
  max_mat <- data.frame(estra_max, proge_max, event)
  
  
  ### Do monthly panel plots
  m1$event <- event
  #newM1 = melt(m1,id.vars = c('id','day','date','months','year'))
  newM1 <- tidyr::pivot_longer(data=m1,cols=-c('ID','day','date','months','year'),
                               names_to="variable",values_to ="value")
  p1 = ggplot(newM1, aes(x = day, value, col=variable)) + geom_line() + facet_grid(months~.) + 
    theme_bw() +    theme(
      plot.title = element_text(color="black", size=24, face="bold", hjust = 0.5),
      axis.title.x = element_text(color="black", size=18, face="bold"),
      axis.title.y = element_text(color="black", size=18, face="bold"),
      axis.text.x = element_text(color="black", size=12, face="bold")) +
    ggtitle('Ovulation') + xlab('date') + ylab('Log Concentration')
  
  ### Look at time between peaks
  time_between_peaks <- diff(which(event > 0 ))
#  cat("about to look at event...\n") debugging code
  if(sum(event>0,na.rm=TRUE) < 5){
    print('insufficient data to run KS Test')
  } else{
    print(summary_peaks(time_between_peaks))
    print(test_between_peaks_distribution(time_between_peaks))
    
    fit1 <- fitdistr(time_between_peaks, "exponential")

    # 
     true_Exp <- rexp(10000, fit1$estimate)
     
    plot(ecdf(time_between_peaks), xlim=range(c(time_between_peaks, true_Exp)), col="dodgerblue", main='KS Test Visualization', 
          ylab=paste("Wait-time b/w Events CDF"), xlab= "")
     plot(ecdf(true_Exp), add=TRUE, lty="dashed", col="purple", ylab="", xlab="")
     legend("right", legend=c("2016 observed", "Exponential Dist'n"), col=c("dodgerblue", "purple"), 
            lty="dashed", lwd=2 )
     
    p2 <- recordPlot()
    
  }

  print(p1)  
  return(list(summary_peaks(time_between_peaks),
              time_between_peaks,
              df))
  
}

##################################
## summary_peaks :
## - provides tabular summary of 
##   peaks
##################################

summary_peaks <- function(time_between_peaks){
  numPeaks <- length(time_between_peaks) + 1
  AvgLength <- mean(time_between_peaks)
  AvgStd <- sqrt(var(time_between_peaks))
  
  if(numPeaks < 5){
    Pvalue <- NA_real_
  } else{
    require(MASS)
    fit1 <- fitdistr(time_between_peaks, "exponential")
    KS.res <- ks.test(time_between_peaks, "pexp", fit1$estimate)
    Pvalue = KS.res$p.value
  }
  
  
  
  summary_stats <- data.frame(NumberOfOvulation = numPeaks,
                              Avg.Time_BW_Ovu = AvgLength,
                              Median.Time_BW_Ovu = median(time_between_peaks),
                              # Max_Time_Between_Peaks = max(time_between_peaks),
                              # Min_Time_Between_Peaks = min(time_between_peaks),
                              Std_Deviation = AvgStd,
                              Pvalue=Pvalue)
  
  return(round(summary_stats,2))
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
  
  return(KS.res)
}

### Yearly summary
years_plot <- function(days_lag, cutoff_Estradiol, cutoff_Progesterone,
                       up_pro = 70,
                       bot_pro = 3,
                       up_est = 350,
                       bot_est = 20){
  
  par(mfrow=c(2,1), mar=c(5,5,5,5))
  analyze_yearly_trends(m1=year2015, lag = days_lag,
                        cutoff_Estradiol = cutoff_Estradiol,
                        cutoff_Progesterone = cutoff_Progesterone,
                        year='2015',
                        up_pro = up_pro,
                        bot_pro = bot_pro,
                        up_est = up_est,
                        bot_est = bot_est)
  
  analyze_yearly_trends(m1 = year2016, lag = days_lag,
                        cutoff_Estradiol = cutoff_Estradiol,
                        cutoff_Progesterone = cutoff_Progesterone,
                        year = '2016',
                        up_pro = up_pro,
                        bot_pro = bot_pro,
                        up_est = up_est,
                        bot_est = bot_est)
  
  #analyze_yearly_trends(m1=year2017, lag = days_lag,
    #                    cutoff_Estradiol = cutoff_Estradiol,
    ##                    year='2017',
     #                   up_pro = up_pro,
      #                  bot_pro = bot_pro,
       #                 up_est = up_est,
      #                 bot_est = bot_est)
}

##
##
################################################################################################
################################################################################################


################################################################################################
################################################################################################
## 
## Analyze Results
## 


### Set custom parameters
cutoff_Estradiol = 200
cutoff_Progesterone = 30
days_lag = 0



## Analyze squirrel data using
## Monthly Panels + KS Test
#jpeg('C:/Users/stuwe/Desktop/Stat analysis from stat lab/monthy_ovulation2015.jpeg')
time_between_peaks2015 <- analyze_monthly_trends(m1 = year2015, lag = days_lag, 
                                                 cutoff_Estradiol = cutoff_Estradiol, 
                                                 cutoff_Progesterone = cutoff_Progesterone)
#dev.off()

#jpeg('C:/Users/stuwe/Desktop/Stat analysis from stat lab/monthy_ovulation2016.jpeg')
time_between_peaks2016 <- analyze_monthly_trends(m1 = year2016, lag = days_lag, 
                                                 cutoff_Estradiol = cutoff_Estradiol, 
                                                 cutoff_Progesterone = cutoff_Progesterone)

#time_between_peaks2017 <- analyze_monthly_trends(m1=year2017, lag=days_lag, 
                                                # cutoff_Estradiol = cutoff_Estradiol, 
                                                 #cutoff_Progesterone = cutoff_Progesterone)
dev.off()

peakSummary = data.frame(rbind(time_between_peaks2015[[1]], 
      time_between_peaks2016[[1]]), row.names = c('2015','2016'))
print(peakSummary)
#write.csv(peakSummary, file ='polyestry stats/ovulation_summary.csv')

## Analyze squirrel data using
## Yearly analysis

jpeg('polyestry stats/yearlyanalysisa.jpg',width = 600, height = 800)
years_plot(days_lag = days_lag , 
           cutoff_Estradiol = cutoff_Estradiol, 
           cutoff_Progesterone = cutoff_Progesterone,
           up_pro = 70,
           bot_pro = 10,
           up_est = 200,
           bot_est = 20)
dev.off()


#Start multifemale approach
#readdata

#loop over each female
female_ID <- unique(squirrel$ID)
for (one_ID in female_ID) { 
  #do all the stuff here
  cat("analyzing female",one_ID,"\n")

  #subsetdata for this female
  one_female <- squirrel[squirrel$ID == one_ID,-5]
  #subset 2015 and 2016 data
  year_2015 <- one_female[one_female$year == 2015,]
  year_2016 <- one_female[one_female$year == 2016,]
 # year_2016 <- one_female[one_female$year == 2017,]
  #analyze yearly trends
  time_between_peaks2015 <- analyze_monthly_trends(m1 = year_2015, lag = days_lag, 
                                                   cutoff_Estradiol = cutoff_Estradiol, 
                                                   cutoff_Progesterone = cutoff_Progesterone)
   
  
   
  time_between_peaks2016 <- analyze_monthly_trends(m1=year_2016, lag=days_lag, 
                                                   cutoff_Estradiol = cutoff_Estradiol, 
                                                   cutoff_Progesterone = cutoff_Progesterone)
  
 # time_between_peaks2017 <- analyze_monthly_trends(m1=year_2017, lag=days_lag, 
  #                                                 cutoff_Estradiol = cutoff_Estradiol, 
   #                                                cutoff_Progesterone = cutoff_Progesterone)
  
  
  peakSummary = data.frame(rbind(time_between_peaks2015[[1]], 
                                 time_between_peaks2016[[1]]), row.names = c('2015','2016'))
  print(peakSummary)
  
  #save output files
  ovulation_file<-paste0("polyestry stats/ovulation_summary.csv",one_ID,".csv")
  write.csv(peakSummary, file= ovulation_file)
  
  ## Analyze squirrel data using
  ## Yearly analysis
  comparison_file <- paste0("polyestry stats/monthlysummary",one_ID,".jpeg")
  jpeg(comparison_file ,width = 600, height = 800)
  years_plot(days_lag = days_lag , 
             cutoff_Estradiol = cutoff_Estradiol, 
             cutoff_Progesterone = cutoff_Progesterone,
             up_pro = 70,
             bot_pro =10,
             up_est = 200,
             bot_est = 20)
  dev.off()
}
#code suggestions from Oliver to see what output is doing-9-17/21
##cat("date_search:", date_search, "\n")
##cat("First day:", day(m1$date[i]), "\n")
#cat("Last day:", day(m1$date[date_search]), "\n"))
#cat("Estra max:", estra_max[i], "\n")
#cat("Progesterone sum:", sum(proge_max[i:(i+lag)]), "\n")

if(estra_max[i] == 1 & sum(proge_max[i:(i+lag)]) > 0 &
day(m1$date[date_search]) - day(m1$date[i]) <= lag) {
  

