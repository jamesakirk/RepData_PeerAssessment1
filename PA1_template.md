# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
library(ggplot2)
unzip('activity.zip')
data <- read.csv('activity.csv', stringsAsFactors=FALSE)
myData <- data.frame(steps=data$steps, date=as.Date(data$date), interval=as.factor(data$interval))
```

## What is mean total number of steps taken per day?

```r
stepseachday <- with(myData, tapply(steps, date, sum))
p <- ggplot(data=NULL, aes(stepseachday))+geom_histogram(binwidth=1000)
p + xlab("Steps per Day") + ylab("Number of Days")
```

![](PA1_template_files/figure-html/E-1.png) 

```r
meansteps <- mean(stepseachday,na.rm = TRUE)
mediansteps <- median(stepseachday, na.rm = TRUE)
```
For simply ignoring the NA values, the median number of steps is 10765, while the mean number of steps is 1.0766189\times 10^{4}.

## What is the average daily activity pattern?

```r
avgstepsinterval <- with(myData, tapply(steps, interval, function(x) mean(x, na.rm =TRUE)))
avgData <- with(myData, data.frame(avgsteps=avgstepsinterval, indices=1:length(levels(interval))))
p<- ggplot(data=avgData, aes(x=indices, y=avgstepsinterval)) + geom_line()
p + ylab("Average Steps per Interval") + xlab("Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
which.max(avgstepsinterval)
```

```
## 835 
## 104
```

## Imputing missing values

```r
#filter the step values
#if it is not an NA, copy it directly
#otherwise, replace it with the mean of the steps that share the same interval
intstep <- with(data,ifelse(
  is.na(steps),
  ave(steps,interval,FUN=function(x) mean(x,na.rm=TRUE)),
  steps
))
#now sum the steps per day and plot
intstepseachday <- with(myData, tapply(intstep, date, sum))
p<-ggplot(data=NULL, aes(intstepseachday))+geom_histogram(binwidth=1000)
p + xlab("Steps per Day") + ylab("Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
newmeansteps <- mean(intstepseachday)
newmediansteps <- median(intstepseachday)
```
For replacing the NA values with the average for that interval over all days, the median number of steps is 1.0766189\times 10^{4}, while the mean number of steps is 1.0766189\times 10^{4}. There is only a slight change in these statitics versus ignorming them. Specifically, the median number of step increases by 1.1886792, while the mean number of steps increases by 0.

## Are there differences in activity patterns between weekdays and weekends?

```r
date <- myData$date
interval <-myData$interval
weekenddays <- which(weekdays(date)=="Saturday" | weekdays(date)=="Sunday")
weekdaydays <- which(!(weekdays(date)=="Saturday" | weekdays(date)=="Sunday"))
weekendaverage <- tapply(intstep[weekenddays], interval[weekenddays], function(x) mean(x, na.rm=TRUE))
weekdayaverage <- tapply(intstep[weekdaydays], interval[weekdaydays], function(x) mean(x, na.rm=TRUE))
weekenddata <- data.frame(averagesteps=weekendaverage, interval=1:length(levels(interval)), dayType='Weekend')
weekdaydata <- data.frame(averagesteps=weekdayaverage, interval=1:length(levels(interval)), dayType='Weekday')
allData <- rbind(weekdaydata, weekenddata)
p<-ggplot(allData, aes(interval, averagesteps))+geom_line()
p + facet_grid(dayType ~ .)+ ylab("Average Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
There is a significant difference in Weekday activity vs Weekend activity. During the weekends, there is more activity in the afternoons.
