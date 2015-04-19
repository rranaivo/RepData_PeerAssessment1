# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
data <-read.csv ("activity/activity.csv")
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps_taken_per_day <- summarise(group_by(data, date),total_steps=sum(steps,na.rm=TRUE))
hist(steps_taken_per_day$total_steps,main='Total number of steps taken per day',xlab='Steps per day',col='red')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps_taken_per_day$total_steps,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(steps_taken_per_day$total_steps,na.rm=TRUE)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

```r
average_steps_per_interval <- summarise(group_by(data,interval),average_per_interval=mean(steps,na.rm=TRUE))
with(average_steps_per_interval,plot(interval,average_per_interval,type = "l",main='Average number of steps taken per interval',xlab='Interval',ylab='Average number of steps',col='blue'))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
with(average_steps_per_interval,interval[which.max(average_per_interval)])
```

```
## [1] 835
```
## Imputing missing values
Strategy: NA values will be replaced by the mean value with the mean for 5-minute interval.

```r
sum(is.na(data$steps)|is.na(data$date)|is.na(data$interval))
```

```
## [1] 2304
```

```r
data_all <- inner_join(data,average_steps_per_interval)
```

```
## Joining by: "interval"
```

```r
data_filled <- mutate(data_all,steps=ifelse(is.na(steps),average_per_interval,steps))
data_filled <- select(data_filled,-average_per_interval)
data_filled <- summarise(group_by(data_filled, date),total_steps=sum(steps,na.rm=TRUE))
hist(data_filled$total_steps,main='Total number of steps taken per day',xlab='Steps per day',col='green')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(data_filled$total_steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(data_filled$total_steps,na.rm=TRUE)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
data_weekly<-mutate(data,day=as.factor(ifelse(wday(date) %in% c(1,7),"weekend","weekday")))
data_day_interval<- summarise(group_by(data_weekly,day,interval), average=mean(steps,na.rm=TRUE))
library(lattice)
with (data_day_interval, 
      xyplot(average ~ interval|day, type="l", 
             ylab="Number of steps",xlab='Interval',layout=c(1,2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


