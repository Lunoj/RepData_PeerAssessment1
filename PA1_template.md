# Reproducible Research: Peer Assessment 1

```r
library(lattice)
```


## Loading and preprocessing the data

download zip file.


```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}


activity <- read.csv("activity.csv",header = TRUE)
```

## What is mean total number of steps taken per day?

sum steps by days , generate histogram and calculare mean and median


```r
activitypeday <-  aggregate(activity$steps, by=list(activity$date),sum,na.rm = TRUE)

colnames(activitypeday) <- c("date","totalactivityperday")

hist(activitypeday$totalactivityperday,main="Histogram of the total number of steps taken each day without NA",
     xlab="Total Numer of steps per day",
     col="blue",
     border = "red",
     breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calcualte mean and median of activity per day. 



The mean is 9354.2295082  and median is 10395.


## What is the average daily activity pattern?

calculate avarage steps per interval.
Plot timeseries avarage number of steps per day by interval.
Find interval with most avarage steps.



```r
meanactivityperinterval <- aggregate(activity$steps,by=list(activity$interval),mean,na.rm=TRUE)

colnames(meanactivityperinterval) <- c("Interval","Mean_activity_per_interval")


plot(meanactivityperinterval$Interval,meanactivityperinterval$Mean_activity_per_interval,
     xaxt="n",type="l",
     col="blue",
     main="Time series plot of the average number of steps taken without NA",
     xlab="Interval",
     ylab = "Mean_activity_per_interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calcualte max activity per interval.



The 5-minute interval, on average across all the days in the dataset, contains the maximum 
number of steps 835, 206.1698113.


## Imputing missing values

calculate total number of missing value in dataset.



Total number of missing value 2304.


for all missing value was replaced with mean value for that particular day and created new dataset.



```r
newactivity <- transform(activity,steps = ifelse(is.na(activity$steps),mean(activity$steps,na.rm=TRUE),activity$steps)) 

newactivityperday <- aggregate(activity$steps,by=list(newactivity$date),sum)

colnames(newactivityperday) <- c("date","activity")
```

Plot histogram of total number of steps taken each day.



```r
hist(newactivityperday$activity,main="Histogram of the total number of steps taken each day with removing missing value",
     xlab="Total Numer of steps per day",
     col="blue",
     border = "red",
     breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Calculate mean and median value for Imputed dataset.




mean value for Imputed data set NA.
median value for Imputed data set NA.


## Are there differences in activity patterns between weekdays and weekends?

Create new factor variable weekday and weekend based on date.



```r
newactivity$day <- factor(weekdays(as.Date(newactivity$date)))

newactivity$wday <- factor(ifelse(newactivity$day  %in% c("Saturday","Sunday"),"Weekend","Weekday"))

meannewactivityperday <- aggregate(newactivity$steps,by=list(newactivity$interval,newactivity$wday),mean)

colnames(meannewactivityperday) <- c("interval","wday","Number_of_Steps")
```

Make a panel plot containing time series plot.



```r
xyplot(Number_of_Steps~interval|wday,meannewactivityperday,type="l",layout=c(1,2),
       ylab = "Number of Steps",
       main = "Activity patterns between weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

