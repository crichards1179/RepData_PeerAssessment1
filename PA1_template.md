---
title: "PA1_template.Rmd"
output: html_document
---
<!-- rmarkdown v1 -->



## Loading and preprocessing the data

Show any code that is needed to:
    Load the data (i.e. read.csv())
    Process/transform the data (if necessary) into a format                suitable for your analysis


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.3.3
```

```r
library(dplyr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
activity <- read.csv("activity.csv")

activity$date <- ymd( as.character(activity$date) )
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

    Calculate the total number of steps taken per day
    If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
    Calculate and report the mean and median of the total number of steps taken per day


```r
sum.steps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
hist(sum.steps)
```

![plot of chunk sum](figure/sum-1.png)

```r
mean.steps <- mean(sum.steps, na.rm = T)
mean.steps
```

```
## [1] 9354.23
```

```r
median.steps <- median(sum.steps, na.rm = T)
median.steps
```

```
## [1] 10395
```
## What is the average daily activity pattern?
    Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    

```r
agg.steps <- aggregate(list(steps = activity$steps), 
                       by = list(ints = activity$interval),
                       mean, na.rm = T)
ggplot(agg.steps, aes(ints, steps)) +
    geom_line() +
    xlab("Daily Intervals") + ylab("Average Steps")
```

![plot of chunk missing](figure/missing-1.png)

```r
which.max(agg.steps$steps)
```

```
## [1] 104
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing.data <- sum(is.na(activity$steps))
missing.data
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
activity2 <- activity %>% group_by(interval) %>% mutate(filler = mean(steps, na.rm = T))

filled.steps <- ifelse(is.na(activity2$steps), activity2$filler ,activity2$steps)

activity$filled.steps <- filled.steps
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity4 <- activity[,c(2,3,4)]
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum.steps2 <- tapply(activity4$filled.steps, activity4$date, FUN=sum, na.rm=TRUE)
hist(sum.steps2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean.steps2 <- mean(sum.steps2, na.rm = T)
mean.steps2
```

```
## [1] 10766.19
```

```r
median.steps2 <- median(sum.steps2, na.rm = T)
median.steps2
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
weekdays <- weekdays(activity4$date)
activity4$wkday <- weekdays

weekdays2 <- ifelse(activity4$wkday == c("Saturday", "Sunday"), "Weekend", "Weekday")
activity4$wkday <- as.factor(weekdays2)

agg.steps2 <- aggregate(filled.steps ~ interval + wkday, activity4, mean)

plot <- ggplot(agg.steps2, aes(interval, filled.steps)) + 
        geom_line() +
        xlab("Daily Intervals") + ylab("Average Steps")

plot + facet_grid(wkday ~ .) 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
