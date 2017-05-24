library(lubridate)
library(dplyr)
library(ggplot2)

activity <- read.csv("activity.csv")

activity$date <- ymd( as.character(activity$date) )

sum.steps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
hist(sum.steps)
mean.steps <- mean(sum.steps, na.rm = T)
median.steps <- median(sum.steps, na.rm = T)

# aggregate steps by intervals
agg.steps <- aggregate(list(steps = activity$steps), 
                       by = list(ints = activity$interval),
                       mean, na.rm = T)
#plot time-series of aggregated steps
ggplot(agg.steps, aes(ints, steps)) +
    geom_line() +
    xlab("Daily Intervals") + ylab("Average Steps")

which.max(agg.steps$steps)

missing.data <- sum(is.na(activity$steps))



#create a new dataframe, activity2, and add a new column, filler.
# use group_by to group intervals together.  Use mutate to add the new
# column.  The values in the new column are created by calculating 
# the mean of each interval group.
activity2 <- activity %>% group_by(interval) %>% mutate(filler = mean(steps, na.rm = T))

# replace NAs in steps column with the calculated interval mean found in the 
# filler column
filled.steps <- ifelse(is.na(activity2$steps), activity2$filler ,activity2$steps)

#add filled.steps column to original data set
activity$filled.steps <- filled.steps

# remove original, unfilled steps column and create a new dataset
activity4 <- activity[,c(2,3,4)]

# 
sum.steps2 <- tapply(activity4$filled.steps, activity4$date, FUN=sum, na.rm=TRUE)
hist(sum.steps2)

mean.steps2 <- mean(sum.steps2, na.rm = T)
mean.steps2
median.steps2 <- median(sum.steps2, na.rm = T)
median.steps2

weekdays <- weekdays(activity4$date)
activity4$wkday <- weekdays

weekdays2 <- ifelse(activity4$wkday == c("Saturday", "Sunday"), "Weekend", "Weekday")
activity4$wkday <- as.factor(weekdays2)

# aggregate filled.steps by intervals and weekday factor.  Compute the mean of aggregated steps.
agg.steps2 <- aggregate(filled.steps ~ interval + wkday, activity4, mean)
#plot time-series of aggregated steps
plot <- ggplot(agg.steps2, aes(interval, filled.steps)) + 
        geom_line() +
        xlab("Daily Intervals") + ylab("Average Steps")

#Add panels based on factor variable, weekday.
plot + facet_grid(wkday ~ .) 

