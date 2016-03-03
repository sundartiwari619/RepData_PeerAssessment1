
## This first line will likely take a few seconds. Be patient!
if(!exists("activity")){
  activity <- read.csv("activity.csv")
}

## What is mean total number of steps taken per day?
total <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
png('figure1.png')
barplot(height=total$steps, names.arg=total$date, xlab="dates", ylab=expression('total steps'),main=expression('Total steps per day'))
dev.off()
mean(total$steps)
median(total$steps)


## What is the average daily activity pattern?
library(ggplot2)
intervalTotal <- aggregate(steps ~ interval, activity, mean, na.rm=TRUE)
png('figure2.png')
plot(intervalTotal$interval,intervalTotal$steps,type="l", xlab="Interval", ylab= "steps", main = "Steps by Interval")
dev.off()

intervalTotal[which.max(intervalTotal$steps),]

## Imputing missing values
missingValues <- is.na(activity$steps)
table(missingValues)

library(Hmisc)

completeActivity <- activity
completeActivity$steps <- impute(completeActivity$steps,fun=mean)
totalimputed <- aggregate(steps ~ interval, completeActivity, sum)
png('figure3.png')
plot(totalimputed$interval,totalimputed$steps,type="h", xlab="Interval", ylab= "steps", main = "Imputed Steps by Interval")
dev.off()

mean(totalimputed$steps)
median(totalimputed$steps)


##Are there differences in activity patterns between weekdays and weekends?

completeActivity$dayType <-  ifelse(as.POSIXlt(completeActivity$date)$wday %in% c(0,6), 'weekend', 'weekday')

totalDayType <- aggregate(steps ~ interval+dayType, completeActivity, mean)
png('figure4.png')
ggplot(totalDayType, aes(interval, steps)) + geom_line() + facet_grid(dayType ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
dev.off()
