data <- read.csv('activity.csv')
data$date <- as.Date(data$date)

length(unique(data$date))
names(data)
class(data$date)
total_steps_taken_per_day<- tapply(data$steps, data$date, sum)

head(total_steps_taken_per_day)
hist(total_steps_taken_per_day, xlab = "Total steps taken per day", ylab = "frequency", col = "grey", breaks = 20, main = "Histogram of total steps taken per day")


# Mean number of steps taken per day
mean(total_steps_taken_per_day, na.rm = TRUE)

#Median number of steps taken per day
median(total_steps_taken_per_day, na.rm = TRUE)

# Time series graph of number of steps Vs Five minute intervals
five_minute_interval <- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)
plot(x= five_minute_interval$interval , y = five_minute_interval$steps, xlab = "Time interval", ylab = "Number of steps", type = "l", lwd = 2, col = "blue")

# Five min interval that contains the maximum number of steps 
five_minute_interval_with_max_steps <- NA
max_steps <- max(five_minute_interval$steps)
for(i in 1: dim(five_minute_interval)[1]){
if(five_minute_interval$steps[i] == max_steps){
five_minute_interval_with_max_steps = five_minute_interval$interval[i]
}
}
print(five_minute_interval_with_max_steps)

# 835 5th min interval had max number of steps 

# Find number of missing values in dataset
total_na<- 0
for(i in 1: dim(data)[1]){

if(is.na(data$steps[i])){
total_na<- total_na +1
}
}
#Total number of missing values :
print(total_na)

#Imputing missing values

data_filled_in <- data
for (i in 1:17568) # loop to find the na
{
    if(is.na(data_filled_in$steps[i])) # if steps is na store the pointer 
    { 
        five_minute_pointer <- data_filled_in$interval[i] #store the value of pointer to find the mean on five minute interval
        for (j in 1:288)  # loop to find the value of pointer on the data frame of five minute interval
        {
            if (five_minute_interval$interval[j] == five_minute_pointer) # finding the value of mean of five minute interval data frame
                data_filled_in$steps[i] <- five_minute_interval$steps[j] # replacing the na by the mean in that fime minute interval 

        }
    }
}

total_steps_each_day_filled_in <- aggregate(steps~date, data=data_filled_in, FUN=sum, na.rm=TRUE)

## Generating the Histogram by each day with new dataset (activity_filled_in)
hist(total_steps_each_day_filled_in$steps, breaks= 10, col = "grey", xlab = "Total steps per day w/ filled in data", ylab = "Frequency", main = "Histogram of total steps per day w/ filled data")


# Activity pattern in weekdays and weekends

#find weather the date is weekday or weekend
day_Type <- ifelse(weekdays(data_filled_in$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"), "weekday","weekend") 

#Weekday and weekend facor variable
library(lattice)
 
averageStepsDayType <- aggregate(steps ~ interval + day_Type, data_filled_in, mean)  
xyplot(steps ~ interval | day_Type, averageStepsDayType, type = "l", aspect = 1/2)  
