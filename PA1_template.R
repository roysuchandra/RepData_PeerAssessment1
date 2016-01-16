## Assignment Course project 1
## It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

##This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##The data for this assignment can be downloaded from the course web site:
  
##Dataset: Activity monitoring data [52K]
##The variables included in this dataset are:
  
 ## steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
##date: The date on which the measurement was taken in YYYY-MM-DD format
##interval: Identifier for the 5-minute interval in which measurement was taken
##The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Loading and preprocessing the data
###Show any code that is needed to

###1.Load the data (i.e. read.csv())
###2.Process/transform the data (if necessary) into a format suitable for your analysis

activity <- read.csv("activity.csv", colClass=c('integer', 'Date', 'integer'))


##What is mean total number of steps taken per day?

###1. Calculate the total number of steps taken per day
steps_date <- aggregate(steps ~ date, activity, sum)

###2. If you do not understand the difference between a histogram and a barplot, research the difference between them. 
### Make a histogram of the total number of steps taken each day

hist(steps_date$steps, main =" ",breaks = 10, col="darkgreen", xlab = "Total number of steps taken each day")

###3. Calculate and report the mean and median of the total number of steps taken per day

mean(steps_date$steps)

median(steps_date$steps)


## What is the average daily activity pattern?
###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all days (y-axis)

steps_interval <- aggregate(steps ~ interval, activity, mean)
head(steps_interval)
plot(steps_interval, type='l', xlab="5-minute interval",ylab="Number of steps taken",
     main="Average daily activity pattern", col="darkgreen")

steps_interval[which.max(steps_interval$steps), ]

##Imputing missing values
###Note that there are a number of days/intervals where there are missing values (coded as NA).
##The presence of missing days may introduce bias into some calculations or summaries of the data.

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))

###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
### For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity_clean <- merge(activity, steps_date, by="date", suffixes=c("", ".mean"))
nas <- is.na(activity_clean$steps)
activity_clean$steps[nas] <- activity_clean$steps.mean[nas]
activity_clean <- activity_clean[, c(1:3)]
head(activity_clean)

###4. Make a histogram of the total number of steps taken each day 
### and Calculate and report the mean and median total number of steps taken per day. 
### Do these values differ from the estimates from the first part of the assignment? 
### What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps_date <- aggregate(steps ~ date, activity_clean, sum)
head(steps_date)
hist(steps_date$steps, main=" ", breaks = 10,col="darkgreen",xlab="Total number of steps taken daily after imputing NA")

###Calculate and report the mean and median total number of steps taken per day.
mean(steps_date$steps)
median(steps_date$steps)


###Are there differences in activity patterns between weekdays and weekends?

###For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part
###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
###indicating whether a given date is a weekday or weekend day.

dayType <- function(dates) {
  f <- function(date) {
    if (weekdays(date) %in% c("Saturday", "Sunday")) {
      "weekend"
    }
    else {
      "weekday"
    }
  }
  sapply(dates, f)
}

activity$dayType <- as.factor(dayType(activity$date))

###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
### See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

library(lattice)
steps.interval <- aggregate(steps ~ interval + dayType, activity, mean)

xyplot(steps ~ interval | dayType, data=steps.interval, layout=c(2,1), type='l',xlab="5-minute interval",
       ylab="Average number of steps taken", col="darkgreen")
