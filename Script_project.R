#Loading packages
library(ggplot2)

#Edit local time
localtime <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","C") #Set local time to English

#Loading and preprocessing the data
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
summary(activity)


##What is mean total number of steps taken per day?

#(1) Calculate the total number of steps taken per day
daysteps <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
daysteps

#(2) Make a histogram of the total number of steps taken each day
png(file="plot1.png", height=480, width=480, units="px")
plot1 <- ggplot(daysteps, aes(x=steps)) + 
    geom_histogram(bins=nrow(daysteps)) +
    xlab("Number of steps per day") + ylab("Frequency")
plot1 
dev.off()

#(3) Calculate and report the mean and median of the total number of steps taken per day
stat_steps <- summary(daysteps$steps)[3:4] 
stat_steps


##What is the average daily activity pattern?
#(1) Calcular the averaged of 5-minute inteval
stepsinterval <- aggregate(steps ~ interval, activity, mean, na.rm=TRUE)
head(stepsinterval)   

#(2) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
png(file="plot2.png", height=480, width=480, units="px")
plot2 <- ggplot(stepsinterval, aes(x=interval, y=steps)) + 
    geom_line() +
    xlab("averaged 5-min interval") + ylab("averaged of steps taken")
plot2 
dev.off()

#(3)Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxStepsbyInterval <- stepsinterval[which.max(stepsinterval[,2]),1]    
maxStepsbyInterval    


##Imputing missing values

#(1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NA_total <- sum(is.na(activity$steps))
NA_total

#(2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
changeNAs <- function(x){ 
    interSte <- aggregate(x[,1]~ x[,2], x, mean, na.rm=TRUE)
    id.na <- which(is.na(x[,1]))
    change <- x[,1]
    for(i in 1:length(id.na)){
        inter <- x[id.na[i],2]
        change[id.na[i]] <- interSte[which(interSte[,1]==inter),2] 
    }
    change
}

#(3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity_notNas <- activity
activity_notNas$steps <- changeNAs(activity[, c("steps","interval")])
summary(activity_notNas)


#(4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
StepsDayNotNA <- aggregate(steps ~ date, activity_notNas, sum)    
png(file="plot3.png", height=480, width=480, units="px")
plot3 <- ggplot(StepsDayNotNA, aes(x=steps)) + 
    geom_histogram(bins=nrow(StepsDayNotNA)) +
    xlab("Number of steps per day") + ylab("Frequency")
plot3 
dev.off()
#Calculating mean and median of Steps per day change the NAs by mean values of interval 5-minute
stat_steps_not_nas <- summary(StepsDayNotNA$steps)[3:4] 

stat_steps_not_nas #change NAs
stat_steps #ignoring NAs

#Answer: the mean not changed with replacement of NAs values, while the median increses 1%


##Are there differences in activity patterns between weekdays and weekends?

#(1) For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
weekday <- weekdays(activity_notNas$date)
activity_notNas$weekday <- weekday
summary(activity_notNas)


#(2) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
id.weekend <- grepl("Saturday|Sunday", activity_notNas$weekday)
activity_notNas$week <- factor(ifelse(id.weekend==TRUE, "weekend", "weekday"), levels=c("weekend", "weekday"))
summary(activity_notNas)


#(3) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
#preparing the data
steps_week_interval <- aggregate(steps ~ interval + week, activity_notNas, mean)
steps_week_interval   
#plot
png(file="plot4.png", height=480, width=480, units="px")
plot4 <- ggplot(steps_week_interval, aes(x=interval, y=steps)) + 
    geom_line() +
    facet_grid(week~.) +
    xlab("5-min interval") + ylab("averaged of steps")
plot4 
dev.off()
