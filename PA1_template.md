# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

* Unzip the data
* Set date to class Date
* Add column 'time' with a bit of formatting


```r
unzip( zipfile='activity.zip' )
activity <- read.csv( 'activity.csv', colClasses = c("date" = "Date" ) )
activity <- transform( activity, time=sprintf("%04d", interval ))
```


## What is mean total number of steps taken per day?

* Activity data is grouped by date, with NA removed.
* To make use of the date column, convert the resulting vector to a dataframe.
* Expand the dataframe so it can be used by histogram.


```r
# Calculate stepsperday
stepsperday <- tapply( activity$steps, activity$date, sum, na.rm = TRUE)
# Convert it to a dataframe...
df.steps <- data.frame( stepsperday, date=as.Date(row.names(stepsperday)))
# ...which is then expanded to a vector...
df.dates <- df.steps[rep(1:nrow(df.steps), df.steps$stepsperday ),'date' ]
# ... that can be used for a histogram.
hist( df.dates, breaks="days", main="Steps per day", freq=TRUE, xlab="Date" )
```

![](PA1_template_files/figure-html/stepstaken-1.png)<!-- -->

```r
mean.steps <- mean(stepsperday)
median.steps <- median(stepsperday)
```

Mean steps per day is 9354.2295082.  
Media steps per day is 10395.


## What is the average daily activity pattern?

* Group steps by 5-minute interval
* Convert that to a dataframe for easy access
* Plot the data


```r
meanperinterval <- tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
df.meanperinterval <- data.frame( interval=names(meanperinterval), 
                                  mean=meanperinterval)
plot( unique(activity$interval), meanperinterval, type="l", xlab="Time", 
      ylab="frequency", main="Mean steps per day" )
grid()
```

![](PA1_template_files/figure-html/dailyactitivty-1.png)<!-- -->

```r
intervalmax <- sprintf("%04d",as.integer(names(meanperinterval[meanperinterval==
                                                   max(meanperinterval)])))
```

The most steps on average per 5-minute interval is 0835


## Imputing missing values

* Loop through the activity data
* Any NA is replace with the mean of the corresponding 5-minute interval
* Repeat the steps taken earlier


```r
# Rows with missing values
rowswithna <- sum(sapply(is.na(activity), any ))

activity.new <- activity
for( i in 1:nrow( activity.new ) ) { 
   if ( is.na(activity.new[i,'steps']) ) { 
      activity.new[i,'steps'] <- 
         as.double(subset( df.meanperinterval, interval ==
                        activity.new[i,'interval'], select=c('mean') ) ) } }

# Calculate stepsperday
stepsperday.new <- tapply( activity.new$steps, 
                           activity.new$date, sum, na.rm = TRUE)
# Convert it to a dataframe...
df.steps.new <- data.frame( stepsperday.new, 
                            date=as.Date(row.names(stepsperday.new)))
# ...which is then expanded to a vector...
df.dates.new <- df.steps.new[rep(1:nrow(df.steps.new), 
                                 df.steps.new$stepsperday ),'date' ]
# ... that can be used for a histogram.
hist( df.dates.new, breaks="days", main="Steps per day", freq=TRUE, xlab="Date" )
```

![](PA1_template_files/figure-html/missingvalues-1.png)<!-- -->

```r
mean.steps.new <- mean(stepsperday.new)
median.steps.new <- median(stepsperday.new)
```

Mean steps per day is 1.0766189\times 10^{4}.  
Media steps per day is 1.0766189\times 10^{4}.

## Are there differences in activity patterns between weekdays and weekends?

* Create an extra column for weekday/weekend
* Calculate the mean steps for interval+day
* xyplot the data


```r
library(lattice)
days <- weekdays(activity.new$date)
activity.new$day <- ifelse(days == "Saturday" | days == "Sunday", 
                                "Weekend", "Weekday")

meansteps <- aggregate(activity.new$steps,
                                    by=list(activity.new$interval,
                                            activity.new$day),mean)
names(meansteps) <- c('interval','day','steps')
xyplot(steps~interval | day, meansteps,type="l",
       layout=c(1,2),xlab="Interval",ylab = "Number of steps")
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->
