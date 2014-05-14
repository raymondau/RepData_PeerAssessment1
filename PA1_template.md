Reproduceable Research Peer Assessment 1
========================================
Analysing A Person's Activities Through Numbers of Steps Taken
==============================================================

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and includes the number of steps taken in 5 minute intervals each day.

First we read the file "activity.csv" and set the column classes.


```r
af <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"), 
    header = TRUE)
```


We then sum the steps by date into a data frame steps.date with column names "date", "x". We then convert steps.date to a table, steps.table, so that the histogram can be created.


```r
steps.date <- aggregate(af$steps, by = list(date = af$date), FUN = sum)
steps.table <- xtabs(x ~ date, steps.date)
```


Q1. Display histogram of total number of steps taken per day. We also display the dates under the x-axis. 


```r
x <- barplot(steps.table, xaxt = "n", main = "Total number of steps taken each day", 
    ylab = "Steps")
labs <- names(steps.table)
text(cex = 0.5, x = x - 0.25, y = -1, labs, adj = 1, xpd = TRUE, srt = 90, offset = 1.5, 
    pos = 1)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Q2. Calculate the mean and median of the total number of steps per day, ignoring the NAs. (ANS mean=10766 and median=10765)


```r
mean(steps.date$x, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(steps.date$x, na.rm = TRUE)
```

```
## [1] 10765
```


Q3. Plot a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 

First we find the mean of the number of steps, group by interval and then plot the chart.


```r
steps.interval <- aggregate(af$steps, by = list(interval = af$interval), FUN = mean, 
    na.rm = TRUE)
plot(steps.interval$interval, steps.interval$x, type = "l", main = "Mean number of steps by 5-minutes interval", 
    xlab = "Time interval", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


Q4. Find the interval when the mean of the steps is the maximum. Note that steps.interval has only 288 rows. (ANS: 835)


```r
steps.interval$interval[steps.interval$x == max(steps.interval$x)]
```

```
## [1] 835
```


Q5. Find the number of rows with NA. (ANS 2304)


```r
bad <- is.na(af$steps)
sum(bad)
```

```
## [1] 2304
```


Q6.  Missing value imputation.
We will use the mean for that 5-minute interval as computed in steps.interval above to fill in the missing values.

First we round the number of steps per interval, i.e. there should not be fractional steps. Create a new column "round" to store these in the steps.interval data frame. We then create a new data frame "af1" by copying the original.


```r
steps.interval$round <- round(steps.interval$x)
af1 <- af
```


Using the steps.interval (which contains only 288 intervals per day), we create a repeated list that is the multiple number of dates as per af1. We will use this list to fill in the NA values in one statement using the "bad" list. 

First we find number of unique dates


```r
number.of.dates <- length(af1$date[!(duplicated(af1$date))])
```


We then create the new entire.steps.interval list, matching to the entire length of af1.


```r
entire.steps.interval <- rep(steps.interval$round, number.of.dates)
```


We now fill in the mean of that interval into the respective interval where the value is NA.


```r
af1$steps[bad] <- entire.steps.interval[bad]
```


Q7. Re-plot the histogram of total number of steps taken per day with the missing values imputed. 

First we sum the steps by date into a data frame with column names "date", "x". We then convert data frame to a table so that histogram can be created.


```r
new.steps.date <- aggregate(af1$steps, by = list(date = af1$date), FUN = sum)
new.steps.table <- xtabs(x ~ date, new.steps.date)
```


We now display histogram of total number of steps taken per day. We also display the dates under the x-axis. 


```r
x <- barplot(new.steps.table, xaxt = "n", main = "New total number of steps taken each day", 
    ylab = "Steps")
labs <- names(new.steps.table)
text(cex = 0.5, x = x - 0.25, y = -1, labs, adj = 1, xpd = TRUE, srt = 90, offset = 1.5, 
    pos = 1)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


Q8. Calculate the new mean and median of the total number of steps per day. (ANS mean=10766 and median=10762)


```r
mean(new.steps.date$x)
```

```
## [1] 10766
```

```r
median(new.steps.date$x)
```

```
## [1] 10762
```


Q9. Analysis of imputing the missing values:

    Before missing values imputation:   mean = 10766  and   median = 10765
    After NAs are filled with the mean: mean = 10766  and   median = 10762

    It shows that there is no difference in the mean and median values after imputation if we impute the missing values using the mean of all the steps talen per day. This result is expected since prior to imputation, we ignore all days with missing values. After imputation using the mean, we included these days in the new computation. But because we imputed using the mean, the resultant new mean is no difference from the original. We had simply added more days to be computed, without having any effect on the mean value. 
    
    Of course, if we were to impute the missing values using a value other than the mean, then the resultant new mean will be different from the original mean. A similar explanation follows for the median.

Q10. Explore whether there are differences in activity patterns between weekdays and weekends.

First we find the abbreviated day-name for each date.


```r
af1$days <- weekdays(af1$date, abbreviate = TRUE)
```


We set two vectors containing the day-abbreviation of the weekdays and weekends.


```r
week.days <- c("Mon", "Tue", "Wed", "Thu", "Fri")
week.ends <- c("Sat", "Sun")
```


Now we compute the weekdays & weekends mean of the number of steps, group by interval.


```r
weekday.steps.interval <- aggregate(af1$steps[af1$days %in% week.days], by = list(interval = af1$interval[af1$days %in% 
    week.days]), FUN = mean)
weekend.steps.interval <- aggregate(af1$steps[af1$days %in% week.ends], by = list(interval = af1$interval[af1$days %in% 
    week.ends]), FUN = mean)
```


We then add a label to indicate whether the data belongs to weekends or weekdays.


```r
weekday.steps.interval$days <- "weekday"
weekend.steps.interval$days <- "weekend"
```


We now create a new data frame combining the weekdays and weekends data counts for the purpose of creating the required time series chart.


```r
combined.steps.interval <- rbind(weekday.steps.interval, weekend.steps.interval)
```


We use the lattice package to plot chart fo weekend vs weekday.


```r
library(lattice)

xyplot(x ~ interval | days, data = combined.steps.interval, type = "l", layout = c(1, 
    2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 


Analysis of weekends versus weekdays mean number of steps taken over 5-minutes intervals:

    From the graph, we see that the subject walked more on average during the weekends than weekdays during the afternoon and evening period (1200-2200). On eexplanation could be more outdoor activities were done during this period of time.

    In contrast, the subject walked the most around 8am during weekdays and this I assumed to be his/her walking to the place of work. The subject walked less during working hours (0900-1800) and this could be the nature of the work that impose a more sedatory lifestyle during this period of time. 

    The above activity pattern is typical among modern office workers whereby on average, people walked more during the weekends than weekdays.
    
