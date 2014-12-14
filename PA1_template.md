## Title: Reproducible Research Peer Assessment 1 

### Loading and preprocessing the data

1. Download, unzip, read and convert to required data.


```r
if (!file.exists("data.zip")) {
        download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                      destfile="data.zip")
        unzip("data.zip")
}

## Read data from csv file
data <- read.csv("activity.csv")

## Convert date to date data type
data$date <- as.Date(data$date)
```

### What is mean total number of steps taken per day?

1. Remove the NA values.

```r
data_remove_na <- na.omit(data) 
```

2. Aggregate the total numbers of steps.

```r
total_daily_steps <- rowsum(data_remove_na$steps, format(data_remove_na$date, '%Y-%m-%d')) 
total_daily_steps <- data.frame(total_daily_steps) 
names(total_daily_steps) <- ("steps")
```

3. Plot the required histogram.

```r
hist(total_daily_steps$steps, col="blue", breaks=10, main="Daily distribution of the total number of steps taken daily", xlab="Total number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

4. Compute the mean of the total numbers of steps taken daily.


```r
mean(total_daily_steps$steps)
```

```
## [1] 10766.19
```

5. Compute the median of the total numbers of steps taken daily.

```r
median(total_daily_steps$steps)
```

```
## [1] 10765
```
- The mean of total number of steps taken daily is 10766.

- The median of total number of steps taken daily is 10765.

## What is the average daily activity pattern?

1. Calculate average steps for each of 5-minute interval during a 24-hour period.

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
mean_steps <- ddply(data_remove_na,~interval, summarise, mean=mean(steps))
```

2. Plot time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
qplot(x=interval, y=mean, data = mean_steps,  geom = "line",
      xlab="5-Minute interval",
      ylab="Average number of steps taken, averaged across all days ",
      main="Average distribution of steps taken, averaged across all days"
)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

3. Report the 5-min interval, on average across all the days in the dataset, contains the maximum number of steps.

```r
mean_steps[which.max(mean_steps$mean), ]
```

```
##     interval     mean
## 104      835 206.1698
```

- The 5-minute interval, on average across all days, that contains the maximum number of steps is 835.

### Imputing missing values

1. Calculate and report the total number of NA values in the dataset.

```r
library(sqldf)
```

```
## Warning: package 'sqldf' was built under R version 3.1.2
```

```
## Loading required package: gsubfn
```

```
## Warning: package 'gsubfn' was built under R version 3.1.2
```

```
## Loading required package: proto
```

```
## Warning: package 'proto' was built under R version 3.1.2
```

```
## Loading required package: RSQLite
```

```
## Warning: package 'RSQLite' was built under R version 3.1.2
```

```
## Loading required package: DBI
```

```
## Warning: package 'DBI' was built under R version 3.1.2
```

```r
NA_values <- sqldf(' 
    SELECT d.*            
    FROM "data" as d
    WHERE d.steps IS NULL 
    ORDER BY d.date, d.interval ')
```

```
## Loading required package: tcltk
```

2. Return the total number of NA values in the dataset

```r
nrow(NA_values)
```

```
## [1] 2304
```
- The total number of NA values in the dataset is 2304

3. Strategy for filling in all of the missing values in the dataset


```r
value1 <- sqldf('  
    SELECT d.*, i.mean
    FROM "mean_steps" as i
            JOIN "data" as d
            ON d.interval = i.interval 
            ORDER BY d.date, d.interval ') 

value1$steps[is.na(value1$steps)] <- value1$mean[is.na(value1$steps)]

value1_total_steps <- as.integer( sqldf(' 
    SELECT sum(steps)  
    FROM value1') );

value1_total_steps_by_date <- sqldf(' 
    SELECT date, sum(steps) as "value1_total_steps_by_date" 
    FROM value1 GROUP BY date 
    ORDER BY date') 

daily_61_steps <- sqldf('   
    SELECT date, value1_total_steps_by_date as "steps"
    FROM "value1_total_steps_by_date"
    ORDER BY date')
```

4. Plot histogram of the total number of steps taken each day


```r
hist(daily_61_steps$steps, 
     main="Daily distribution of the number of steps taken daily",
     breaks=10, col="blue", 
     xlab="Total number of steps taken daily (Including NA value)")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

5. Compute the mean of the total numbers of steps taken daily (Including NA values)

```r
mean(value1_total_steps/NROW(value1_total_steps_by_date))
```

```
## [1] 10766.18
```

6. Compute the median of the total numbers of steps taken daily (Including NA values)

```r
median(value1_total_steps_by_date$value1_total_steps_by_date)
```

```
## [1] 10766.19
```
- The mean of total number steps per day is 10766.

- The median of total number steps per day is 10766.

Observations:

- Do these values (mean and median) differ from the estimates from the first part of the assignment? No. The estimates are quite similar.

- What is the impact of imputing missing data on the estimates of the total daily number of steps? The shape of the histogram remains the same as the histogram from removed NA values. However, the frequency counts increased as expected i.e. Imputing the missing data has increased the average number of steps. In this case, it seems that the data imputation strategy should work for the downstream data analysis and modeling.

### Are there differences in activity patterns between weekdays and weekends?

1. Create a factor variable weektime with two levels (weekday, weekend)

```r
value1$weektime <- as.factor(ifelse(weekdays(value1$date) %in% c("Saturday","Sunday"),"weekend", "weekday"))

value2 <- sqldf('   
    SELECT interval, avg(steps) as "mean.steps", weektime
    FROM value1
    GROUP BY weektime, interval
    ORDER BY interval ')
```

2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
library("lattice")
plot <- xyplot(mean.steps ~ interval | factor(weektime), data=value2, 
            type = 'l',
            main="Activity patterns on weekday days and weekend days",
            xlab="5-Minute interval",
            ylab="Average number of steps taken")
print (plot)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

Observations:

- Are there differences in activity patterns between weekdays and weekends? Yes. The plot indicates that the person moves around more (or more active) during the weekend days.
