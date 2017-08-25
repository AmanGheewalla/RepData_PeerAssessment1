# Reproducible Research Programming Assignment 1

Setting up global options to show all code


```r
knitr::opts_chunk$set(echo = TRUE)
```

Importing Packages

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(timeDate)
```


## Downloading, Importing, and Processing Data


```r
# Download the dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="C:/Users/amang/Documents/Coursera/Reproducible Research/Programming Assignment 1/Activity Monitoring Data.zip")

# Unzip the file
unzip(zipfile="C:/Users/amang/Documents/Coursera/Reproducible Research/Programming Assignment 1/Activity Monitoring Data.zip",exdir="C:/Users/amang/Documents/Coursera/Reproducible Research/Programming Assignment 1")

# Read activity table:
activity <- read.csv("C:/Users/amang/Documents/Coursera/Reproducible Research/Programming Assignment 1/activity.csv")

# Convert date field into date format
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

Create histogram of steps per day


```r
# Remove NA values from dataset
activityactual <- activity[complete.cases(activity), ]

# Group by date to get a sum of steps and create histogram
grp <- group_by(activityactual, date)
steps <- summarise(grp, steps = sum(steps))
qplot(steps, data = steps, fill = I("red"), col = I("white"))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/total_steps-1.png)<!-- -->


Calculate mean of steps

```r
mean(steps$steps)
```

```
## [1] 10766.19
```

Calculate median of steps

```r
median(steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Create time series plot

```r
# Group by interval to get the average steps for each interval
grp_interval <- group_by(activityactual, interval)
interval <- summarize(grp_interval, steps = mean(steps))

# Add column with preceeding zeros to minutes for further transformation
interval <- mutate(interval, time = formatC(interval$interval, width = 4, format = "d", flag = "0"))
interval$time <- strptime(interval$time, format = "%H%M")

# Create time series plot
qplot(interval$time, interval$steps, geom=c("line"), col = I("red"), xlab = "Time", ylab = "Steps")
```

![](PA1_template_files/figure-html/interval-1.png)<!-- -->

Interval with maximum number of steps

```r
#Interval with maximumn number of steps
interval[which(interval$steps == max(interval$steps)), ]
```

```
## # A tibble: 1 × 3
##   interval    steps      time
##      <int>    <dbl>    <dttm>
## 1      835 206.1698 <POSIXlt>
```

## Input missing values

Find the total number of missing values

```r
sum(is.na(activity))
```

```
## [1] 2304
```

Create a new dataset that replaces the missing values with an average of the interval that it belongs to

```r
#Calculate average steps per interval
interval_mean <- summarise(grp_interval, steps = mean(steps))$steps

#Fill in missing values
activity_complete <- within(activity, steps <- ifelse(is.na(steps), interval_mean, steps))
```

Create data frame by populating NA values with the average of interval data

```r
# Group by date to get a sum of steps and create histogram
grp_complete <- group_by(activity_complete, date)
steps_complete <- summarise(grp_complete, steps = sum(steps))
qplot(steps, data = steps_complete, fill = I("blue"), col = I("white"))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate mean of steps with missing data filled in

```r
mean(steps_complete$steps)
```

```
## [1] 10766.19
```

Calculate median of steps with missing data filled in

```r
median(steps_complete$steps)
```

```
## [1] 10766.19
```

* We see that the shape of the histogram of the two datasets (one with missing values and one without) remains generally the same.

* However, the values between the two histograms differ as the range of the plot showing steps with the missing values replaced is higher than the plot with missing values.

* Replacing the missing values gave us a more accurate analysis as we still have the same amount of observations, but more data to reflect those observations

##Are there differences in activity patterns between weekdays and weekends?

We will add a new field to the dataset with missing values filled, and that field will tell us whether the date field is a weekday or a weekend

```r
# Create daytpe field to tell us whether date is weekday or weekend
activity_complete <- mutate(activity_complete, daytype = ifelse(isWeekday(activity_complete$date), "weekday", "weekend"))
weekday <- filter(activity_complete, daytype == "weekday")
weekend <- filter(activity_complete, daytype == "weekend")
```

Create time series plot

```r
# Group by interval to get the average steps for each interval
grp_weekday <- group_by(weekday, interval, daytype)
grp_weekend <- group_by(weekend, interval, daytype)
interval_weekday <- summarize(grp_weekday, steps = mean(steps))
interval_weekend <- summarize(grp_weekend, steps = mean(steps))

# Add column with preceeding zeros to minutes for further transformation
interval_weekday$time <- formatC(interval_weekday$interval, width = 4, format = "d", flag = "0")
interval_weekend$time <- formatC(interval_weekend$interval, width = 4, format = "d", flag = "0")
interval_weekday$time <- strptime(interval_weekday$time, format = "%H%M")
interval_weekend$time <- strptime(interval_weekend$time, format = "%H%M")

# Weekday Plot
qplot(interval_weekday$time, interval_weekday$steps, geom=c("line"), col = I("blue"), xlab = "Time", ylab = "Steps", main = "Weekday")
```

![](PA1_template_files/figure-html/interval_complete-1.png)<!-- -->

```r
# Weekend Plot
qplot(interval_weekend$time, interval_weekend$steps, geom=c("line"), col = I("blue"), xlab = "Time", ylab = "Steps", main = "Weekend")
```

![](PA1_template_files/figure-html/interval_complete-2.png)<!-- -->
