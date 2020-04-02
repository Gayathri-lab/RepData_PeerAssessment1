---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true 
---

This document primarily focusses on the basic exploratory analysis done wih the activity data collected by monitoring devices like fitbit, Jawbone Up etc., The data considered here is for a two month period and it includes number of steps taken in 5 minutes interval each day.

Lets start the EDA by loading all the required packages that I will be using in this analysis:  

# Loading Packages

```r
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
```

# Loading and preprocessing the data


```r
setwd("C:\\Users\\hp\\Downloads\\repdata_data_activity")
activity <- read.csv("activity.csv")
activity$steps <- as.numeric(activity$steps)
activity$interval <- as.numeric(activity$interval)
```

Lets see how the data looks like:

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
## What is mean total number of steps taken per day?

Histogram of total steps taken per day

```r
steps_per_day <- activity %>% group_by(date) %>% summarise(TotalSteps=sum(steps))
hist(steps_per_day$TotalSteps)
```

![](PA_1_Assignment_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Lets look at the mean and medain of the steps taken:

```r
sprintf("Mean of the total steps taken each day : %g ",mean(steps_per_day$TotalSteps,na.rm=TRUE))
```

```
## [1] "Mean of the total steps taken each day : 10766.2 "
```

```r
sprintf("Median of the total steps taken each day : %g ",median(steps_per_day$TotalSteps,na.rm=TRUE))
```

```
## [1] "Median of the total steps taken each day : 10765 "
```

## What is the average daily activity pattern?

Time series plot of the 5-min interval(x-axis) and average steps taken,averaged across all days


```r
steps_across_interval <- activity %>% group_by(interval) %>% summarise(AverageSteps = mean(steps,na.rm=TRUE))

ggplot(data = steps_across_interval, aes(x = interval, y = AverageSteps)) + 
  geom_line(color = "#FC4E07", size = 2)
```

![](PA_1_Assignment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

As seen from the graph above,

```r
sprintf("The 5-min interval range that has highest average steps taken is: %g",steps_across_interval[which.max(steps_across_interval$AverageSteps),1])
```

```
## [1] "The 5-min interval range that has highest average steps taken is: 835"
```

## Imputing missing values


```r
sprintf("Total number of missing values in the dataset is : %g ",sum(is.na(activity$steps)))
```

```
## [1] "Total number of missing values in the dataset is : 2304 "
```

Creating a new data set on which Missing value treatment is going to be applied:

```r
Modifiedactivity <- activity
```
Here, we are ging to replace the missing values by the mean,

```r
Modifiedactivity$steps[is.na(activity$steps)] <- mean(Modifiedactivity$steps,na.rm=TRUE)
Modifiedactivity$steps <- as.numeric(Modifiedactivity$steps)

sprintf("Total number of missing values in the dataset is : %g ",sum(is.na(Modifiedactivity$steps)))
```

```
## [1] "Total number of missing values in the dataset is : 0 "
```


```r
##Summarizing data by date
modified_steps_per_day <- Modifiedactivity %>% group_by(date) %>% summarise(TotalSteps=as.integer(sum(steps)))

hist(modified_steps_per_day$TotalSteps)
```

![](PA_1_Assignment_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Now that the missing values has been treated, lets see the impact of imputing missing data in mean and median 

```r
Mean_before <- as.integer(mean(steps_per_day$TotalSteps,na.rm=TRUE))
Median_before <- as.integer(median(steps_per_day$TotalSteps,na.rm=TRUE))
sprintf("Before imputing missing data: Mean=%g , Median=%g",Mean_before,Median_before )
```

```
## [1] "Before imputing missing data: Mean=10766 , Median=10765"
```

```r
Mean_after <- as.integer(mean(modified_steps_per_day$TotalSteps))
Median_after <- as.integer(median(modified_steps_per_day$TotalSteps))
sprintf("After imputing missing data:Mean= %g , Median=%g",Mean_after,Median_after)
```

```
## [1] "After imputing missing data:Mean= 10766 , Median=10766"
```

## Are there differences in activity patterns between weekdays and weekends?


```r
#Including a weekday/wekend flag column in the dataset
Weekday_Inclusion <- Modifiedactivity %>% mutate(weekFlag = ifelse(weekdays(as.Date(Modifiedactivity$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday"))

#Summarising data at interval level
Final <- Weekday_Inclusion %>% group_by(interval,weekFlag) %>% summarise(AvgSteps=mean(steps,na.rm=TRUE))


#Plotting a panel plot showing the average steps taken during weekday and weekends
panelplot <- ggplot(Final,aes(x=interval,y=AvgSteps))+ 
               geom_line(color = "#FC4E07", size = 2)
panelplot + facet_grid(weekFlag ~ .)+
  labs(title="Average Steps taken at 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Series")
```

![](PA_1_Assignment_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
