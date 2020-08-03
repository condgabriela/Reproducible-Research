---
title: 'Reproducible Research: Peer Assessment 1'
output: 
  html_document:
    keep_md: true
---





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


## Loadind and preprocessing data

```r
# Read data=================================================

activity <- read.csv("./data/activity.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
activity$date<-as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
act_by_day<-group_by(activity, date)%>%
          summarize(steps = sum(steps, na.rm = TRUE))
```



![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



```r
#Mean steps taken per day
mean(act_by_day$steps)
```

```
## [1] 9354.23
```

```r
#Median of steps taken per day
median(act_by_day$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
act_interval<-group_by(activity, interval)%>%
  summarize(steps = mean(steps, na.rm = TRUE))
```


![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



```r
# 5-minute interval with the maximum number of steps
act_interval$interval[which.max(act_interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

```r
na_steps<-c(which(is.na(activity$steps)))
length(na_steps)
```

```
## [1] 2304
```

```r
#For imputing missing values, the mean of each of interval is selected.

median_act_day<-group_by(activity, interval)%>% 
  summarize(steps = mean(steps, na.rm = TRUE))

activity_dup<-activity

for (i in 1:length(activity_dup$steps)){
  for (j in 1:length(median_act_day$steps)){
    if (is.na(activity_dup[i, 1]) == TRUE) {
      activity_dup[i, 1] = median_act_day[j, 2]
      
    }
  }
}    


act_by_day_smooth<-group_by(activity_dup, date)%>%
  summarize(steps = sum(steps, na.rm = TRUE))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->




```r
#Mean steps taken per day
mean(act_by_day_smooth$steps)
```

```
## [1] 9419.081
```

```r
#Median of steps taken per day
median(act_by_day_smooth$steps)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?


```r
activity_dup$weekdays<-weekdays(activity_dup$date)

activity_dup$weekdays[activity_dup$weekdays %in% c("lunes", "martes", 
                                                   "miércoles", "jueves", "viernes")]<-"weekday"


activity_dup$weekdays[activity_dup$weekdays %in% c("sábado", "domingo")]<-"weekend"
activity_dup$weekdays<-as.factor(activity_dup$weekdays)

act_by_weekday<-group_by(activity_dup, interval, weekdays)%>%
  summarize(steps = mean(steps, na.rm = TRUE)) 
```


![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
# On weekdays, activity starts around 5am, peaks around 8 - 9 am and slows down form 8pm.
# On weekends, activity starts around 8am and keeps a more upbeat pace than on weekdays. It diminishes past 8pm.
```
