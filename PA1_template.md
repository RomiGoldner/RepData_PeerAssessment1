--- 
title: "Analyzing Activity Data - Course Project 1" 
output: 
  html_document: 
    keep_md: true 
---

Analyzing Activity Data - Course Project 1
==========================================

General set up for the r code chunks.



## Loading and preprocessing the data

First we will read the file (the directory was set in the console).
The date column was set to be a "date" so it will be easier to work with.


```r
readActivityFile <- read.csv("activity.csv")
readActivityFile$date <- as.Date(readActivityFile$date)
```

## What is mean total number of steps taken per day?

For this part we will ignore the missing values. 
We will make a histogram of the number of steps each day.
We will calculate the mean and median of the total steps. 


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
total_steps <-readActivityFile %>% group_by(date) %>% summarise(totalSteps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
total_steps$totalSteps <- as.numeric(total_steps$totalSteps)
hist(total_steps$totalSteps, xlab = "Total Number of Steps", 
     main = "Total Number of Steps Taken Each Day", col = "red",  breaks=seq(from=0, to=25000, by=2500), ylim = c(0,25))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

We will calculate the mean and median of the total steps.


```r
mean(total_steps$totalSteps)
```

```
## [1] 9354.23
```

```r
median(total_steps$totalSteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

We will make a series plot.


```r
average_steps <-readActivityFile %>% group_by(interval) %>% summarise(average = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(average_steps$interval, average_steps$average, type = "l", 
     xlab = "Time Intervals", ylab = "Average Steps", main = "Average Daily Activity")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
subset(average_steps, average_steps$average == max(average_steps$average))
```

```
## # A tibble: 1 x 2
##   interval average
##      <int>   <dbl>
## 1      835    206.
```

## Imputing missing values

We will calculate the number of missing values.


```r
NA_values <- subset(readActivityFile, is.na(readActivityFile$steps))
nrow(NA_values)
```

```
## [1] 2304
```


Input Values for missing values by inserting the mean for than 5 minute interval.
The new values will be in the new data "imputed_values"


```r
No_NA_Values <-subset(readActivityFile, !is.na(readActivityFile))
imputed_values <- readActivityFile
NA_values <- is.na(imputed_values$steps)
int_avg <- tapply(No_NA_Values$steps, No_NA_Values$interval, mean, na.rm = TRUE, simplify = T)
imputed_values$steps[NA_values] <- int_avg[as.character(imputed_values$interval[NA_values])]
```

Make histogram

```r
new_sum <- tapply(imputed_values$steps, imputed_values$date, sum, na.rm = TRUE)

hist(new_sum, col = "green", xlab = "Daily Steps", main = "Total Number of Steps Taken Each Day Imputed", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

We will calculate the new mean and median of the total steps.


```r
mean(new_sum)
```

```
## [1] 10766.19
```

```r
median(new_sum)
```

```
## [1] 10766.19
```

 We can see that the mean and the median are different from previous results, which 
 supports the fact that the NA values do affect these values.


## Are there differences in activity patterns between weekdays and weekends?

We will use the imputed_values data. 
We will add to the data set a new factor, which will divide the days into weekdays and weekends.
We will make 2 plots - for weekdays and weekends by subsetting by the days.


```r
library(ggplot2)
imputed_values$day_of_week <- weekdays(imputed_values$date)

imputed_values$day_type <- ifelse(imputed_values$day_of_week == "Saturday" | imputed_values$day_of_week == "Sunday", "weekend", "weekday") 

#calculate mean of steps
total_steps <-imputed_values %>% group_by(interval, day_type) %>% summarise(Steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
total_steps$Steps <- as.numeric(total_steps$Steps)

ggplot(data = total_steps, aes(interval, Steps) ) + geom_line() + facet_wrap(~total_steps$day_type) 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

























