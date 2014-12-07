# Reproducible Research: Peer Assessment 1
## Environment setup

```r
suppressMessages(library(dplyr))
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```r
library(ggplot2)
setwd("C:/MOOC/module5/assessments1/RepData_PeerAssessment1")
```

## Loading and preprocessing the data

```r
data = read.csv("activity.csv",header=T)
data$date = as.Date(data$date,"%Y-%m-%d")
data = tbl_df(data)
```

## What is mean total number of steps taken per day?

```r
daily_steps <- data %>%
                  group_by(date) %>%
                  summarise(total_steps = sum(steps, na.rm=TRUE))

hist(daily_steps$total_steps,
main=" ",
breaks=10,
xlab="Total Number of Steps Taken Daily")
```

![](./PA1_template_files/figure-html/dailytotalsteps-1.png) 


```r
daily_meansteps <- data %>%
                  group_by(date) %>%
                  summarise(mean_steps = mean(steps, na.rm=TRUE))
daily_meansteps
```

```
## Source: local data frame [61 x 2]
## 
##          date mean_steps
## 1  2012-10-01        NaN
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08        NaN
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01        NaN
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04        NaN
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09        NaN
## 41 2012-11-10        NaN
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14        NaN
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30        NaN
```

```r
mean(daily_meansteps$mean_steps,na.rm=TRUE)
```

```
## [1] 37.3826
```


```r
daily_mediansteps <- data %>%
                  filter(!is.na(steps) & steps > 0) %>%
                  group_by(date) %>%
                  summarise(median_steps = median(steps))
daily_mediansteps
```

```
## Source: local data frame [53 x 2]
## 
##          date median_steps
## 1  2012-10-02         63.0
## 2  2012-10-03         61.0
## 3  2012-10-04         56.5
## 4  2012-10-05         66.0
## 5  2012-10-06         67.0
## 6  2012-10-07         52.5
## 7  2012-10-09         48.0
## 8  2012-10-10         56.5
## 9  2012-10-11         35.0
## 10 2012-10-12         46.0
## 11 2012-10-13         45.5
## 12 2012-10-14         60.5
## 13 2012-10-15         54.0
## 14 2012-10-16         64.0
## 15 2012-10-17         61.5
## 16 2012-10-18         52.5
## 17 2012-10-19         74.0
## 18 2012-10-20         49.0
## 19 2012-10-21         48.0
## 20 2012-10-22         52.0
## 21 2012-10-23         56.0
## 22 2012-10-24         51.5
## 23 2012-10-25         35.0
## 24 2012-10-26         36.5
## 25 2012-10-27         72.0
## 26 2012-10-28         61.0
## 27 2012-10-29         54.5
## 28 2012-10-30         40.0
## 29 2012-10-31         83.5
## 30 2012-11-02         55.5
## 31 2012-11-03         59.0
## 32 2012-11-05         66.0
## 33 2012-11-06         52.0
## 34 2012-11-07         58.0
## 35 2012-11-08         42.5
## 36 2012-11-11         55.0
## 37 2012-11-12         42.0
## 38 2012-11-13         57.0
## 39 2012-11-15         20.5
## 40 2012-11-16         43.0
## 41 2012-11-17         65.5
## 42 2012-11-18         80.0
## 43 2012-11-19         34.0
## 44 2012-11-20         58.0
## 45 2012-11-21         55.0
## 46 2012-11-22         65.0
## 47 2012-11-23        113.0
## 48 2012-11-24         65.5
## 49 2012-11-25         84.0
## 50 2012-11-26         53.0
## 51 2012-11-27         57.0
## 52 2012-11-28         70.0
## 53 2012-11-29         44.5
```

```r
median(daily_mediansteps$median_steps)
```

```
## [1] 56
```
## What is the average daily activity pattern?

```r
avg_daily_pattern <- data %>%
                  filter(!is.na(steps) & steps > 0) %>%
                  group_by(interval) %>%
                  summarise(avg_daily_steps = mean(steps))
avg_daily_pattern %>% arrange(desc(avg_daily_steps))
```

```
## Source: local data frame [269 x 2]
## 
##    interval avg_daily_steps
## 1       835        352.4839
## 2       850        324.0000
## 3       845        307.0000
## 4       840        296.6857
## 5       815        287.8966
## 6       855        285.5484
## 7      1540        274.6250
## 8      1550        270.6000
## 9       550        261.3750
## 10      605        261.1000
## ..      ...             ...
```

```r
qplot(x=interval, y=avg_daily_steps, data = avg_daily_pattern,  geom = "line",
      xlab="5-Minute Interval (military time)",
      ylab="Number of Step Count",
      main="Average Number of Steps Taken Averaged Across All Days"
     )
```

![](./PA1_template_files/figure-html/averagedailyactivity-1.png) 
  
Observations:
At interval 8:35am with the highest mean steps of 352.48.

## Imputing missing values

```r
daily_nasteps <- data %>%
                  filter(is.na(steps)) %>%
                  group_by(date) %>%
                  summarise(count_nasteps = sum(is.na(steps)))
daily_nasteps
```

```
## Source: local data frame [8 x 2]
## 
##         date count_nasteps
## 1 2012-10-01           288
## 2 2012-10-08           288
## 3 2012-11-01           288
## 4 2012-11-04           288
## 5 2012-11-09           288
## 6 2012-11-10           288
## 7 2012-11-14           288
## 8 2012-11-30           288
```

```r
sum(daily_nasteps$count_nasteps)
```

```
## [1] 2304
```


```r
imput_median = median(daily_mediansteps$median_steps)

imput_data = data
imput_data$steps[is.na(imput_data$steps)] <- imput_median
```


```r
imput_daily_steps <- imput_data %>%
                  group_by(date) %>%
                  summarise(total_steps = sum(steps))

hist(imput_daily_steps$total_steps,
main=" ",
breaks=10,
xlab="Total Number of Steps Taken Daily")
```

![](./PA1_template_files/figure-html/histomeanmedian-1.png) 

```r
imput_daily_meansteps <- imput_data %>%
                  group_by(date) %>%
                  summarise(mean_steps = mean(steps))
imput_daily_meansteps
```

```
## Source: local data frame [61 x 2]
## 
##          date mean_steps
## 1  2012-10-01 56.0000000
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08 56.0000000
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01 56.0000000
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04 56.0000000
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09 56.0000000
## 41 2012-11-10 56.0000000
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14 56.0000000
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30 56.0000000
```

```r
mean(imput_daily_meansteps$mean_steps)
```

```
## [1] 39.82423
```

```r
imput_daily_mediansteps <- imput_data %>% 
                  filter(steps > 0) %>%
                  group_by(date) %>%
                  summarise(median_steps = median(steps))
imput_daily_mediansteps
```

```
## Source: local data frame [61 x 2]
## 
##          date median_steps
## 1  2012-10-01         56.0
## 2  2012-10-02         63.0
## 3  2012-10-03         61.0
## 4  2012-10-04         56.5
## 5  2012-10-05         66.0
## 6  2012-10-06         67.0
## 7  2012-10-07         52.5
## 8  2012-10-08         56.0
## 9  2012-10-09         48.0
## 10 2012-10-10         56.5
## 11 2012-10-11         35.0
## 12 2012-10-12         46.0
## 13 2012-10-13         45.5
## 14 2012-10-14         60.5
## 15 2012-10-15         54.0
## 16 2012-10-16         64.0
## 17 2012-10-17         61.5
## 18 2012-10-18         52.5
## 19 2012-10-19         74.0
## 20 2012-10-20         49.0
## 21 2012-10-21         48.0
## 22 2012-10-22         52.0
## 23 2012-10-23         56.0
## 24 2012-10-24         51.5
## 25 2012-10-25         35.0
## 26 2012-10-26         36.5
## 27 2012-10-27         72.0
## 28 2012-10-28         61.0
## 29 2012-10-29         54.5
## 30 2012-10-30         40.0
## 31 2012-10-31         83.5
## 32 2012-11-01         56.0
## 33 2012-11-02         55.5
## 34 2012-11-03         59.0
## 35 2012-11-04         56.0
## 36 2012-11-05         66.0
## 37 2012-11-06         52.0
## 38 2012-11-07         58.0
## 39 2012-11-08         42.5
## 40 2012-11-09         56.0
## 41 2012-11-10         56.0
## 42 2012-11-11         55.0
## 43 2012-11-12         42.0
## 44 2012-11-13         57.0
## 45 2012-11-14         56.0
## 46 2012-11-15         20.5
## 47 2012-11-16         43.0
## 48 2012-11-17         65.5
## 49 2012-11-18         80.0
## 50 2012-11-19         34.0
## 51 2012-11-20         58.0
## 52 2012-11-21         55.0
## 53 2012-11-22         65.0
## 54 2012-11-23        113.0
## 55 2012-11-24         65.5
## 56 2012-11-25         84.0
## 57 2012-11-26         53.0
## 58 2012-11-27         57.0
## 59 2012-11-28         70.0
## 60 2012-11-29         44.5
## 61 2012-11-30         56.0
```

```r
median(imput_daily_mediansteps$median_steps)
```

```
## [1] 56
```
  
The mean are close to each other while the median has the same value.  
It seem to have little impact.

## Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```

```r
dataweek = data %>%
               mutate(daytype = ifelse(weekdays(date) %in% c("Satuday", "Sunday"), 
                               "weekend", "weekday")) %>%
               filter(!is.na(steps) & steps > 0) %>% 
               group_by(interval,daytype) %>%
               summarise(avg_daily_steps = mean(steps))

dataweek
```

```
## Source: local data frame [478 x 3]
## Groups: interval
## 
##    interval daytype avg_daily_steps
## 1         0 weekday        30.33333
## 2         5 weekday        18.00000
## 3        10 weekday         7.00000
## 4        15 weekday         8.00000
## 5        20 weekday         4.00000
## 6        25 weekday        19.66667
## 7        25 weekend        52.00000
## 8        30 weekday        28.00000
## 9        35 weekday        46.00000
## 10       45 weekday        39.00000
## ..      ...     ...             ...
```

```r
p2 <- qplot(x=interval, y=avg_daily_steps, data = dataweek,  geom = "line",
      xlab="5-Minute Interval (military time)",
      ylab="Number of Step Count",
      main="Average Number of Steps Taken Averaged Across All Days"
      )

p2 + facet_grid(daytype ~ .)
```

![](./PA1_template_files/figure-html/activitypatterns-1.png) 
