Q1: Loading and preprocessing the data
======================================

    setwd("D:/TIKI/Quan Luong/Coursera/Course 5_Reproducible Research")
    # load raw data
    data <- read.csv("./repdata_data_activity/activity.csv")
    # re-format date 
    data$date <- as.POSIXct(data$date, format="%Y-%m-%d")

Q2: What is mean total number of steps taken per day?
=====================================================

    library(dplyr)
    data_na_rm <- na.omit(data)
    data_sum <- data_na_rm %>% 
                group_by(date) %>% 
                summarise(sum_step = sum(steps))
    hist(data_sum$sum_step, xlab="Total number of steps", 
         main="Histogram of the total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ## [1] "mean total number of steps taken per day 10766.19"

    ## [1] "median total number of steps taken per day 10765"

Q3: What is the average daily activity pattern?
===============================================

    library(ggplot2)
    data_interval <- data_na_rm %>% 
                     group_by(interval) %>% 
                     summarise(sum_step = sum(steps),
                               mean_step = mean(steps))
    ggplot(data_interval, aes(interval, sum_step)) + geom_line() + 
         xlab("interval (minutes)") + 
         ylab("Average number of steps") + 
         ggtitle("Time-series of the average number of steps per intervals/n(NA removed)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    ## [1] "Averaged number of steps peaks at interval 835"

Q4: Imputing missing values
===========================

Report the total number of missing values in the dataset
--------------------------------------------------------

    ## [1] "Total number of missing values in the dataset 2304"

Filling missing data
--------------------

    data_na_filled <- data
    data_na_filled$steps <- ifelse(!is.na(data_na_filled$steps), 
                                   data_na_filled$steps, 
                                   ifelse(data_na_filled$interval == data_interval$interval, 
                                          data_interval$mean_step, 
                                          0))
    #NA values in steps are replaced by mean steps at relevant interval

New dataset
-----------

"data\_na\_filled" is the new dataset with NA values replaced

New histogram, total steps, mean steps, median steps
----------------------------------------------------

    data_na_filled_sum <- data_na_filled %>% 
                          group_by(date) %>% 
                          summarise(sum_step = sum(steps))
    hist(data_na_filled_sum$sum_step, xlab="Total number of steps", 
         main="Histogram of the total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    ## [1] "mean total number of steps taken per day after imputing missing data 10766.19"

    ## [1] "median total number of steps taken per day after imputing missing data 10766.19"

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
-------------------------------------------------------------------------------------

    data_na_filled$date_type <- ifelse(tolower(weekdays(data_na_filled$date)) %in% c("saturday", "sunday"),
                                       "weekend", "weekday")

Calculate mean and median of total steps by new levels of date type
-------------------------------------------------------------------

    data_na_filled_sum_wkday <- data_na_filled %>% 
                                group_by(date_type, interval) %>% 
                                summarise(mean_step = mean(steps))

    library(lattice)
    xyplot(mean_step ~ interval | date_type, data_na_filled_sum_wkday, 
           type="l", 
           lwd=1, 
           xlab="Interval", 
           ylab="Number of steps", 
           layout=c(1,2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
knit2html()
