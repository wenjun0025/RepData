---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
knitr::opts_chunk$set(echo=TRUE,warning = F,message=F)
```


## Loading and preprocessing the data


```r
library(readr)
options(digits = 2)
activity=read_csv("activity.csv",col_names = T)
str(activity)
```

```
## spc_tbl_ [17,568 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```


## What is mean total number of steps taken per day?

```r
library(dplyr)
activitytotal=activity %>% 
    group_by(date) %>% 
    summarize(steps_total=sum(steps,na.rm = T))
mean=mean(activitytotal$steps_total)
median=median(activitytotal$steps_total)
library(ggplot2)
ggplot(activitytotal,aes(date,steps_total))+
    geom_histogram(stat="identity")+
    labs(x="Date",y="Total steps",title="Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean total number of steps taken per day was 9354.23, and the median number was 1.04\times 10^{4}.

## What is the average daily activity pattern?

```r
activitymean=activity %>% 
    group_by(interval) %>% 
    summarize(meansteps=mean(steps,na.rm=T))
maxsteps=max(activitymean$meansteps)
maxinterval=activitymean$interval[activitymean$meansteps==maxsteps]
ggplot(activitymean,aes(interval,meansteps))+
    geom_line(col="blue")+
    labs(x="Interval",y="Mean steps",title="The average number of steps taken per interval")
```

![](PA1_template_files/figure-html/pattern-1.png)<!-- -->

The 835 interval, on average across all the days, contains the maximum number of steps, which was 206.17 steps.

## Imputing missing values

```r
na=sum(is.na(activity$steps))
activityimpute = activity %>% 
    group_by(interval) %>% 
    mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=T),steps))
activityimputetotal=activityimpute %>% 
    group_by(date) %>% 
    summarize(steps_total=sum(steps))
mean_i=mean(activityimputetotal$steps_total)
median_i=median(activityimputetotal$steps_total)
ggplot(activityimputetotal,aes(date,steps_total))+
    geom_histogram(stat="identity")+
    labs(x="Date",y="Total steps",title="Total number of steps taken per day based on imputed data")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Based on imputed data, the mean total number of steps taken per day was 1.08\times 10^{4}, and the median number was 1.08\times 10^{4}. The mean and median of imputed data are a little bit higher than data with NA.


## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)
activityimpute=activityimpute %>% 
    mutate(weekend=factor(ifelse(wday(date)==1|wday(date)==7,1,0),levels=c(1,0),labels = c("Weekend","weekday")))
activityimputemean=activityimpute %>% 
    group_by(weekend,interval) %>% 
    summarise(meansteps=mean(steps))
ggplot(activityimputemean,aes(interval,meansteps))+
    geom_line(col="blue")+
    facet_wrap(~weekend)+
    labs(x="Interval",y="Mean steps",title="The average number of steps taken per interval on weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


