# Activity Monitoring Data
================================



## 1. Loading and preprocessing the data        


***Loading/Processing the data in R and obtaining overview of the data***


```r
setwd("C:/Users/rolande.mbatchou/Desktop/Data Science/")

#Install necessery packages
install.packages(c("data.table", "codetools", "lattice", "plyr", "knitr", "ggplot2"))
```

```
## Installing packages into 'C:/Users/rolande.mbatchou/Documents/R/win-library/3.0'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(codetools)
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.0.3
```

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
library(knitr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.0.3
```

```r
# Open data and coarce the classes for each column to fit appropriate one. 
activity <- read.csv("./Reproducible Research/Week 2/Peer Assignment 1/activity.csv", colClasses = c("numeric", "Date", "numeric"))
```


***Dealing with missing values***


```r
# Create a new data table that will exclude all na values.
good <- complete.cases(activity)
steps <- activity$steps[good]
date <- activity$date[good]
interval <- activity$interval[good]
data <- data.table(date, interval, steps)
```



## 2. What is mean total number of steps taken per day?


***Converting data from intervals per day to total number of steps per day***


```r
dailysteps <- rowsum(steps, format(date, "%d-%m-%Y"))
```


***Obtaining the histogram of the total number of steps taken each day***


```r
hist(dailysteps, main = "Distribution of Total Daily Steps", xlab = "Frequency of daily steps", col="lightgreen")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


***Obtaining the mean of the total number of steps taken per day***


```r
stepsmean <- mean(dailysteps)
stepsmedian <- median(dailysteps)
```

The mean and median of the total number of steps taken per day are **1.0766 &times; 10<sup>4</sup>** and **1.0765 &times; 10<sup>4</sup>** ,respectively.



## 3. What is the average daily activity pattern?


***Finding the average number of steps taken, averaged across all days***


```r
avgstep <- data.frame(data[, mean(steps), by="interval"])
names(avgstep) <- c("interval", "steps") 
```


***Make a time series plot of the 5-minute interval and the avg number of steps***


```r
plot.ts(avgstep$interval, avgstep$steps, type = "l", main="Daily Activity Pattern", xlab="5-min Intervxal", ylab="Avg Steps Taken", col="blue")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

***Finding the 5-minute interval, which contains the maximum number of steps***


```r
maxsteps <- subset(avgstep, steps == max(avgstep$steps))
maxint <- maxsteps$interval
```

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is **835** 



## 4. Imputing missing values


***Calculating and reporting total number of missing values***


```r
# We have already assigned a set for good values "good", so we just need to sum the na's (!good values).

totalna <- sum(good == FALSE)
```

The total number of missing values in the dataset is **2304**

        
***Filling in missing values***


```r
# We will use the mean for that 5-minute interval to fill in the missing values. In order to achieve this, we will use the "avgstep" data frame, which includes / the intervals and the means for each interval, accross all days. Then, we will merge this new data set with the original data set "activity", by interval, and replace all missing values with mean for that interval. I will create a new data set that is equal to the original dataset but with the missing data filled in. 

r <- merge(activity, avgstep, by="interval", suffixes=c(".activity", ".avgsteps"))
r$newsteps <- ifelse(is.na(r$steps.activity), r$steps.avgstep, r$steps.activity)
activitynew <- data.frame(r$interval, r$date, r$newsteps)
names(activitynew) <- c("interval", "date", "steps")
head(activitynew)
```

```
##   interval       date steps
## 1        0 2012-10-01 1.717
## 2        0 2012-11-23 0.000
## 3        0 2012-10-28 0.000
## 4        0 2012-11-06 0.000
## 5        0 2012-11-24 0.000
## 6        0 2012-11-15 0.000
```


***Make a histogram of the total number of steps taken each day***


```r
#Converting data from intervals per day to total number of steps per day***
dailystepsnew <- rowsum(activitynew$steps, activitynew$date)

#Obtaining the histogram of the total number of steps taken each day***
hist(dailystepsnew, main = "New Distribution of Total Daily Steps", xlab = "Frequency of daily steps", col="lightgreen")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


***Obtaining the new mean and median of the total number of steps taken per day***


```r
stepsmeannew <- mean(dailystepsnew)
stepsmediannew <- median(dailystepsnew)
```

The mean and median of the total number of steps taken per day are **1.0766 &times; 10<sup>4</sup>** and **1.0766 &times; 10<sup>4</sup>** ,respectively. 

The mean is about the same as with the original data set sine we used means of steps to replace na's. As a result, since we have replaced na values with values that are equal to means and closer to overall data set, the median of the total number of steps taken per day is now the same as the mean. The dataset is less skewed by replacing na values with actual values. 


## 5. Are there differences in activity patterns between weekdays and weekends?


***Create a new factor variable with two levels: "weekday" and "weekend".***


```r
activitynew$day <- factor(ifelse(as.POSIXlt(activitynew$date)$wday %% 6 == 0, "Weekend", "Weekday"))
```

***Finding the average number of steps taken, averaged across all days***


```r
temp <- data.table(activitynew)
avgstepnew <- data.frame(temp[, mean(steps), by="interval,day"])
names(avgstepnew) <- c("interval", "day", "steps")
head(avgstepnew)
```

```
##   interval     day   steps
## 1        0 Weekday 2.25115
## 2        0 Weekend 0.21462
## 3        5 Weekend 0.04245
## 4        5 Weekday 0.44528
## 5       10 Weekday 0.17317
## 6       10 Weekend 0.01651
```


***Make a time series plot of the 5-minute interval and the avg number of steps, averaged across all weekday days or weekend days***


```r
coplot(avgstepnew$interval ~ avgstepnew$step | avgstepnew$day, type = "l", main="Daily Activity Pattern", xlab="5-min Interval", ylab="Avg Steps Taken", col="blue")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 
