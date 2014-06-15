#Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Load and preprocess the activity data

1. The data file if not present is downloaded
2. It is then read into a data frame.
3. The interval column is then converted into factor type.
4. Next the date column is converted into Date type.



```r
library(ggplot2) 


readData <- function() {
    fileName = "repdata-data-activity.zip"
    sourceURL = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(fileName)) {
        download.file(sourceURL, destfile=fileName, method="curl")
    }
    fileCon <- unz(fileName, "activity.csv")
    activityData <- read.csv(fileCon, header=T, colClasses=c("numeric", "character", "numeric"))
    activityData$interval <- factor(activityData$interval)
    activityData$date <- as.Date(activityData$date, format="%Y-%m-%d")
    activityData
}
activityDataFinal <- readData()
```


## What is mean total number of steps taken per day?

A histogram of the daily total number of steps taken (plotted with a bin interval of 1500 steps).



```r
  calculatedStepsPerDay <- function(activityDataFinal) {
    stepsPerDay <- aggregate(steps ~ date, activityDataFinal, sum)
    colnames(stepsPerDay) <- c("date", "steps")
    stepsPerDay
}

plotStepsPerDay <- function(stepsPerDay, meanSteps, medianSteps) {
    col_labels=c(paste("Mean:", meanSteps), paste("Median:", medianSteps))
    cols = c("blue", "red")
    
    ggplot(stepsPerDay, aes(x=steps)) + 
        geom_histogram(fill="black", binwidth=1500) + 
        geom_point(aes(x=meanSteps, y=0, color="blue"), size=4, shape=14) + 
        geom_point(aes(x=medianSteps, y=0, color="red"), size=4, shape=16) + 
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) + 
        labs(title="Steps Taken per Day", x="NO. of Steps", y="Count") + 
        theme_bw() + theme(legend.position = "bottom")    
}

stepsPerDay <- calculatedStepsPerDay(activityDataFinal)
meanSteps = round(mean(stepsPerDay$steps), 2)
medianSteps = round(median(stepsPerDay$steps), 2)
plotStepsPerDay(stepsPerDay, meanSteps, medianSteps)
```

![plot of chunk stepsPerDay](figure/stepsPerDay.png) 

### For the total number of steps taken/day:
- **Mean:  10766.19**
- **Median:  10765**


## What is the average daily activity pattern?

A plot of the average daily pattern of the number of steps plotted against the interval.




```r
calculatedStepsPerInterval <- function(activityDataFinal) {
    stepsPerInterval <- aggregate(activityDataFinal$steps, by=list(interval=activityDataFinal$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    stepsPerInterval$interval <- as.integer(levels(stepsPerInterval$interval)[stepsPerInterval$interval])
    colnames(stepsPerInterval) <- c("interval", "steps")
    stepsPerInterval
}

plotActivityPattern <- function(stepsPerInterval, maxStepInterval) {
    col_labels=c(paste("Interval with Maximum Activity: ", maxStepInterval))
    cols = c("blue")
    
    ggplot(stepsPerInterval, aes(x=interval, y=steps)) +   
        geom_line(color="black", size=2) +  
        geom_point(aes(x=maxStepInterval, y=0, color="blue"), size=4, shape=15) +  
        scale_color_manual(name=element_blank(), labels=col_labels, values=cols) +     
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw() + theme(legend.position = "bottom")
}

stepsPerInterval <- calculatedStepsPerInterval(activityDataFinal)
maxStepInterval <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval

plotActivityPattern(stepsPerInterval, maxStepInterval)
```

![plot of chunk stepsPerInterval](figure/stepsPerInterval.png) 

The **835th interval** has the maximum activity on the average.

## Taking care of missing values




```r
imputeMeans <- function(activityDataFinal, defaults) {
    extractedNaIndices <- which(is.na(activityDataFinal$steps))
    defaults <- stepsPerInterval
    naReplacement <- unlist(lapply(extractedNaIndices, FUN=function(idx){
        interval = activityDataFinal[idx,]$interval
        defaults[defaults$interval == interval,]$steps
        }))
    imputeSteps <- activityDataFinal$steps
    imputeSteps[extractedNaIndices] <- naReplacement
    imputeSteps
}
completeActivityData <- data.frame(  
    steps = imputeMeans(activityDataFinal, stepsPerInterval),  
    date = activityDataFinal$date,  
    interval = activityDataFinal$interval)
```

## Summarizing the processed dataset :

```r
summary(completeActivityData)
```

```
##      steps            date               interval    
##  Min.   :  0.0   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.0   Median :2012-10-31   10     :   61  
##  Mean   : 37.4   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 27.0   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.0   Max.   :2012-11-30   25     :   61  
##                                       (Other):17202
```

A histogram of the daily total number of steps taken, plotted with a bin interval of 1500 steps.


```r
completeStepsPerDay <- calculatedStepsPerDay(completeActivityData)
completeMeanSteps = round(mean(completeStepsPerDay$steps), 2)
complete_median_steps = round(median(completeStepsPerDay$steps), 2)
plotStepsPerDay(completeStepsPerDay, completeMeanSteps, complete_median_steps)
```

![plot of chunk completeStepsPerDay](figure/completeStepsPerDay.png) 


## Are there differences in activity patterns between weekdays and weekends?


```r
dayOfWeekData <- function(activityDataFinal) {
    activityDataFinal$weekday <- as.factor(weekdays(activityDataFinal$date))
    weekendData <- subset(activityDataFinal, weekday %in% c("Saturday","Sunday"))
    weekdayData <- subset(activityDataFinal, !weekday %in% c("Saturday","Sunday"))
    
    weekendSPL <- calculatedStepsPerInterval(weekendData)
    weekdaySPL <- calculatedStepsPerInterval(weekdayData)
    
    weekendSPL$dayofweek <- rep("weekend", nrow(weekendSPL))
    weekdaySPL$dayofweek <- rep("weekday", nrow(weekdaySPL))
    
    dayOfWeekData <- rbind(weekendSPL, weekdaySPL)
    dayOfWeekData$dayofweek <- as.factor(dayOfWeekData$dayofweek)
    dayOfWeekData
}
dayOfWeekComparisionPlot <- function(dow_data) {
    ggplot(dow_data, 
        aes(x=interval, y=steps)) + 
        geom_line(color="black", size=2) + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
}
dayOfWeekData <- dayOfWeekData(completeActivityData)
dayOfWeekComparisionPlot(dayOfWeekData)
```

![plot of chunk weekday_compare](figure/weekday_compare.png) 
