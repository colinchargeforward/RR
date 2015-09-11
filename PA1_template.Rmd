---
output: html_document
---

```{r}
library(dplyr)
library(stats)
library(ggplot2)
```

##Loading and preprocessing the data
```{r,results='hide'}
df <- read.csv('activity.csv',na.strings = 'NA',colClasses = c("integer", "Date", "factor"))
dfun <- filter(df,!is.na(steps))
head(dfun)
by_date <- group_by(dfun,date)
by_interval <- group_by(dfun,interval)
```

##Calculating and plotting the total 
```{r}
totalperday <- summarize(by_date,sum(steps))
colnames(totalperday) <- c('date','totalsteps')
ggplot(totalperday, aes(date, totalsteps)) + geom_bar(stat = "identity", colour = "purple", fill = "purple", width = 0.7) + labs(title = "Total Steps Per Day", x = "Date", y = "Total Steps")
```

##The mean and median of total steps per day
```{r}
totalmean <- mean(totalperday$totalsteps)
totalmedian <- median(totalperday$totalsteps)
totalmean
totalmedian
```

##Daily average activity pattern
```{r}
avgsteps <- summarize(by_interval,mean(steps))
avgsteps <- mutate(avgsteps,Interval = as.numeric(as.character(avgsteps$interval)))
avgsteps <- arrange(avgsteps,Interval)
colnames(avgsteps) <- c('i','steps','interval')
ggplot(avgsteps, aes(interval, steps)) + geom_line(color = "purple", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval")
```
##Imputing missing values
Total number of missing values in given dataset
```{r}
sum(is.na(df))
```

To fill in all the missing values in given dataset
```{r}
newData <- df
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgsteps[which(newData$interval[i] == avgsteps$interval), ]$steps
    }
}
```
Plotting the total steps and calculating the mean and median
```{r}
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",colour = "purple",fill = "purple",width = 0.7 ) + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean
newMedian <- median(newTotalSteps)
newMedian
```
Compare them with the two before imputing missing data:
```{r}
newMean-totalmean
newMedian-totalmedian
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

##Differences in activity patterns between weekdays and weekends
```{r}
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))

avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```