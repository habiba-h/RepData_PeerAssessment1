# Data Science with R: Reproduceable Research


### Peer-graded Assignment: Course Project 1



#### Loading relevant libraries


```r
library(dplyr)
library(ggplot2)
library(lubridate)
```

#### 1. Loading and preprocessing the data
a.  Checking if the data is in the working directory

```r
if(!("activity.csv" %in% dir())) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl, destfile = "repdata%2Fdata%2Factivity.zip", method = "curl")
    unzip("repdata%2Fdata%2Factivity.zip")
}
```
b. Load activity data

```r
activity <- read.csv("activity.csv", header = TRUE)
```

c. Transform data: convert date column into the date format

```r
activity$date <- ymd(activity$date)
```
d. Remove NAs

```r
activitySansNA <- filter(activity, !is.na(steps))
```

#### 2. Mean total number of steps taken per day
a. Total number of steps taken each day

```r
by_date <- group_by(activitySansNA, date)
stepsPerDay <- summarize(by_date, total = sum(steps), average = mean(steps), med = median(steps))
```
b. Plot for the total number of steps per day

```r
ggplot(stepsPerDay, aes(date)) + geom_bar(aes(weight  = total), fill = "darkblue") + scale_x_date(date_breaks = "2 day", date_labels = "%m/%d")  + labs(title  ="Total steps per day",  x = "date", y = "total steps") + theme(plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5), axis.title  = element_text(size = 12, face = "italic"), axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) + scale_y_continuous(breaks = seq(min(stepsPerDay$total)-1,max(stepsPerDay$total), 1000 ))
```

![](PA1_template_files/figure-html/steps_per_day-1.png)<!-- -->

c.  Average and median of number of steps taken each day

```r
ggplot(stepsPerDay, aes(x = date))  +  geom_line(aes(y = average, color = "average")) +  geom_line(aes(y = med, color = "median")) + scale_colour_manual("", breaks = c("average", "median"), values = c("maroon4", "orangered1")) + scale_x_date(date_breaks = "2 day", date_labels = "%m/%d")  + labs(title  ="Average and median steps per day",  x = "date", y = "steps") + theme(plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5), axis.title  = element_text(size = 12, face = "italic"), axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) + scale_y_continuous(breaks = round(seq(min(stepsPerDay$average),max(stepsPerDay$average), 5)))
```

![](PA1_template_files/figure-html/average_median_steps_per_day-1.png)<!-- -->

#### 3. Average daily activity pattern
a. Time series plot of the average number of steps taken

```r
by_interval <- group_by(activitySansNA, interval)
stepsPerInterval <- summarize(by_interval, avg = mean(steps))
ggplot(data = stepsPerInterval, aes(interval,avg)) + geom_line(color = "darkblue") + scale_x_continuous(breaks = seq(min(stepsPerInterval$interval), max(stepsPerInterval$interval), 50)) + labs(title  ="Average  steps per interval",  x = "interval", y = "steps") + theme(plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5), axis.title  = element_text(size = 12, face = "italic"), axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) + scale_y_continuous(breaks = round(seq(min(stepsPerInterval$avg),max(stepsPerInterval$avg), 10)))
```

![](PA1_template_files/figure-html/timeSeries_avgsteps-1.png)<!-- -->

b. The 5-minute interval that, on average, contains the maximum number of steps

```r
interval <-  filter(stepsPerInterval, avg == max(stepsPerInterval$avg))$interval
interval
```

```
## [1] 835
```

####4. Imputing missing values

a. Number of rows with NAs

```r
rowsWithNA <- sum(!complete.cases(activity))
rowsWithNA
```

```
## [1] 2304
```

b. Building a new data set with missing values filled by the average of that interval

```r
imputedActivityDt = activity
imputedActivityDt$steps <- ifelse(is.na(imputedActivityDt$steps), stepsPerInterval$avg[match(stepsPerInterval$interval,imputedActivityDt$interval)], imputedActivityDt$steps)
```

c. Histogram of the total number of steps taken each day after missing values are imputed

```r
by_date <- group_by(imputedActivityDt, date)
stepsPerDay <- summarize(by_date, total = sum(steps), average = mean(steps), med = median(steps))

ggplot(stepsPerDay, aes(date)) + geom_bar(aes(weight  = total), fill = "darkblue") + scale_x_date(date_breaks = "2 day", date_labels = "%m/%d")  + labs(title  ="Total steps per day",  x = "date", y = "total steps") + theme(plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5), axis.title  = element_text(size = 12, face = "italic"), axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) + scale_y_continuous(breaks = seq(min(stepsPerDay$total)-1,max(stepsPerDay$total), 1000 ))
```

![](PA1_template_files/figure-html/steps_per_day_imputed_data-1.png)<!-- -->

d. Average and median of number of steps taken each day

```r
ggplot(stepsPerDay, aes(x = date))  +  geom_line(aes(y = average, color = "average")) +  geom_line(aes(y = med, color = "median")) + scale_colour_manual("", breaks = c("average", "median"), values = c("maroon4", "orangered1")) + scale_x_date(date_breaks = "2 day", date_labels = "%m/%d")  + labs(title  ="Average and median steps per day",  x = "date", y = "steps") + theme(plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5), axis.title  = element_text(size = 12, face = "italic"), axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) + scale_y_continuous(breaks = round(seq(min(stepsPerDay$average),max(stepsPerDay$average), 5)))
```

![](PA1_template_files/figure-html/avg_median_steps_per_day_imputed_data-1.png)<!-- -->

#### 5. Differences in activity patterns between weekdays and weekends

a. Add the weekdays/weekends column and convert it into factor variable

```r
imputedActivityDt <- mutate(imputedActivityDt, days = ifelse(weekdays(imputedActivityDt$date) %in% c("Saturday", "Sunday"), "weekends", "weekdays"))
imputedActivityDt$days<- as.factor(imputedActivityDt$days)
```
b. Get the counts of steps for each interval for weekdays and weekends

```r
by_interval <- group_by(imputedActivityDt, interval, days)
stepsPerInterval <- summarize(by_interval, steps = sum(steps))
```
c. Panel plot to compare weekdays and weekends acivity patterns

```r
ggplot(data = stepsPerInterval) + geom_line(aes(interval, steps, color = days)) + facet_grid(days~., scale = "free")  + scale_x_continuous(breaks = seq(min(stepsPerInterval$interval), max(stepsPerInterval$interval), 100)) + labs(title  ="steps per interval",  x = "interval", y = "steps") + theme(plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5), axis.title  = element_text(size = 12, face = "italic"), axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) + scale_y_continuous(breaks = round(seq(min(stepsPerInterval$steps),max(stepsPerInterval$steps), 1000)))
```

![](PA1_template_files/figure-html/comparison_daytypes-1.png)<!-- -->
