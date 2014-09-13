##Loading and preprocessing the data 
setwd('E:/Course/Johns hopkins_Data Science/Reproducible Research/Project1')
data<-read.csv('activity.csv')
names(data)


## What is mean total number of steps taken per day? 
mean_stpe_per_day<-aggregate(steps ~ date, data = data, mean)
names(mean_stpe_per_day)
##Make a plot of the total number of steps taken each day
with(mean_stpe_per_day,plot(date, steps, type = "l",main='Total number of steps taken per day') )
##Calculate and report the mean and median total number of steps taken per day
mean_stpe_per_day<-aggregate(steps ~ date, data = data, mean)

## What is the average daily activity pattern? 
## Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all days (y-axis)
mean_stpe_per_time<-aggregate(steps ~ interval, data = data, mean)
with(mean_stpe_per_time,plot(interval, steps, type = "l",xlab = "5-minute interval", ylab = "Steps",
                            main='The mean total number of steps taken') )

## Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
max_stpe_per_day<-aggregate(steps ~ date, data = data, max)
max_stpe_per_time<- merge(max_stpe_per_day,data,by=c("date","steps"),all.x = TRUE)
max_stpe_per_time<-cbind(max_stpe_per_time[1],max_stpe_per_time[3],max_stpe_per_time[2])
## Imputing missing values 
## Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
library(plyr)
total_NA<-count(data[is.na(data$steps),] , vars ='steps')

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
## For example, you could use the mean for that day, or the mean for that 5-minute interval, etc.
newdata<-cbind(data,newsetps=ifelse(is.na(data$steps),0,data$steps))
mean_stpe_per_time_newdata<-aggregate(newsetps ~ interval, data = newdata, mean)
with(mean_stpe_per_time_newdata,plot(interval, newsetps, type = "l",xlab = "5-minute interval", ylab = "Steps",
                             main='The mean total number of steps taken',
                             sub='NA is replaced by 0'))

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. How is the impact of imputing missing data on the estimates of the total daily number of steps?
mean_stpe_per_date_newdata<-aggregate(newsetps ~ date, data = newdata, mean)
with(mean_stpe_per_date_newdata,plot(date, newsetps, type = "l",xlab = "Date", ylab = "Steps",
                                     main='The mean total number of steps taken',
                                     sub='NA is replaced by 0'))

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
##Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
par(mfrow = c(2, 2))
with(mean_stpe_per_day,plot(date, steps, type = "l",main='Total number of steps taken per day') )
with(mean_stpe_per_date_newdata,plot(date, newsetps, type = "l",xlab = "Date", ylab = "Steps",
                                     main='The mean total number of steps taken',
                                     sub='NA is replaced by 0'))
with(mean_stpe_per_time,plot(interval, steps, type = "l",xlab = "5-minute interval", ylab = "Steps",
                             main='The mean total number of steps taken') )
with(mean_stpe_per_time_newdata,plot(interval, newsetps, type = "l",xlab = "5-minute interval", ylab = "Steps",
                                     main='The mean total number of steps taken',
                                     sub='NA is replaced by 0'))

## Are there differences in activity patterns between weekdays and weekends? 
library("data.table")
library("lattice")
Sys.setlocale("LC_TIME", "English")
##Create a new variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
data$date<- as.Date(data$date, "%Y-%m-%d") 
wkdaydb<-cbind(data,wkname=weekdays(data$date))
wkgroupdb<-cbind(wkdaydb,wkgroup=ifelse(grepl('Saturday|Sunday',wkdaydb$wkname),'weekend','weekday'))
wkdb<-rbind(cbind(aggregate(steps ~ interval, data = subset(wkgroupdb,wkgroup=='weekend'), mean),wkgroup='weekend'),
cbind(aggregate(steps ~ interval, data = subset(wkgroupdb,wkgroup=='weekday'), mean),wkgroup='weekday'))

##Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
##averaged across all weekday days or weekend days (y-axis). 
##The plot should look something like the following, which was creating using simulated data:
library(ggplot2)
qplot(interval,steps,data=wkdb,facets=.~wkgroup,main='The mean total number of steps taken')
