#Loading and preprocessing the data
unzip(zipfile = "repdata-data-activity.zip")

data <- read.csv("activity.csv")

#What is the mean total number of steps taken per day?
require(ggplot2)

total.steps <- aggregate(data$steps,list(data$date),sum,na.rm = TRUE)

names(total.steps) <- c("date","steps")

ggplot(total.steps, aes(x=steps)) + geom_histogram(binwidth=1000,colour = "white",fill = "blue") + 
    geom_vline(aes(xintercept=mean(steps)),color="red",linetype="dashed",size=1) + 
    geom_vline(aes(xintercept=median(steps)),color="green",linetype="dashed",size=1,) + 
    labs(title = "Steps per Day") +
    geom_text(aes(mean(steps),9,label = "Mean",angle = 90,vjust = -.5)) +
    geom_text(aes(median(steps),4,label = "Median",angle = 270,vjust = -.5,colour = "red"))+
    theme(legend.position="none")

mean(total.steps$steps)

median(total.steps$steps)

#What is the average daily activity pattern?
interval.steps <- aggregate(data$steps, list(interval = data$interval), sum, na.rm = TRUE)

names(interval.steps) <- c("interval","steps")

ggplot(interval.steps,aes(interval,steps)) + geom_line() + geom_vline(aes(xintercept = interval[steps == max(steps)]),colour = "red", linetype = "dashed")

interval.steps$interval[interval.steps$steps == max(interval.steps$steps)]

#Imputing missing values
sum(is.na(data$steps))

steps.ave <- aggregate(data$steps,list(interval = data$interval),mean,na.rm = TRUE)

data$steps.ave <- c(rep(steps.ave$x,times = length(unique(data$date))))

data$steps[is.na(data$steps)] <- data$steps.ave[is.na(data$steps)]

total.steps <- aggregate(data$steps,list(data$date),sum)

names(total.steps) <- c("date","steps")

ggplot(total.steps, aes(x=steps)) + geom_histogram(binwidth=1000,colour = "white",fill = "blue") + 
    geom_vline(aes(xintercept=mean(steps)),color="red",linetype="dashed",size=1) + 
    geom_vline(aes(xintercept=median(steps)),color="green",linetype="dashed",size=1,) + 
    labs(title = "Steps per Day") +
    geom_text(aes(mean(steps),9,label = "Mean",angle = 90,vjust = -.5)) +
    geom_text(aes(median(steps),4,label = "Median",angle = 270,vjust = -.5,colour = "red"))+
    theme(legend.position="none")

mean(total.steps$steps)

median(total.steps$steps)

#Are there differences in activity patterns between weekdays and weekends?
data <- read.csv("activity.csv")

data$days <- weekdays(as.Date(data$date))

data$days[which(data$days != "Saturday" & data$days != "Sunday")] <- "weekday"

data$days[which(data$days == "Saturday" | data$days == "Sunday")] <- "weekend"

interval.steps <- aggregate(data$steps, list(data$interval,data$days), mean, na.rm = TRUE)

names(interval.steps) <- c("interval","days","steps")

ggplot(interval.steps,aes(interval,steps)) + geom_line() + 
      geom_vline(aes(xintercept = interval[steps == max(steps)]),colour = "red", linetype = "dashed") + 
      facet_wrap(~days,nrow=2) + theme_bw()
