
library(knitr)
library(dplyr)
library(lattice)

# Download and unzip the dataset
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
unzip("activity.zip")
file.remove("activity.zip")

# Load the dataset
data <- read.csv("activity.csv")
str(data)

# The values of the _date_ variable are provided as characters, so we convert them to dates.
data$date <- as.Date(data$date, "%Y-%m-%d")

# Calculate the total number of steps per day (ignoring NA values)
steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)
head(steps_per_day)

# Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps, 
            breaks = 20,
            col = "lightblue",
            xlab = "Steps per day",
            main = "Histogram of total number of steps taken each day")

# Calculate and report the mean and median of the total number of steps taken per day
summary(steps_per_day)

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
steps_per_interval_means <- data %>%
                           group_by(interval) %>%
                           summarise(steps = round(mean(steps, na.rm = TRUE)))

plot(steps ~ interval,
        data = steps_per_interval_means,
        type = "l",
        col = "blue",
        xlab = "Elapsed 5-minute intervals",
        ylab = "Number of steps", 
        main = "Average number of steps across all days")

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
max_index <- which.max(steps_per_interval_means$steps)
print(steps_per_interval_means[max_index, ]$interval)

# Calculate the time of the day that corresponds to this interval
first_835 = which(data$interval == 835)[1]
elapsed = first_835 * 5
paste(floor(elapsed/60), ":", elapsed %% 60, sep = '')

# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
summary(data)

# Calculate the percentage of NA values of the _steps_ variable for each interval,
# averaged across all days, and run some statistics on these percentages.
data <- data %>%
        group_by(interval) %>%
        mutate(steps_NA_percent = mean(is.na(steps)))

summary(data$steps_NA_percent)

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
data <- data %>%
        group_by(interval) %>%
        mutate(steps_no_NA = ifelse(is.na(steps), round(mean(steps, na.rm = TRUE)), steps))
               
data_no_NA = data.frame(steps = data$steps_no_NA, date = data$date, interval = data$interval)

summary(data_no_NA)

# Make a histogram of the total number of steps taken each day
steps_per_day_no_NA <- aggregate(steps ~ date, data_no_NA, sum, na.rm = TRUE)

hist(steps_per_day_no_NA$steps, 
        main = "Histograme of total number of steps taken each day w/o NA values",
        xlab = "Steps per day",
        col = "lightblue",
        breaks = 20)

# Calculate and report the mean and median total number of steps taken per day.
summary(steps_per_day)
summary(steps_per_day_no_NA)

# This is better seen on side by side barplots.
boxplot(steps_per_day$steps, steps_per_day_no_NA$steps,
        col = "light blue",
        names = c("With NA steps values", "Without NA steps values"),
        main = "Boxplots of total numbers of steps taken each day")

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
# indicating whether a given date is a weekday or weekend day.
data_no_NA <- data_no_NA %>%
              mutate(day = as.factor(
                              ifelse(weekdays(date) == "Saturday" | weekdays(date)== "Sunday",
                                     "weekend", "weekday")))

# Make a panel plot containing a time series plot (i.e. type= "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
mean_steps_per_day <- aggregate(steps ~ interval + day, data_no_NA, mean)
names(mean_steps_per_day) <- c("interval", "day", "steps")

xyplot(steps ~ interval | day, mean_steps_per_day, type = "l", layout = c(1, 2), 
       xlab = "Elapsed 5-minutes intervals",
       ylab = "Average number of steps across days",
       main = "Weekdays versus weekends")

