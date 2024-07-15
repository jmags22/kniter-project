#load data
activity_data <- read.csv("activity.csv")

#caluclate total number of steps each day
total_steps_per_day <- activity_data %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE))

#create histogram of total number of steps each day
ggplot(total_steps_per_day, aes(x = total_steps)) +
        geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
        labs(title = "Total Number of Steps Taken Each Day", x = "Total Steps", y = "Frequency")

#calculate the mean and median
mean_steps <- mean(total_steps_per_day$total_steps)
median_steps <- median(total_steps_per_day$total_steps)

#calculate average number of steps per 5-min interval across all days
average_daily_activity <- activity_data %>%
        group_by(interval) %>%
        summarize(average_steps = mean(steps, na.rm = TRUE))

#plot the time series
ggplot(average_daily_activity, aes(x = interval, y = average_steps)) +
        geom_line(color = "blue") +
        labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Average Steps")

#identify interval with the maximum average steps
max_interval <- average_daily_activity[which.max(average_daily_activity$average_steps), ]

#count total NA values
na_count <- sum(is.na(activity_data$steps))

#impute the missing values
imputed_data <- activity_data
for(i in unique(imputed_data$interval[is.na(imputed_data$steps)])) {
        imputed_data$steps[is.na(imputed_data$steps) & imputed_data$interval == i] <- mean(imputed_data$steps[imputed_data$interval == i], na.rm = TRUE)
}

#repeat the histogram of mean/median calucaltion for the imputed data
ggplot(total_steps_per_day, aes(x = total_steps)) +
        geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
        labs(title = "Total Number of Steps Taken Each Day", x = "Total Steps", y = "Frequency")

#create new factor variable
imputed_data$date <- as.Date(imputed_data$date)
imputed_data$day_type <- ifelse(weekdays(imputed_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

#create panel plot
average_steps_by_day_type <- imputed_data %>%
        group_by(interval, day_type) %>%
        summarize(average_steps = mean(steps))

ggplot(average_steps_by_day_type, aes(x = interval, y = average_steps, color = day_type)) +
        geom_line() +
        facet_wrap(~day_type, scales = "free_y") +
        labs(title = "Activity Patterns on Weekdays vs. Weekends", x = "Interval", y = "Average Steps")


