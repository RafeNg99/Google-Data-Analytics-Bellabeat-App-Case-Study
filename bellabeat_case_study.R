#Import libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)

#Import datasets
dailyactivity_df <- read_csv("dailyActivity_merged.csv")
sleepday_df <- read.csv("sleepDay_merged.csv")
hourlysteps_df <- read.csv("hourlySteps_merged.csv")

#Summary of the datasets
head(dailyactivity_df)
str(dailyactivity_df)
colnames(dailyactivity_df)
summary(dailyactivity_df)
n_distinct(dailyactivity_df$Id)

head(sleepday_df)
str(sleepday_df)
colnames(sleepday_df)
summary(sleepday_df)
n_distinct(sleepday_df$Id)

head(hourlysteps_df)
str(hourlysteps_df)
colnames(hourlysteps_df)
summary(hourlysteps_df)
n_distinct(hourlysteps_df$Id)

#Process Phase
sum(is.na(dailyactivity_df))
dailyactivity_df$ActivityDate <- mdy(dailyactivity_df$ActivityDate)

dailyactivity_df <- rename(dailyactivity_df, Date = ActivityDate)

sum(is.na(sleepday_df))
sleepday_df$SleepDay <- mdy_hms(sleepday_df$SleepDay)
sleepday_df <- rename(sleepday_df, Date = SleepDay)

merged_df <- merge(sleepday_df, dailyactivity_df, by = c('Id', 'Date'))
merged_df <- add_column(merged_df, ActivityDay = weekdays(merged_df$Date), 
                        .after = "Id")

merged_df$ActivityDay <-ordered(merged_df$ActivityDay, 
                                      levels = c("Monday", "Tuesday", 
                                               "Wednesday", "Thursday",
                                               "Friday", "Saturday", "Sunday"))

weekday_activity <- merged_df %>%
  group_by(ActivityDay) %>%
  summarize (daily_steps = mean(TotalSteps), daily_sleep = mean(TotalMinutesAsleep),
             daily_calories = mean(Calories), daily_distance = mean(TotalDistance))

user_activity <- merged_df %>% 
  group_by(Id) %>%
  summarize (daily_steps = mean(TotalSteps), daily_sleep = mean(TotalMinutesAsleep),
             daily_calories = mean(Calories), daily_distance = mean(TotalDistance))

#https://www.10000steps.org.au/articles/healthy-lifestyles/counting-steps/
user_type <- user_activity %>% 
  mutate(user_type = case_when(
    daily_steps < 5000 ~ "Sedentary",
    daily_steps >= 5000 & daily_steps <= 7499 ~ "Low Active",
    daily_steps >= 7500 & daily_steps <= 9999 ~ "Somewhat Active",
    daily_steps >= 10000 & daily_steps <= 12499 ~ "Active",
    daily_steps >= 12500 ~ "Highly Active"
  ))

user_type_percent <- user_type %>% 
  group_by(user_type) %>% 
  summarise(Total = n(), Total_Percent = Total/24) %>% 
  mutate(labels = scales::percent(Total_Percent))
  
user_type_percent$user_type <- ordered(user_type_percent$user_type,
                                      levels = c("Highly Active", "Active", 
                                                "Somewhat Active", 
                                                "Low Active", "Sedentary"))

sum(is.na(hourlysteps_df))
hourlysteps_df <- hourlysteps_df %>% 
  rename(date_time = ActivityHour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p", 
                                tz=Sys.timezone()))

hourlysteps_df <- hourlysteps_df %>% 
  separate(date_time, into = c("Date", "Time"), sep = " ") %>% 
  mutate(Date = ymd(Date))

hourly_steps <- hourlysteps_df %>% group_by(Time) %>% 
  summarise(mean_steps = mean(StepTotal))

user_usage <- merged_df %>% 
  group_by(Id) %>% summarise(app_usage_days = sum(n())) %>% 
  mutate(usage = case_when(
    app_usage_days >= 1 & app_usage_days <= 10 ~ "Low Usage",
    app_usage_days >= 11 & app_usage_days <= 20 ~ "Moderate Usage",
    app_usage_days >= 21 ~ "High Usage"
  ))

user_usage_percent <- user_usage %>% 
  group_by(usage) %>% 
  summarise(Total = n(), Total_Percent = Total/24) %>% 
  mutate(labels = scales::percent(Total_Percent))

user_usage_percent$usage <- ordered(user_usage_percent$usage,
                                       levels = c("High Usage", "Moderate Usage", 
                                                  "Low Usage"))

#Visualization phase

#FitBit App Usage Across The Week #fct_infreq()
ggplot(merged_df, aes(x = ActivityDay, fill = ActivityDay)) + 
  geom_bar() + theme(axis.text.x = element_text(angle = 25), 
                     plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "FitBit App Usage Across The Week", 
       x = "Days of Activity", y = "Frequency", fill = "Days")


#Calories Burnt vs. Total Steps Taken
ggplot(merged_df, aes(x = TotalSteps, y = Calories)) + 
  geom_point(color = "red") + geom_smooth() + 
  geom_vline(xintercept = mean(merged_df$TotalSteps), color = "darkgoldenrod3") + 
  geom_hline(yintercept = mean(merged_df$Calories), color = "purple") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Calories Burnt vs. Total Steps Taken", x = "Total Steps Taken",
       y = "Calories Burnt") + 
  annotate("text", x = 12500, y = 500, label = "Average Steps Taken", 
           color = "darkgoldenrod3", size = 3) + 
  annotate("text", x = 21000, y = 2300, label = "Average Calories Burnt", 
           color = "purple", size = 3)


#Calories Burnt vs. Total Distance Traveled
ggplot(merged_df, aes(x = TotalDistance, y = Calories)) + 
  geom_point(color = "darkorchid1") + geom_smooth() + 
  geom_vline(xintercept = mean(merged_df$TotalDistance), color = "darkgoldenrod3") + 
  geom_hline(yintercept = mean(merged_df$Calories), color = "red") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Calories Burnt vs. Total Distance Traveled", 
       x = "Total Distance Traveled", y = "Calories Burnt") + 
  annotate("text", x = 11, y = 500, label = "Average Distance Traveled", 
           color = "darkgoldenrod3", size = 3) + 
  annotate("text", x = 20, y = 2500, label = "Average Calories Burnt", 
           color = "red", size = 3)


#Total Distance Traveled vs. Total Steps Taken
ggplot(merged_df, aes(x = TotalSteps, y = TotalDistance)) + 
  geom_point(color = "orange") + geom_smooth(method = "lm") + 
  geom_vline(xintercept = mean(merged_df$TotalSteps), color = "red") + 
  geom_hline(yintercept = mean(merged_df$TotalDistance), color = "purple") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Total Distance Traveled vs. Total Steps Taken", 
       x = "Total Steps Taken", y = "Total Distance Traveled") + 
  annotate("text", x = 25000, y = 7, label = "Average Distance Traveled", 
           color = "purple", size = 3) + 
  annotate("text", x = 12500, y = 0, label = "Average Steps Taken", 
           color = "red", size = 3)


#Average Minutes Asleep vs. Days
ggplot(weekday_activity, aes(x = ActivityDay, y = daily_sleep, fill = ActivityDay)) + 
  geom_col() + theme(axis.text.x = element_text(angle = 25), 
                     plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept = 480) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Average Minutes Asleep vs. Days", 
       x = "Days", y = "Average Minutes Asleep", fill = "Days") +
  annotate("text", x = "Thursday", y = 500, 
           label = "Recommended Amount of Sleep in Minutes", size = 3)


#Average Steps Taken vs. Days
ggplot(weekday_activity, aes(x = ActivityDay, y = daily_steps, fill = ActivityDay)) + 
  geom_col() + theme(axis.text.x = element_text(angle = 25), 
                     plot.title = element_text(hjust = 0.5)) + 
  geom_hline(yintercept = 7500) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Average Steps Taken vs. Days", 
       x = "Days", y = "Average Steps Taken", fill = "Days") +
  annotate("text", x = "Thursday", y = 7800, 
           label = "Recommended Amount of Steps Taken Daily", size = 3)


#User Type Distribution Based On Daily Steps Taken
ggplot(user_type_percent, aes(x = "", y = Total_Percent, fill = user_type)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + 
  theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                          panel.border = element_blank(), panel.grid = element_blank(),
                          axis.ticks = element_blank(), axis.text.x = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5), size = 3) + 
  labs(title = "User Type Distribution Based On Daily Steps Taken", 
       fill = "User Type") 


#Total Minutes Asleep vs. Total Steps Taken
ggplot(merged_df, aes(x = TotalSteps, y = TotalMinutesAsleep)) + 
  geom_point(color = "red") + geom_smooth() + 
  geom_vline(xintercept = mean(merged_df$TotalSteps), color = "darkgoldenrod3") + 
  geom_hline(yintercept = mean(merged_df$TotalMinutesAsleep), color = "purple") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  labs(title = "Total Minutes Asleep vs. Total Steps Taken", x = "Total Steps Taken",
       y = "Total Minutes Asleep") + 
  annotate("text", x = 11500, y = 800, label = "Average Steps Taken", 
           color = "darkgoldenrod3", size = 3) + 
  annotate("text", x = 19500, y = 440, label = "Average Minutes Asleep", 
           color = "purple", size = 3)


#Hourly Average Steps Throughout The Day
ggplot(hourly_steps, aes(x = Time, y = mean_steps, fill = mean_steps)) + 
  geom_col() + scale_fill_gradient(low = "red", high = "green") + 
  theme(axis.text.x = element_text(angle = 90, size = 5), 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  labs(title = "Hourly Average Steps Throughout The Day", x = "Time", 
       y = "Average Steps", fill = "Average Steps")


#User Usage Distribution Based On Number of Days
ggplot(user_usage_percent, aes(x = "", y = Total_Percent, fill = usage)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + 
  theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                          panel.border = element_blank(), panel.grid = element_blank(),
                          axis.ticks = element_blank(), axis.text.x = element_blank(),
                          plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5), size = 3) + 
  labs(title = "User Usage Distribution Based On Number of Days", fill = "Usage") 
