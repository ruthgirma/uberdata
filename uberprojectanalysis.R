library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidytext)
library(textdata) 
library(dplyr)
library(stringr)
library(reshape2)
library(data.table)
library(rsconnect)
library(forcats)
library(ggplot2)
library(dplyr)

# Setting working directory
#setwd("~/Desktop/uber new assignment/app.R")
#project_directory <- "~/Desktop/uber new assignment/app.R"

# Read CSV files
uber_data_sep_14 <- read.csv("uber-raw-data-sep14.csv")
uber_data_may_14 <- read.csv("uber-raw-data-may14.csv")
uber_data_jun_14 <- read.csv("uber-raw-data-jun14.csv")
uber_data_aug_14 <- read.csv("uber-raw-data-aug14.csv")
uber_data_jul_14 <- read.csv("uber-raw-data-jul14.csv")
uber_data_apr_14 <- read.csv("uber-raw-data-apr14.csv")

# Combine data
combined_uber_data <- rbind(uber_data_sep_14, uber_data_may_14, uber_data_jun_14,
                            uber_data_aug_14, uber_data_jul_14, uber_data_apr_14)

# Create and save RDS file
saveRDS(combined_uber_data, file = "combined_uber_data.rds")

combined_uber_data$Date.Time <- as.POSIXct(combined_uber_data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

#Create a new column for hour
combined_uber_data$Hour <- hour(combined_uber_data$Date.Time)

#Create a new column for month and change the format from numbers to the month names
combined_uber_data$Month <- month.name[month(combined_uber_data$Date.Time)]

# Create a new column for day of the month
combined_uber_data$Day <- day(combined_uber_data$Date.Time)

# Create a new column for day of the week
combined_uber_data$Weekday <- weekdays(combined_uber_data$Date.Time)

# Load your combined Uber data here
# combined_uber_data <- read.csv("combined_uber_data.csv")

# Pivot table to display trips by the hour
pivot_table_trips_hr <- combined_uber_data %>%
  group_by(Hour) %>%
  summarize(Trips = n())

# Chart that shows Trips by Hour and Month
pivot_table_trips_hr_month <- combined_uber_data %>%
  group_by(Hour, Month) %>%
  summarize(Trips = n())

# Chart that shows Trips by Hour and Month
ggplot(pivot_table_trips_hr_month, aes(x = Hour, y = Trips, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Number of Trips", fill = "Hour")

saveRDS(pivot_table_trips_hr_month, file = "Chart that shows Trips by Hour and Month.rds")


# Chart that displays Trips Every Hour
trips_every_hour <- combined_uber_data %>%
  group_by(Hour) %>%
  summarize(Trips = n())

# Chart that displays Trips Every Hour
ggplot(trips_every_hour, aes(x = Hour, y = Trips)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Trips every hour",
       x = "Hour", 
       y = "Number of Trips") +
  theme_minimal()

saveRDS(trips_every_hour, file = "Chart that displays Trips Every Hour.rds")


# Plot data by trips taken during every day of the month
trips_every_day <- combined_uber_data %>%
  group_by(Day) %>%
  summarize(trips = n())

# Plot data by trips taken during every day of the month
ggplot(trips_every_day, aes(x = Day, y = trips)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  labs(x = "Day of Month", y = "Number of Trips")

saveRDS(trips_every_day, file = "Plot data by trips taken during every day of the month.rds")


# Chart by Trips by Day and Month
trips_by_day_and_month <- combined_uber_data %>%
  group_by(Day, Month) %>%
  summarise(trips = n())

ggplot(trips_by_day_and_month, aes(x = Month, y = trips, fill = Day)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Trips", fill = "Day of the Week", title = "Number of Uber Trips by Day and Month") +
  theme_bw()

saveRDS(trips_by_day_and_month, file = "Chart by Trips by Day and Month.rds")


# Chart that shows number of trips by month
trips_by_month <- combined_uber_data %>%
  group_by(Month) %>%
  summarize(Trips = n())


# pivot table of Trips by Base and Month
trips_by_base_and_month <- combined_uber_data %>%
  group_by(Base, Month) %>%
  summarize(Trips = n())

# Chart Trips by Base and Month
ggplot(trips_by_base_and_month, aes(x = Month, y = Trips, fill = Base)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Trips", fill = "Base")

saveRDS(trips_by_base_and_month, file = "Chart Trips by Base and Month.rds")

# Heatmap: Hour and Day
heatmap_hour_day <- combined_uber_data %>%
  group_by(Hour, Day) %>%
  summarise(trips = n())

ggplot(heatmap_hour_day, aes(x = Hour, y = Day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Hour", y = "Day of Month", fill = "Number of trips")

saveRDS(heatmap_hour_day, file = "Heatmap of trips by hour and day.rds")

# Pivot Table: Heatmap by Month and Day
heatmap_month_day <- combined_uber_data %>%
  group_by(Month, Day) %>%
  summarise(trips = n())

# Heatmap: Month and Day
ggplot(heatmap_month_day, aes(x = Day, y = Month, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Day of Month", y = "Month", fill = "Number of trips")

saveRDS(heatmap_month_day, file = "Heatmap of trips by month and day.rds")

# Pivot Table: Heatmap by Month and Week
heatmap_month_week <- combined_uber_data %>%
  group_by(Month, Weekday) %>%
  summarise(trips = n())

# Heatmap: Month and Week
ggplot(heatmap_month_week, aes(x = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                               y = Month, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Weekday", y = "Month", fill = "Number of trips") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(heatmap_month_week, file = "Heatmap of trips by month and week.rds")

# Heat map: bases and day
heatmap_bases_day <- combined_uber_data %>%
  group_by(Base, Day) %>%
  summarise(trips = n())

# Heatmap: Bases and Day
ggplot(heatmap_bases_day, aes(x = Base, y = Day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Base", y = "Day of Month", fill = "Number of trips") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(heatmap_bases_day, file = "Heatmap of trips by Base and day.rds")






