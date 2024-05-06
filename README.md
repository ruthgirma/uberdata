# Uberdata analysis  
# By: Ruth Girma  
<img src = "Images/uber-one.webp" height = 300, width = 500>  

# Introduction
In this project, I aim to analyze Uber trip data to uncover patterns and insights regarding trip frequencies, temporal trends, and geographical distribution. By leveraging various data visualization techniques, including pivot tables, bar charts, heatmaps, and interactive maps through a Shiny application, I provide an insightful exploration of Uber trip data across different dimensions such as time of day, day of the week, month, and geographical location.

# Data cleaning
1. I read all the CSV data, combined them, and saved the data in RDS since it is a large data and it is hard to read everything 
```
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
```
2. Convert the date column
In this section, the date column was in this format "2024-11-09 00:01:20". I created different columns for the date, time, hour, day, and month. 
```
combined_uber_data$Hour <- hour(combined_uber_data$Date.Time)
combined_uber_data$Month <- month.name[month(combined_uber_data$Date.Time)]
combined_uber_data$Day <- day(combined_uber_data$Date.Time)
combined_uber_data$Weekday <- weekdays(combined_uber_data$Date.Time)

```
# Filtering and creating a pivot table
1. Pivot table to display trips by the hour

In this part, I created a pivot table that displays the number of trips by the hour.
```
pivot_table_trips_hr <- combined_uber_data %>%
  group_by(Hour) %>%
  summarize(Trips = n())
```
2. Chart that shows Trips by Hour and Month

In this section, I created a pivot table and a chart that shows the number of trips grouped by hour and month. Since the dataset was very large I saved my charts as RDS so that it wouldn't be hard for me to publish it on the shiny app.
```
pivot_table_trips_hr_month <- combined_uber_data %>%
  group_by(Hour, Month) %>%
  summarize(Trips = n())

ggplot(pivot_table_trips_hr_month, aes(x = Hour, y = Trips, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Number of Trips", fill = "Hour")

saveRDS(pivot_table_trips_hr_month, file = "Chart that shows Trips by Hour and Month.rds")
```
# Heat maps
One of the things needed for this project was creating heat maps. 

1. Heat map that displays by hour and day
For the heat maps, I first created a pivot table that displays the number of trips by hour and day. I also saved the heatmap as an RDS file to save space.
```
heatmap_hour_day <- combined_uber_data %>%
  group_by(Hour, Day) %>%
  summarise(trips = n())

ggplot(heatmap_hour_day, aes(x = Hour, y = Day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Hour", y = "Day of Month", fill = "Number of trips")

saveRDS(heatmap_hour_day, file = "Heatmap of trips by hour and day.rds")
```
2. Heat map by month and day
For the heat maps, I first created a pivot table that displays the number of trips by month and day. I also saved the heatmap as an RDS file to save space.

```
heatmap_month_day <- combined_uber_data %>%
  group_by(Month, Day) %>%
  summarise(trips = n())

ggplot(heatmap_month_day, aes(x = Day, y = Month, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Day of Month", y = "Month", fill = "Number of trips")

saveRDS(heatmap_month_day, file = "Heatmap of trips by month and day.rds")

```
3. Heat map by month and week
For the heat maps, I first created a pivot table that displays the number of trips by month and week. I also saved the heatmap as an RDS file to save space.
```
heatmap_month_week <- combined_uber_data %>%
  group_by(Month, Weekday) %>%
  summarise(trips = n())

ggplot(heatmap_month_week, aes(x = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                               y = Month, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Weekday", y = "Month", fill = "Number of trips") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(heatmap_month_week, file = "Heatmap of trips by month and week.rds")

```
4. Heat map Bases and Day of the Week
For the heat maps, I first created a pivot table that displays the number of trips by Base and day. I also saved the heatmap as an RDS file to save space.

```
heatmap_bases_day <- combined_uber_data %>%
  group_by(Base, Day) %>%
  summarise(trips = n())

ggplot(heatmap_bases_day, aes(x = Base, y = Day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Base", y = "Day of Month", fill = "Number of trips") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(heatmap_bases_day, file = "Heatmap of trips by Base and day.rds")

```
# Geospatial mapping
1. Load all the necessary libraries to complete the geospatial mapping
```
library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
```
2. Define the UI
```
ui <- fluidPage(
  # Use shinyjs to reset map
  useShinyjs(),
  extendShinyjs(text = "shinyjs.resetMap = function() { map.setView([40.75, -73.98], 10); }", functions = list(
    resetMap = JS("function() { map.setView([40.75, -73.98], 10); }")
  )),

  # Reset map button and measure button
  tags$div(
    id = "buttons",
    actionButton("reset", "Reset Map"),
    actionButton("measure", "Measure Distance")
  ),
  
  # Leaflet map and info box
  mainPanel(
    leafletOutput("map"),
    verbatimTextOutput("info")
  )
)
```
3. Define the server
```
server <- function(input, output, session) {
  # Initialize leaflet map with default view
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.98, lat = 40.75, zoom = 10)
  })
  
  # Assuming your large dataset is stored in 'large_data'
  # Subset the first 50 observations for demonstration
  observe({
    subset_data <- combined_uber_data[1:50, ]
    
    # Add markers to map
    leafletProxy("map", data = subset_data) %>%
      clearMarkers() %>%
      addMarkers(lng = ~Lon, lat = ~Lat, popup = ~Base)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
```

# Github link
[https://ruthgirma.shinyapps.io/UberProjectRideAnalysis/]
