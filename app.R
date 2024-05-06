library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidytext)
library(textdata) 
library(janeaustenr)
library(dplyr)
library(stringr)
library(reshape2)
library(data.table)
library(rsconnect)
library(forcats)
library(shinyjs)
library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
library(shiny)
library(leaflet)


trips_by_base_and_month <- readRDS("Chart Trips by Base and Month.rds")
trips_by_day_and_month <- readRDS("Chart by Trips by Day and Month.rds")
trips_every_day <- readRDS("Plot data by trips taken during every day of the month.rds")
trips_every_hour <- readRDS("Chart that displays Trips Every Hour.rds")
pivot_table_trips_hr_month <- readRDS("Chart that shows Trips by Hour and Month.rds")

heatmap_bases_day <- readRDS("Heatmap of trips by Base and day.rds")
heatmap_hour_day <- readRDS("Heatmap of trips by hour and day.rds")
heatmap_month_day <- readRDS("Heatmap of trips by month and day.rds")
heatmap_month_week <- readRDS("Heatmap of trips by month and week.rds")

combined_uber_data <- readRDS("combined_uber_data.rds")
# Define UI
ui <- fluidPage(
  titlePanel("Uber Rides Data Analysis"),
  
  # Tabs
  tabsetPanel(
    tabPanel("Geospatial Mapping",
             titlePanel("Geospatial Mapping"),
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
             )),
    tabPanel("Trips by Base and Month",
             titlePanel("Trips by Base and Month"),
             plotOutput("plot1"),
             p("This graph illustrates the distribution of Uber trips across different bases (locations) over the months. 
                Each bar represents the number of trips originating from a specific base, grouped by month, allowing users to identify which bases are more active during particular months.")),
    tabPanel("Trips by Day and Month",
             titlePanel("Trips by Day and Month"),
             plotOutput("plot2"),
             p("This graph displays the number of Uber trips taken on each day of the month, categorized by month. 
               By observing the bars, users can discern patterns and trends in trip volume throughout the month, 
               helping them understand the variations in demand over time.")),
    tabPanel("Plot data by trips taken during every day of the month",
             titlePanel("Plot data by trips taken during every day of the month"),
             plotOutput("plot3"),
             p("This graph presents a breakdown of Uber trips taken on each day of the month, providing insights into daily trip patterns. 
                Users can analyze the distribution of trips across different days to identify any recurring patterns or anomalies.")),
    tabPanel("Chart that displays Trips Every Hour",
             titlePanel("Chart that displays Trips Every Hour"),
             plotOutput("plot4"),
             p("This graph showcases the distribution of Uber trips by hour of the day. 
                By visualizing trip volume over the course of 24 hours, users can identify peak hours of activity and understand how trip demand varies throughout the day.")),
    tabPanel("Chart that shows Trips by Hour and Month",
             titlePanel("Chart that shows Trips by Hour and Month"),
             plotOutput("plot5"),
             p("This graph presents Uber trip data aggregated by hour of the day and month. 
                Users can analyze how trip volume fluctuates across different hours and months, 
                identifying any temporal patterns or trends in ride demand.")),
    tabPanel("Heatmap of trips by Base and day",
             titlePanel("Heatmap of trips by Base and day"),
             plotOutput("plot6"),
             p("This heatmap visualizes Uber trip data based on bases and days of the month. 
                The color intensity represents the number of trips, allowing users to quickly identify the busiest days for each base and understand the spatial-temporal distribution of trip activity.")),
    tabPanel("Heatmap of trips by hour and day",
             titlePanel("Heatmap of trips by hour and day"),
             plotOutput("plot7"),
             p("This heatmap provides a comprehensive view of Uber trip volume by month and day of the month. 
                Users can analyze how trip volume changes over different months and days, 
                facilitating the identification of temporal patterns and seasonal trends in ride demand.")),
    tabPanel("Heatmap of trips by month and day",
             titlePanel("Heatmap of trips by month and day"),
             plotOutput("plot8"),
             p("This heatmap visualizes Uber trip volume by month and day of the week. 
                  Users can explore how trip activity varies across different months and weekdays, 
                  helping them understand weekly patterns in ride demand.")),
    tabPanel("Heatmap of trips by month and week",
             titlePanel("Heatmap of trips by month and week"),
             plotOutput("plot9"),
             p("This heatmap visualizes Uber trip data based on bases and days of the month. 
              The color intensity represents the number of trips, allowing users to quickly identify the busiest days for each base and understand the spatial-temporal distribution of trip activity."))    
  )
)


# Define server logic
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    ggplot(trips_by_base_and_month, aes(x = Base, y = Trips, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Base and Month",
           x = "Base",
           y = "Number of Trips",
           fill = "Month") +
      theme_minimal()
  })
  
  output$plot2 <- renderPlot({
    ggplot(trips_by_day_and_month, aes(x = Month, y = trips, fill = Day)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Day and Month",
           x = "Month",
           y = "Number of Trips",
           fill = "Day") +
      theme_minimal()
  })
  
  output$plot3 <- renderPlot({
    ggplot(trips_every_day, aes(x = Day, y = trips)) +
      geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
      labs(title = "Plot data by trips taken during every day of the month",
           x = "Day of the Month",
           y = "Number of Trips") +
      theme_minimal()
  })
  
  output$plot4 <- renderPlot({
    ggplot(trips_every_hour, aes(x = Hour, y = Trips)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(title = "Chart that displays Trips Every Hour",
           x = "Hour of the Day",
           y = "Number of Trips") +
      theme_minimal()
  })
  
  output$plot5 <- renderPlot({
    ggplot(pivot_table_trips_hr_month, aes(x = Hour, y = Trips, color = Month)) +
      geom_bar(stat = "identity") +
      labs(title = "Chart that shows Trips by Hour and Month",
           x = "Hour of the Day",
           y = "Number of Trips",
           color = "Month") +
      theme_minimal()
  })
  
  output$plot6 <- renderPlot({
    ggplot(heatmap_hour_day, aes(x = Hour, y = Day, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "Hour", y = "Day of Month", fill = "Number of trips") +
      theme_minimal()
  })
  
  output$plot7 <- renderPlot({
    ggplot(heatmap_month_day, aes(x = Month, y = Day, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "Month", y = "Day of Month", fill = "Number of trips") +
      theme_minimal()
  })
  
  output$plot8 <- renderPlot({
    ggplot(heatmap_month_week, aes(x = Weekday, y = Month, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "Weekday", y = "Month", fill = "Number of trips") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot9 <- renderPlot({
    ggplot(heatmap_bases_day, aes(x = Base, y = Day, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "Base", y = "Day of Month", fill = "Number of trips") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
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
