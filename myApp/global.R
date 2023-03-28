library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(mapview)
library(leaflet)
library(zoo)

df = read.csv('testing.csv') %>% filter(Defining.Parameter == 'PM2.5' | Defining.Parameter == 'Ozone')
df$Date = as.POSIXct(df$Date)
df$month = month(df$Date)
df$year = year(df$Date)


## Given average AQI, assigns value of categories

assign_category <- function(dataframe) {
  # Define AQI breakpoints and corresponding categories
  breakpoints <- c(0, 50, 100, 150, 200, 300, 500)
  categories <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy",
                  "Very Unhealthy", "Hazardous")
  
  # Use cut function to assign category based on AQI value
  dataframe$Category <- cut(dataframe$avg, breakpoints, labels = categories, include.lowest = TRUE)
  
  # Return the updated dataframe
  return(dataframe)
}

## Given dataframe with lat, lng, AQI score (Category), creates a map
create_map <- function(dataframe) {
  # Create a leaflet map
  m <- leaflet() %>%
    addTiles()
  
  # Define a color palette for the categories
  color_palette <- colorFactor(c("green", "yellow", "orange", "red", "purple", "black"),
                               levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
                                          "Unhealthy","Very Unhealthy", "Hazardous"))
  
  # Construct circle markers with colors based on quality column
  markers <- data.frame(lng = dataframe$lng,
                        lat = dataframe$lat,
                        color = color_palette(dataframe$Category),
                        radius = 4,
                        popup = paste0(dataframe$city, ", ", dataframe$state, 
                                       "<br> Measurement: ", dataframe$Defining.Parameter,
                                       "<br> Air Quality: ", dataframe$Category,
                                       "<br> Average reported AQI: ", dataframe$avg,
                                       "<br> Median reported AQI: ", dataframe$median,
                                       "<br> Minimum reported AQI: ", dataframe$min,
                                       "<br> Maximum reported AQI: ", dataframe$max,
                                       "<br> Number of Days Reported: ", dataframe$count
                                       )       
                        )
  m <- addCircleMarkers(m, data = markers, lng = ~lng, lat = ~lat, color = ~color, radius = ~radius, popup = ~popup)
  
  # Calculate bounding box of markers and set view accordingly
  #m <- fitBounds(m)
  
  # Return the map
  return(m)
}