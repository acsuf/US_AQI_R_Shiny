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
month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
df$month <- month_names[df$month]

######### Time series ############################################

plot_aqi_city <- function(df, cities) {
  df <- df %>% filter(city %in% cities)
  ggplot(df, aes(x = Date, y = AQI, color = city)) +
    geom_line() +
    labs(title = "AQI Over Time by City",
         x = "Date",
         y = "AQI")
}

average_aqi_by_state <- function(df) {
  df %>% 
    group_by(state, Date) %>% 
    summarize(AQI = mean(AQI)) 
}

plot_aqi_state <- function(df, states){
  df <- average_aqi_by_state(df) %>% filter(state %in% states) 
  ggplot(df, aes(x = Date, y = AQI, color = state)) +
    geom_line() +
    labs(title = "AQI Over Time",
         x = "Date",
         y = "AQI")
}

group_cities_by_state <- function(df) {
  df %>% 
    group_by(state) %>% 
    summarize(city_list = list(unique(city)))
}

citydf = group_cities_by_state(df)

############# Compare ########################################### 
plot_aqi_state_boxplots_months <- function(df, state1, state2) {
  df_subset <- df %>% 
    filter(state == state1 | state == state2)
  ggplot(df_subset, aes(x = month, y = AQI, fill=month)) +
    geom_boxplot() +
    facet_wrap(~state, ncol=1) +
    labs(title = "AQI Boxplots by Month",
         x = "Month",
         y = "AQI")
}

plot_aqi_city_boxplots_months <- function(df, city1, city2) {
  df_subset <- df %>% 
    filter(city_full == city1 | city_full == city2)
  ggplot(df_subset, aes(x = month, y = AQI, fill=month)) +
    geom_boxplot() +
    facet_wrap(~city_full, ncol=1) +
    labs(title = "AQI Boxplots by Month",
         x = "Month",
         y = "AQI")
}

plot_aqi_state_boxplots_total <- function(df, state1, state2) {
  df_subset <- df %>% 
    filter(state == state1 | state == state2)
  ggplot(df_subset, aes(x = state, y = AQI, fill = state)) +
    geom_boxplot() +
    labs(title = "AQI Boxplots by Month",
         x = "State",
         y = "AQI")
}

plot_aqi_city_boxplots_total <- function(df, city1, city2) {
  df_subset <- df %>% 
    filter(city_full == city1 | city_full == city2)
  ggplot(df_subset, aes(x = city_full, y = AQI, fill = city_full)) +
    geom_boxplot() +
    labs(title = "AQI Boxplots by Month",
         x = "State",
         y = "AQI")
}


 ############# Scatter ##################################

plot_aqi_sc <- function(df) {
  ggplot(df, aes(x = density, y = AQI)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE)
}

### Bars ###


aqi_summary_city_all_df <- function(df, x) {
  df %>%
    group_by(city_full) %>%
    summarise(avg_aqi = mean(AQI),
              median_aqi = median(AQI),
              min_aqi = min(AQI),
              max_aqi = max(AQI),
              days_above_x = sum(AQI > x),
              total_days = n()) %>%
    mutate(above_x_pct = ifelse(total_days == 0, 0, days_above_x / total_days)) %>%
    ungroup()
}

aqi_summary_city_df <- function(df, x, states) {
  df %>%
    filter(state %in% states) %>%
    group_by(city_full) %>%
    summarise(avg_aqi = mean(AQI),
              median_aqi = median(AQI),
              min_aqi = min(AQI),
              max_aqi = max(AQI),
              days_above_x = sum(AQI > x),
              total_days = n()) %>%
    mutate(above_x_pct = ifelse(total_days == 0, 0, days_above_x / total_days)) %>%
    ungroup()
}
#### For bars

aqi_summary_state_df <- function(df, x) {
  df %>%
    group_by(state) %>%
    summarise(avg_aqi = mean(AQI),
              median_aqi = median(AQI),
              min_aqi = min(AQI),
              max_aqi = max(AQI),
              days_above_x = sum(AQI > x),
              total_days = n()) %>%
    mutate(above_x_pct = ifelse(total_days == 0, 0, days_above_x / total_days)) %>%
    ungroup()
}

aqi_summary_ordered <- function(summ_df, orientation, metric) {
  # Determine which column to use based on selection2
  if (metric == "Average") {
    colname <- "avg_aqi"
  } else if (metric== "Median") {
    colname <- "median_aqi"
  } else if (metric == "Minimum") {
    colname <- "min_aqi"
  } else if (metric == "Maximum") {
    colname <- 'max_aqi'
  } else if (metric == "Ratio of Bad Days") {
    colname <- "above_x_pct"
  }
  
  # Determine whether to order in ascending or descending order based on selection1
  if (orientation == "Best") {
    df_subset <- summ_df %>%
      arrange(!!sym(colname)) %>%
      head(5)
  } else if (orientation == "Worst") {
    df_subset <- summ_df %>%
      arrange(-!!sym(colname)) %>%
      head(5)
  }
  df_subset
}

aqi_city_barplot <- function(df, orientation, metric) {
  # Determine which column to use based on selection2
  if (metric == "Average") {
    colname <- "avg_aqi"
  } else if (metric == "Median") {
    colname <- "median_aqi"
  } else if (metric == "Minimum") {
    colname <- "min_aqi"
  } else if (metric == "Maximum") {
    colname <- "max_aqi"
  } else if (metric == "Ratio of Bad Days") {
    colname <- "above_x_pct"
  }
  # 
  # # Determine whether to order in ascending or descending order based on selection1
  # if (orientation == "Best") {
  #   df_subset <- df %>%
  #     arrange(!!sym(colname)) %>%
  #     head(5)
  # } else if (orientation == "Worst") {
  #   df_subset <- df %>%
  #     arrange(-!!sym(colname)) %>%
  #     head(5)
  # }

  # Create the plot
  ggplot(df, aes(x = city_full, y = !!sym(colname), fill = city_full)) +
    geom_bar(stat = "identity") +
    labs(title = paste0("Top 5 cities by ", metric, ", ordered by ", orientation),
         x = "City",
         y = metric)
}



aqi_state_barplot <- function(df, orientation, metric) {
  # Determine which column to use based on selection2
  if (metric == "Average") {
    colname <- "avg_aqi"
  } else if (metric == "Median") {
    colname <- "median_aqi"
  } else if (metric == "Minimum") {
    colname <- "min_aqi"
  } else if (metric == "Maximum") {
    colname <- "max_aqi"
  } else if (metric == "Ratio of Bad Days") {
    colname <- "above_x_pct"
  }
# Create the plot
ggplot(df, aes(x = state, y = !!sym(colname), fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = paste0("Top 5 cities by ", metric, ", ordered by ", orientation),
       x = "State",
       y = metric)
}




###############################################################
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
