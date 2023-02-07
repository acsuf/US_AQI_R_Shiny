#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
shinyUI(fluidPage(
  
  # Title
  headerPanel("US AQI"),
    sidebarLayout(
      
      sidebarPanel(
        sliderInput(inputId = "Drange", 
                    label = "Time", 
                    min = as.POSIXct("2022-01-01"), 
                    max =as.POSIXct("2022-05-31"), 
                    value= c(as.POSIXct("2022-01-01"), as.POSIXct("2022-05-30")), 
                    timeFormat="%b %d %Y"),
        
        selectInput(
          inputId = 'choose_state', 
          label = 'State',
          choices = c('All', 
                      sort(unique(df$state_name))),
          selected = 'All'
        ),
        selectInput(
          inputId = 'choose_param', 
          label = 'Measurement',
          choices = c('All', 
                      sort(unique(df$Defining.Parameter))),
          selected = 'All'
        ),
      ),
      
      mainPanel(
  
  fluidRow(
    column(12,
           tabsetPanel(id = "ui",
                       tabPanel("Map",
                                column(12, 
                                       shinycssloaders::withSpinner(
                                         leaflet::leafletOutput(
                                           outputId ="map", height="600px"
                                           ),
                                         size=2
                                         , color="#FF0000")
                                       )
                                 ),
                       tabPanel('Time Series',
                                plotOutput('time_ser', height = 250))
                      )
           ),
    ))
  )
))

