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
####################################################### Panels for Map tab    
        conditionalPanel(
          condition = "input.ui == 'Map'",
          selectInput(
            inputId = "Temporal",
            label = "Select temporal inspection method",
            choices = c("Range", "Month"),
            selected = "Range"
          ),
          conditionalPanel(
            condition = "input.Temporal == 'Range'",
            sliderInput(inputId = "Drange", 
              label = "Select date range", 
              min = as.POSIXct("2022-01-01"), 
              max =as.POSIXct("2022-05-31"), 
              value= c(as.POSIXct("2022-01-01"), as.POSIXct("2022-05-30")), 
              timeFormat="%b %d %Y"),
          ),
          conditionalPanel(
            condition = "input.Temporal == 'Month'",
            selectInput(
              inputId = "dMonth",
              label = "Select month(s)",
              choices = c(
                "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"
              ),
              selected = "January",
              multiple = TRUE
            )
          ),
          selectInput(
            inputId = 'choose_state', 
            label = 'State',
            choices = c('All', 
                        sort(unique(df$state))),
            selected = 'All', multiple = TRUE
          ),
          selectInput(
            inputId = 'choose_param', 
            label = 'Measurement',
            choices = c(sort(unique(df$Defining.Parameter))),
            selected = 'PM2.5'
          ),
        ),
####################################################### Panels for Time Series tab       
        conditionalPanel(
          condition = "input.ui == 'Time Series'",
          sliderInput(inputId = "Drange_TS", 
                      label = "Select date range", 
                      min = as.POSIXct("2022-01-01"), 
                      max =as.POSIXct("2022-05-31"), 
                      value= c(as.POSIXct("2022-01-01"), as.POSIXct("2022-05-30")), 
                      timeFormat="%b %d %Y"
                      ),
          selectInput(
            inputId = 'whereabouts',
            label = 'Select comparison method',
            choices = c('Compare within states', 'Compare between states'),
            selected = 'Compare within states'
            ),

          conditionalPanel(
            condition = "input.whereabouts == 'Compare within states'",
            selectInput(
              inputId = 'choose_state_TS1', 
              label = 'State',
              choices = c(sort(unique(df$state))),
              selected = 'Oregon', #multiple = TRUE
            ),
            selectInput(
              inputId = 'choose_city_TS',
              label = 'Cities',
              choices = NULL,
              multiple = TRUE
            )
            
          ),
          conditionalPanel(
            condition = "input.whereabouts == 'Compare between states'",
            selectInput(
              inputId = 'choose_state_TS2', 
              label = 'States',
              choices = c(sort(unique(df$state))),
              selected = 'Oregon', multiple = TRUE
            ),
          ),

          selectInput(
            inputId = 'choose_param_TS', 
            label = 'Measurement',
            choices = c(sort(unique(df$Defining.Parameter))),
            selected = 'PM2.5'
          ),
        ),
########################################################  Panels for Compare tab     
        conditionalPanel(
          condition = "input.ui == 'Compare'",
          sliderInput(inputId = "Drange_comp", 
                      label = "Select date range", 
                      min = as.POSIXct("2022-01-01"), 
                      max =as.POSIXct("2022-05-31"), 
                      value= c(as.POSIXct("2022-01-01"), as.POSIXct("2022-05-30")), 
                      timeFormat="%b %d %Y"
          ),
          selectInput(
            inputId = 'reso',
            label = 'Compare',
            choices = c('States', 'Cities'),
            selected = 'States'
          ),
          conditionalPanel(
            condition = "input.reso == 'States'",
            selectInput(inputId = 'state1',
                        label = 'Select first state',
                        choices = c(sort(unique(df$state))),
                        selected = 'Alabama'
            ),
            selectInput(inputId = 'state2',
                        label = 'Select second state',
                        choices = c(sort(unique(df$state))),
                        selected = 'Wisconsin'
            )
          ),
          
          conditionalPanel(
            condition = "input.reso == 'Cities'",
            selectInput(inputId = 'city1',
                        label = 'Select first city',
                        choices = c(sort(unique(df$city_full))),
                        selected = 'Bend, OR'
            ),
            selectInput(inputId = 'city2',
                        label = 'Select second city',
                        choices = c(sort(unique(df$city_full))),
                        selected = 'Duluth, MN'
            )
          ),
          selectInput(
            inputId = 'how_comp',
            label = 'How to compare',
            choices = c('Overall', 'Monthly'),
            selected = 'Overall'
          ),
          selectInput(
            inputId = 'choose_param_comp', 
            label = 'Measurement',
            choices = c(sort(unique(df$Defining.Parameter))),
            selected = 'PM2.5'
          ),
        ),
########################################################     Panels for scatter tab    
        conditionalPanel(
          condition = "input.ui == 'Scatter'",
          selectInput(
            inputId = "Temporal_sc",
            label = "Select temporal inspection method",
            choices = c("Range", "Month"),
            selected = "Range"
          ),
          conditionalPanel(
            condition = "input.Temporal_sc == 'Range'",
            sliderInput(inputId = "Drange_sc", 
              label = "Select date range", 
              min = as.POSIXct("2022-01-01"), 
              max =as.POSIXct("2022-05-31"), 
              value= c(as.POSIXct("2022-01-01"), as.POSIXct("2022-05-30")), 
              timeFormat="%b %d %Y"),
          ),
          conditionalPanel(
            condition = "input.Temporal_sc == 'Month'",
            selectInput(
              inputId = "dMonth_sc",
              label = "Select month(s)",
              choices = c(
                "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"
              ),
              selected = "January",
              multiple = TRUE
            )
          ),
          selectInput(
            inputId = 'choose_param_sc', 
            label = 'Measurement',
            choices = c(sort(unique(df$Defining.Parameter))),
            selected = 'PM2.5'
          ),
        ),

################### Panels for bars #####################
        conditionalPanel(
          condition = "input.ui == 'Best and Worst Locations'",
          selectInput(
            inputId = "Temporal_b",
            label = "Select temporal inspection method",
            choices = c("Range", "Month"),
            selected = "Range"
          ),
          conditionalPanel(
            condition = "input.Temporal_b == 'Range'",
            sliderInput(inputId = "Drange_b", 
                        label = "Select date range", 
                        min = as.POSIXct("2022-01-01"), 
                        max =as.POSIXct("2022-05-31"), 
                        value= c(as.POSIXct("2022-01-01"), as.POSIXct("2022-05-30")), 
                        timeFormat="%b %d %Y"),
          ),
          conditionalPanel(
            condition = "input.Temporal_b == 'Month'",
            selectInput(
              inputId = "dMonth_b",
              label = "Select month(s)",
              choices = c(
                "January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"
              ),
              selected = "January",
              multiple = TRUE
            )
          ),
          selectInput(
            inputId = 'bw',
            label = 'Looking for the',
            choices = c('Best', 'Worst'),
            selected = 'Best'
          ),
          selectInput(
            inputId = 'cityORstate',
            label = NULL,
            choices = c('States', 'Cities'),
            selected = 'States'
          ),
          conditionalPanel(
            condition = "input.cityORstate == 'Cities'",
            selectInput(
              inputId = 'statelist_b',
              label = 'Select states',
              choices = c('All', sort(unique(df$state))),
              selected = 'All',
              multiple = TRUE
            )
          ),
          selectInput(
            inputId = 'metric',
            label = 'Select metric',
            choices = c('Average', 'Median', 'Minimum', 'Maximum', 'Ratio of Bad Days'),
            selected = 'Average'
          ),
          selectInput(
            inputId = 'choose_param_b', 
            label = 'Measurement',
            choices = c(sort(unique(df$Defining.Parameter))),
            selected = 'PM2.5'
          ),
          numericInput(
            inputId = 'cutoff',
            label = 'Decide AQI cutoff for bad day',
            value = 50
          )
        )
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
                  size=2,
                  color="#FF0000")
                )
              ),
            tabPanel('Time Series',
              plotOutput('time_ser')
              ),
            tabPanel('Compare',
              plotOutput('comp')
              ),
            tabPanel('Scatter',
              plotOutput('scatter')
              ),
            tabPanel('Best and Worst Places to Live',
              plotOutput('bars')
              )
            )
          )
        )
      )
  
  ),
))

