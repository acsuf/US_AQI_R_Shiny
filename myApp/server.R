
states = states(cb = TRUE, progress_bar= FALSE)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Create map
  map = createLeafletMap(session, 'map')
  session$onFlushed(once=T, function(){
    observe({
    output$map = renderLeaflet({
      leaflet(data = df %>%
                select(state_name, state_id, city_ascii, lat, lng) %>%
                distinct()) %>%
        addTiles() %>%
        addPolygons(data = states, opacity = 0.3, fill = FALSE, color = 'black', stroke = FALSE
        ) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(
          lat = ~lat, lng = ~lng, color = "blue", radius = 2
        )
    })})
  })  
  
  date_range = reactive({
    cbind(input$Drange[1],input$Drange[2])
  })
  output$SliderText = renderText({date_range()})
  
  
  ######### REACTIVE ##########
  which_df = reactive({
    df %>%
      filter(between(df$Date, input$Drange[1], input$Drange[2])) %>%
      filter(input$choose_state == state_name | input$choose_state == 'All') %>%
      filter(input$choose_param == Defining.Parameter | input$choose_param == 'All')
  })
  
  which_loc = reactive({
    df %>%
      filter(between(df$Date, input$Drange[1], input$Drange[2])) %>%
      select(state_name, state_id, city_ascii, lat, lng) %>%
      distinct()
  })
})
  
