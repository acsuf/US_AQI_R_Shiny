# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Create map
  output$map <- renderLeaflet({
    create_map(map_df())
  })
  
  output$SliderText = renderText({date_range()})
  
  
  ######### REACTIVE ##########
  map_df = reactive({
    df %>%
      filter(between(df$Date, input$Drange[1], input$Drange[2])) %>%
      filter(input$choose_state == state | input$choose_state == 'All') %>%
      filter(input$choose_param == Defining.Parameter | input$choose_param == 'All') %>%
      group_by(state, state_id, city, lat, lng, Defining.Parameter) %>% 
      summarize(min = min(AQI), max = max(AQI), avg = round(mean(AQI), digits = 2), median = median(AQI), count = n()) %>% ungroup() %>%
      assign_category()
  })
  
  date_range = reactive({
    cbind(input$Drange[1],input$Drange[2])
  })
})
  
