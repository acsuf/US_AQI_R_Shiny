# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  output$SliderText = renderText({date_range()})
  # 
  observe({
    updateSelectInput(session, "choose_city_TS", 
                      choices = setNames(citydf$city_list[citydf$state == input$choose_state_TS1], 
                                         citydf$city_list[citydf$state == input$choose_state_TS1]))
  })
  # Create map
  output$map <- renderLeaflet({
    create_map(map_df())
  })
  
  output$time_ser <- renderPlot({
    if (input$whereabouts == 'Compare within states') {
    plot_aqi_city(TS_df(), input$choose_city_TS)
    }
    else if (input$whereabouts == 'Compare between states') {
    plot_aqi_state(TS_df(), input$choose_state_TS2)
    }
  })
  
  output$comp <- renderPlot({
    if (input$how_comp == 'Overall' & input$reso == 'Cities') {
      plot_aqi_city_boxplots_total(comp_df(), input$city1, input$city2)
    }
    else if (input$how_comp == 'Overall' & input$reso == 'States') {
      plot_aqi_state_boxplots_total(comp_df(), input$state1, input$state2)
    }
    else if (input$how_comp == 'Monthly' & input$reso == 'States') {
      plot_aqi_state_boxplots_months(comp_df(), input$state1, input$state2)
    }
    else if (input$how_comp == 'Monthly' & input$reso == 'Cities') {
      plot_aqi_city_boxplots_months(comp_df(), input$city1, input$city2)
    }
  })
  
  
  output$scatter <- renderPlot({
    plot_aqi_sc(sc_df())
  })
  
  output$bars <- renderPlot({
    if (input$cityORstate == 'States') {
      aqi_state_barplot(
        aqi_summary_ordered(
          aqi_summary_state_df(
            bw_df(), 
            input$cutoff
          ), 
          input$bw, 
          input$metric
        ),
        input$bw,
        input$metric
      )
    }
    else if (input$cityORstate == 'Cities') {
      if ('All' %in% input$statelist_b) {
        aqi_city_barplot(
          aqi_summary_ordered(
            aqi_summary_city_all_df(
              bw_df(), 
              input$cutoff
              ), 
            input$bw, 
            input$metric
          ),
          input$bw,
          input$metric
        )
      }
      else {
        aqi_city_barplot(
          aqi_summary_ordered(
            aqi_summary_city_df(
              bw_df(), 
              input$cutoff,
              input$statelist_b
            ), 
            input$bw, 
            input$metric
          ),
          input$bw,
          input$metric
        )
      }
    }
    
  })
  ######### REACTIVE ##########
  
  date_range = reactive({
    cbind(input$Drange[1],input$Drange[2])
  })
  
  ## MAP ##
  map_df = reactive({
    if (input$Temporal == "Range") {
      df %>%
      filter(between(df$Date, input$Drange[1], input$Drange[2])) %>%
      filter(input$choose_state == state | input$choose_state == 'All') %>%
      filter(input$choose_param == Defining.Parameter | input$choose_param == 'All') %>%
      group_by(state, state_id, city, lat, lng, Defining.Parameter) %>% 
      summarize(min = min(AQI), max = max(AQI), avg = round(mean(AQI), digits = 2), median = median(AQI), count = n()) %>% ungroup() %>%
      assign_category()
  }
  else if (input$Temporal == "Month") {
    df %>%
    filter(input$dMonth == month) %>%
    filter(input$choose_state == state | input$choose_state == 'All') %>%
    filter(input$choose_param == Defining.Parameter | input$choose_param == 'All') %>%
    group_by(state, state_id, city, lat, lng, Defining.Parameter) %>% 
    summarize(min = min(AQI), max = max(AQI), avg = round(mean(AQI), digits = 2), median = median(AQI), count = n()) %>% ungroup() %>%
    assign_category()
  }
  })

  ## TS ##
   
  TS_df = reactive({
    if (input$whereabouts == 'Compare within states') {
      df %>%
        filter(between(df$Date, input$Drange_TS[1], input$Drange_TS[2])) %>%
        filter(input$choose_state_TS1 == state) %>%
        filter(input$choose_param_TS == Defining.Parameter)
    }
    else if (input$whereabouts == 'Compare between states') {
      df %>%
        filter(between(df$Date, input$Drange_TS[1], input$Drange_TS[2])) %>%
        filter(input$choose_state_TS2 == state) %>%
        filter(input$choose_param_TS == Defining.Parameter)
    }
  })
  
  ## Compare ##
  comp_df = reactive({
    df %>% 
      filter(between(df$Date, input$Drange_comp[1], input$Drange_comp[2])) %>%
      filter(input$choose_param_comp == Defining.Parameter)
  })
  ## Scatter ##
  sc_df = reactive({
    if (input$Temporal_sc == 'Range') {
    df %>%
      filter(between(df$Date, input$Drange_sc[1], input$Drange_sc[2])) %>%
      filter(input$choose_param_sc == Defining.Parameter)
    }
    else if (input$Temporal_sc == 'Month') {
      df %>% filter(month == input$dMonth_sc) %>% 
        filter(input$choose_param_sc == Defining.Parameter)
    }
  })
  ## Best/Worst
  bw_df = reactive({
    if (input$Temporal_b == 'Range') {
      df %>%
        filter(between(df$Date, input$Drange_b[1], input$Drange_b[2])) %>%
        filter(input$choose_param_b == Defining.Parameter)
    }
    else if (input$Temporal_b == 'Month') {
      df %>% 
        filter(month == input$dMonth_b) %>%
        filter(input$choose_param_b == Defining.Parameter)
    }
  })
})
  
