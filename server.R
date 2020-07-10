## server file for Andy's SEIR shiny app
shinyServer <- function(input, output, session) {
  
  ## Alters intervention slider value on changing region

  output$intStrength_slider <- renderUI({
    sliderInput(inputId = 'intStrength', 
                label = 'Intervention Effect',
                min = 0, 
                max = 1, 
                value = mean(region_fits[[as.numeric(input$region)]]$gamma5)
    )
  })
  
  ## Need separate plotting functions to be able to download plots  
  
  ParamPlot_generator <- reactive({
    data <- region_params[[as.numeric(input$param)]]
    descriptive_param <- DescriptiveParams[as.numeric(input$param)]
    ggplot(data = data, aes(x = variable, y = value)) +
      geom_boxplot(col = "#68246D", fill = "#CBA8B1", alpha = 0.2 ) +
      theme_bw() +
      theme(
        plot.title = element_text(size=20, face="bold", colour = "#68246D"), 
        axis.text.x = element_text(angle = 90, size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_blank()) +
      # labs(title = descriptive_param, x = "Location", y = names(region_params)[as.numeric(input$param)])
      labs(title = descriptive_param, x = "Location", y = descriptive_param)+coord_flip() +
      scale_y_continuous(sec.axis = dup_axis())
    
  })
  output$ParamPlot <- renderPlot({
    ParamPlot_generator()
    })
  
  output$actualGamma <- renderText({
    choice = as.numeric(input$region)
    paste0(c("Fitted regional intervention strength at 01/06:", round(mean(region_fits[[choice]]$gamma5),3)))
  })
  
  SEIRPlot_generator <- reactive({
    intervention <- list(time = as.numeric(input$intdate - as.Date('2020-01-30') + 1),
                         strength = input$intStrength)
    
    
    
    end_time <- max(as.numeric(input$lastdate - as.Date('2020-01-30') + 1), intervention$time + 50)
    
    out_var <- c("S","I0","I1","I2","I3","D")[as.numeric(input$outvar)]
    
    ## This isolate needs to be here to stop the plot updating on changing input$region    
    isolate(choice <- as.numeric(input$region))
    p <- region_fits[[choice]]
    p_dat <- region_data[[choice]]
    
   results <- try(region_values(points = p,
                                 fitting = p_dat,
                                 region_name = names(region_fits)[choice],
                                 param = out_var,
                                 intervention = intervention,
                                 end_time = end_time), silent = TRUE)
    validate(
      need(length(results)>1,                     # it will only be 1 if it's a try-error
           "Loading first plot...")
    )
    
    
    plot_out = ggplot(data = results, aes(x = t, y = mean)) +
      geom_path(aes(x=t, y=upper), col = "#CBA8B1", size = 1) +
      geom_path(aes(x=t, y=lower), col = "#CBA8B1", size = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill="#CBA8B1", alpha=0.2) +
      geom_line(col = "#68246D", lwd = 1.5) +
      geom_vline(xintercept = input$intdate, col="#B6AAA7", lty=2) +
      labs(title = paste0(c(sub('\\.',' ',names(region_fits)[choice]), ": ", param_human_read[as.numeric(input$outvar)]), collapse = ""), x = 'Date', y = 'Frequency') +
      theme_bw() + theme(
        plot.title = element_text(size=20, face="bold", colour = "#68246D"),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12)
      )
    
    
    
    plot_out
    
  })
  output$SEIRPlot <- renderPlot({
    SEIRPlot_generator()
  })
  
  # SEIR projection plot
  # Should the name involve dates?
  # Should the plot show when intervention happened?
  plotName1 = reactive({
    region_name = names(region_fits)[as.numeric(input$region)]
    outvar_name = param_human_read[as.numeric(input$outvar)]
    paste(
      gsub(" ", "", region_name), '_',
      gsub(" ", "", outvar_name), 
      '.png',
      sep = '')
  })
  
  output$downloadPlot1 <- downloadHandler(
    filename = function(){plotName1()},
    content = function(file){
      ggsave(file, plot = SEIRPlot_generator(), device = 'png', width = 12.8, height=8)
    }
  )

  
  # Parameter fits
  
  plotName2 = reactive({
    param_name = DescriptiveParams[as.numeric(input$param)]
    paste(
      gsub(" ", "", param_name),
      '.png',
      sep = '')
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = function(){plotName2()},
    content = function(file){
      ggsave(file, plot = ParamPlot_generator(), device = 'png', width = 12.8, height=8)
    }
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
