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
  
  output$ParamPlot <- renderPlot({
    data <- region_params[[as.numeric(input$param)]]
    descriptive_param <- c("Proportion of Asymptomatic Cases", "Proportion of Severe Cases", "Proportion of Critical Cases",
                           "Case Fatality Ratio", "Transmission from Incubating Cases", "Transmission from Asymptomatic Cases",
                           "Transmission from Mild Cases", "Transmission from Severe Cases", "Transmission from Critical Cases",
                           "Effect of Public Awareness", "Effect of Full Lockdown", "Effect of Easter Weekend", 
                           "Effect of post-Easter", "Effect of loosening lockdown", "Presymptomatic Period", "Incubation Period", "Length of Mild Infection",
                           "Length of Asymptomatic Infection", "Length of ICU stay", "Length of Hospital Stay")[as.numeric(input$param)]
    ggplot(data = data, aes(x = variable, y = value)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title = descriptive_param, x = "Location", y = names(region_params)[as.numeric(input$param)])
  })
  
  output$actualGamma <- renderText({
    choice = as.numeric(input$region)
    paste0(c("Fitted regional intervention strength at 01/06:", round(mean(region_fits[[choice]]$gamma5),3)))
  })
  
  output$SEIRPlot <- renderPlot({
    
 
    intervention <- list(time = as.numeric(input$intdate - as.Date('2020-01-30') + 1),
                                 strength = input$intStrength)
    
    
    
    end_time <- max(as.numeric(input$lastdate - as.Date('2020-01-30') + 1), intervention$time + 50)
    
    out_var <- c("S","I0","I1","I2","I3","D")[as.numeric(input$outvar)]
    
## This isolate needs to be here to stop the plot updating on changing input$region    
    isolate(choice <- as.numeric(input$region))
    p <- region_fits[[choice]]
    p_dat <- region_data[[choice]]
    
    param_human_read <- c('Susceptible', 'Asymptomatic Cases', 'Mild Cases', 'Severe Cases', 'Critical Cases', 'Deaths')
    
    results <- try(region_values(points = p,
                             fitting = p_dat,
                             region_name = names(region_fits)[choice],
                             param = out_var,
                             intervention = intervention,
                             end_time = end_time))
    validate(
      need(length(results)>1,                     # it will only be 1 if it's a try-error
           "Loading first plot...")
    )
    
       
    plot_out = ggplot(data = results, aes(x = t, y = mean)) +
              geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey70') +
              geom_line(col = 'blue', lwd = 1.5) +
              labs(title = paste0(c(sub('\\.',' ',names(region_fits)[choice]), ": ", param_human_read[as.numeric(input$outvar)]), collapse = ""), x = 'Date', y = 'Frequency') +
              theme_minimal()
    
    
    plot_out
    
    
    
  })
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
