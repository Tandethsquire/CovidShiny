## UI file for Andy's SEIR shiny app 

ui <- shinyUI(fluidPage(
  
  withMathJax(),
  
  chooseSliderSkin("Modern", color = "#72246C"),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
  #   tags$style(type="text/css", "padding:10px;")
  # ),
  tags$style('.container-fluid {
             margin:10px;
             }'),
  # App title ----
  
  titlePanel(
    windowTitle = "Covid SEIR - Regional Projection and Fitting",
    h2(
      div(style="display:inline-block;",img(src="DULogo.png", height = 50,style="left;")),
      div(style="display:inline-block;","Covid SEIR - Regional Projection and Fitting",style="padding-left:10px;font-weight:bold;")
    )
  ),
  
  tabsetPanel(
    tabPanel("Model",
             
             fluidRow(
               includeMarkdown("markdown/model_text.md") 
             ),
             fluidRow(
               column(5,
                      
                      # Input: Selector for region
                      selectInput(inputId = "region",
                                  label = "Region",
                                  choices = setNames(1:length(region_fits), sub('\\.',' ',names(region_fits))),
                                  selected = 1
                      ),
                      
                      # Input: Date selection for the last date to run to
                      dateInput(inputId = "lastdate",
                                label = "Final Date",
                                min = '2020-06-01',
                                max = NULL,
                                format = 'dd-mm-yyyy',
                                value = '2020-08-20'),
                      
                      # Input: Selector for the output variable
                      selectInput(inputId = 'outvar',
                                  label = 'Output',
                                  choices = list("Susceptible" = 1,
                                                 "Asymptomatic Cases" = 2,
                                                 "Mild Cases" = 3,
                                                 "Severe Cases" = 4,
                                                 "Critical Cases" = 5,
                                                 "Deaths" = 6),
                                  selected = 5
                      ),
                      
                      textOutput('actualGamma'),
                      
                      # Slider for intervention strength - dealt with in server.R 
                      uiOutput("intStrength_slider"),
                      
                      # Input: Date selection for intervention
                      dateInput(inputId = "intdate",
                                label = "Intervention Date",
                                min = '2020-06-01',
                                max = NULL,
                                format = 'dd-mm-yyyy',
                                value = '2020-07-01'
                      ),
                      downloadButton('downloadPlot1', label = "Download plot")
               ),
               column(7,
                      plotOutput(outputId = "SEIRPlot", height = '600'
                      )
                      
               )),
             
    ),
    tabPanel('Parameter Fits',
             fluidRow(
               column(5,
                      includeMarkdown("markdown/param_fits.md"),
                      selectInput(inputId = 'param',
                                  label = 'Parameter',
                                  choices = setNames(1:length(region_params), c("Proportion of Asymptomatic Cases", "Proportion of Severe Cases", "Proportion of Critical Cases",
                                                                                "Infection Fatality Rate", "Transmission from Incubating Cases", "Transmission from Asymptomatic Cases",
                                                                                "Transmission from Mild Cases", "Transmission from Severe Cases", "Transmission from Critical Cases",
                                                                                "Effect of Public Awareness", "Effect of Full Lockdown", "Effect of Easter Weekend", 
                                                                                "Effect of post-Easter", "Effect of loosening lockdown", "Presymptomatic Period", "Incubation Period", "Length of Mild Infection",
                                                                                "Length of Asymptomatic Infection", "Length of ICU stay", "Length of Hospital Stay")),
                                  selected = 1),
                      downloadButton('downloadPlot2', label = "Download plot"),
                      includeMarkdown("markdown/param_fit2.md")
               ),
               column(7,
                      plotOutput(outputId = "ParamPlot", height = '800')
               )
             ),
    ),
    tabPanel('Model Description',
             mainPanel(
               includeMarkdown("markdown/model_desc.md")
             )
    )
  )
  ))
