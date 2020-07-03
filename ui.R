## UI file for Andy's SEIR shiny app 

ui <- shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Covid SEIR - Regional Projection and Fitting"),
  tabsetPanel(
    tabPanel("Model",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
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
                 
                 # Input: Slider for strength of intervention
                 sliderInput(inputId = 'intStrength',
                             label = 'Intervention Effect',
                             min = 0,
                             max = 1,
                             value = 0.5
                 ),
                 
                 # Input: Date selection for intervention
                 dateInput(inputId = "intdate",
                           label = "Intervention Date",
                           min = '2020-06-01',
                           max = NULL,
                           format = 'dd-mm-yyyy',
                           value = '2020-07-01'
                 ),
                 
                 actionButton('runModel', "Run Model Output")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 h1("Projected Case Numbers"),
                 # Output: ggplot
                 plotOutput(outputId = "SEIRPlot"),
                 p("This app uses data on cases and deaths in regions in the North East and Yorkshire to fit an SEIR model.
                   The cases data is taken from the ", a("ONS Cases", href = "https://coronavirus.data.gov.uk/"), " data, and the deaths data is taken from the",
                   a("NHS Deaths", href = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/"), " data."),
                 p("The sidebar gives the options for producing the plot:"),
                 tags$ul(
                   tags$li("Region - a choice of fitted regions in the North East and Yorkshire. The final two are individual fits for the aggregated
                           North East, and Yorkshire, regions."),
                   tags$li("Final date - the furthest into the future the model should be run. Later dates come with higher levels of uncertainty."),
                   tags$li("Output - which class to plot. Options are the number of Susceptible people, Asymptomatic, Mild, Severe or Critical cases,
                           or the total number of Deaths."),
                   tags$li("Intervention Effect - we can introduce a new period of lockdown or relaxation to see what could happen under general
                           regional changes in policy. This works as an overall factor on the transmission rates from the infected classes: a 
                           value of 0 indicates that no contact is occuring whatsoever, while a value of 1 corresponds to pre-Covid behaviour."),
                   tags$li("Intervention Date - the date at which the new period starts.")
                   )
                   )
               )
                 ),
    tabPanel('Parameter Fits',
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'param',
                             label = 'Parameter',
                             choices = setNames(1:length(region_params), c("Proportion of Asymptomatic Cases", "Proportion of Severe Cases", "Proportion of Critical Cases",
                                                                           "Infection Fatality Rate", "Transmission from Incubating Cases", "Transmission from Asymptomatic Cases",
                                                                           "Transmission from Mild Cases", "Transmission from Severe Cases", "Transmission from Critical Cases",
                                                                           "Effect of Public Awareness", "Effect of Full Lockdown", "Effect of Easter Weekend", 
                                                                           "Effect of post-Easter", "Effect of loosening lockdown", "Presymptomatic Period", "Incubation Period", "Length of Mild Infection",
                                                                           "Length of Asymptomatic Infection", "Length of ICU stay", "Length of Hospital Stay")),
                             selected = 1)
               ),
               mainPanel(
                 plotOutput(outputId = "ParamPlot")
               )
             )
    ),
    tabPanel('Model Description',
             mainPanel(
               h1("Model Description"),
               p("The model run is a modified SEIR model, with stratification of the Exposed (E) and Infected (I) classes.
          The length of time spent in the E class corresponds to the incubation period of the disease: during the
          incubation period, a person can be incubating (E0) or presymptomatic (E1). The I class is split by severity
          or type of illness: asymptomatic cases correspond to I0, those with a mild infection to I1, those whose
          symptoms are serious enough to warrant hospital care to I2, and those who would be expected to require
          ICU treatment to I3."),
               HTML("<center><img src = 'SEIRModel.png', height = 500, width = 750 alt = 'Model diagram'></center>"),
               p("The parameters displayed in the transitions are the parameters used to run the model, converted from
          more physical parameters (such as pre-symptomatic period, length of hospital stay, etc.). For example, the
          length of incubation period is related to these parameters as 1/alpha1 + 1/alpha2. The transmission of the
          disease from infected to susceptible members of the population can occur from any of E1, I0, I1, I2, or I3,
          each with their own transmission rates."),
               p("In the absence of spatial structure in the model, the effect of lockdown or interventions is treated as a blanket effect on
          the transmission rates - if, for instance, lockdown reduced the amount of contact an average person has to 50% of the
          pre-Covid amount, the transmission parameters would be halved. This does not apply to those people who are in hospital
          with the disease (the transmission from I2 and I3 does not change)."),
               h2("Fitting"),
               p("Given the cases and deaths data for each region, the space of possible parameter values was explored and compared to
          cases and deaths at time-points spaced 7 days apart, from the first incidence of cases in the region. The best 100
          candidate points are used to produce the model runs plotted. The observations used conclude on the 16th June.")
             )
    )
             )
    ))
