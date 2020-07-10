
This app uses data on cases and deaths in regions in the North East and Yorkshire to fit an SEIR model. The cases data is taken from the [https://coronavirus.data.gov.uk/](https://coronavirus.data.gov.uk/ "ONS cases") data, and the deaths data is taken from the [https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/](https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/ "NHS Deaths")  data. See the 'Model description' tab for more information.


The sidebar gives the options for producing the plot:

- __Region__: a choice of fitted regions in the North East and Yorkshire. The final two are individual fits for the aggregated North East, and Yorkshire, regions.
- __Final date__: the furthest into the future the model should be run. Later dates come with higher levels of uncertainty.
- __Output__: which class to plot. Options are the number of Susceptible people, Asymptomatic, Mild, Severe or Critical cases, or the total number of Deaths.
- __Intervention effect__: we can introduce a new period of lockdown or relaxation to see what could happen under general regional changes in policy. This works as an overall factor on the transmission rates from the infected classes: a value of 0 indicates that no contact is occuring whatsoever, while a value of 1 corresponds to pre-Covid behaviour. 
- __Intervention date__: the date at which the new intervention period starts.  




                
                           