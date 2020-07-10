# Model Description

The model run is a modified SEIR model, with stratification of the Exposed (*E*) and Infected (*I*) classes. The length of time spent in the *E* class corresponds to the incubation period of the disease: during the incubation period, a person can be incubating (*E<sub>0</sub>*) or presymptomatic (*E<sub>1</sub>*). The *I* class is split by severity or type of illness: asymptomatic cases correspond to *I<sub>0</sub>*, those with a mild infection to *I<sub>1</sub>*, those whose symptoms are serious enough to warrant hospital care to *I<sub>2</sub>*, and those who would be expected to require ICU treatment to *I<sub>3</sub>*.

![GitHub Logo](SEIRmodel.png)

The parameters displayed in the transitions are the parameters used to run the model, converted from more physical parameters (such as pre-symptomatic period, length of hospital stay, etc.). For example, in terms of these model parameters, the length of incubation period is 

1/&alpha;<sub>1</sub> + 1/&alpha;<sub>2</sub>. 

The transmission of the disease from infected to susceptible members of the population can occur from any of *E<sub>1</sub>*, *I<sub>0</sub>*, *I<sub>1</sub>*, *I<sub>2</sub>*, or *I<sub>3</sub>*, each with their own transmission rates. 

In the absence of spatial structure in the model, the effect of lockdown or interventions is treated as a blanket effect on the transmission rates - if, for instance, lockdown reduced the amount of contact an average person has to 50% of the pre-Covid amount, the transmission rate parameters would be halved. This does not apply to those people who are in hospital with the disease (the transmission from *I<sub>2</sub>* and *I<sub>3</sub>* does not change).

## Fitting

The true values of the rate parameters are unknown, although scientists may have some idea of the values of some of them. To estimate the values, we used cases and deaths data from each region and found the values of rate parameters that produced results best matching the data. For each region, we ran the SEIR model thousands of times, each time with a different set of parameter values. This produced thousands of sets of output, ie. time series of the model classes *E<sub>0</0>*, *E<sub>1</sub>*, *I<sub>0</sub>* and so on. 

For some of these (*D*, *I<sub>1</sub>*, *I<sub>2</sub>* and *I<sub>3</sub>*) we have data that should correspond, although it is worth noting that the case data particularly is likely to be incomplete. We then selected the 100 sets of parameter values that produced output most closely matching the data. These 100 sets of parameter values have been used to produce the model runs plotted. The observations used conclude on the 16<sup>th</sup> June.

               
              