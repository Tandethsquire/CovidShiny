### global file for Andy's SEIR shiny app

library(shiny)
library(purrr)
library(deSolve)
library(ggplot2)
library(shinyWidgets)
library(markdown)

load("region_fits.rda")
load("region_data.rda")
load("region_params.rda")
source('CovidSEIR.r')

DescriptiveParams = c("Proportion of Asymptomatic Cases", "Proportion of Severe Cases", "Proportion of Critical Cases",
  "Infection Fatality Rate", "Transmission from Incubating Cases", "Transmission from Asymptomatic Cases",
  "Transmission from Mild Cases", "Transmission from Severe Cases", "Transmission from Critical Cases",
  "Effect of Public Awareness", "Effect of Full Lockdown", "Effect of Easter Weekend", 
  "Effect of post-Easter", "Effect of loosening lockdown", "Presymptomatic Period", "Incubation Period", "Length of Mild Infection",
  "Length of Asymptomatic Infection", "Length of ICU stay", "Length of Hospital Stay")

param_human_read <- c('Susceptible', 'Asymptomatic Cases', 'Mild Cases', 'Severe Cases', 'Critical Cases', 'Cumulative deaths')
