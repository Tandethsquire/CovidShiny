library(purrr)
library(ggplot2)
library(deSolve)

### Modelling Functions
## Set Up ODEs
SetOdes <- function(t, y, p) {
  S = y[1]
  E0 = y[2]
  Ea = y[3]
  Eb = y[4]
  E1 = y[5]
  I0 = y[6]
  I1 = y[7]
  I2 = y[8]
  I3 = y[9]
  R = y[10]
  D = y[11]
  
  with(as.list(p), {
    dS.dt = -(be*E1 + b0*I0 + b1*I1 + b2*I2 + b3*I3)*S/(S+E0+E1+I0+I1+I2+I3+R)
    dE0.dt = (be*E1 + b0*I0 + b1*I1 + b2*I2 + b3*I3)*S/(S+E0+E1+I0+I1+I2+I3+R) - 3*a0*E0
    dEa.dt = 3*a0*E0 - 3*a0*Ea
    dEb.dt = 3*a0*Ea - 3*a0*Eb
    dE1.dt = 3*a0*Eb - a1*E1
    dI0.dt = f*a1*E1 - g0*I0
    dI1.dt = (1-f)*a1*E1 - g1*I1 - p1*I1
    dI2.dt = p1*I1 - g2*I2 - p2*I2
    dI3.dt = p2*I2 - g3*I3 - u*I3
    dR.dt = g0*I0 + g1*I1 + g2*I2 + g3*I3
    dD.dt = u*I3
    
    return(list(c(dS.dt, dE0.dt, dEa.dt, dEb.dt, dE1.dt, dI0.dt, dI1.dt, dI2.dt, dI3.dt, dR.dt, dD.dt)))
  })
}

## Converts 'human readable' parameters to model parameters
getParams <- function(plist, beta_modifier) {
  with(as.list(plist), {
    output = list()
    output$b0 <- b0*beta_modifier
    output$b1 <- b1*beta_modifier
    output$b2 <- b2
    output$b3 <- b3
    output$be <- be*beta_modifier
    output$a0 <- 1/(IncubPeriod-PreSymPeriod)
    output$a1 <- 1/PreSymPeriod
    output$f <- FracAsym
    output$g0 <- 1/DurAsym
    output$g1 <- FracMild/DurMildInf
    output$g2 <- 1/DurHosp - FracCritical/(DurHosp*(FracSevere + FracCritical))
    output$g3 <- 1/TimeICUDeath - CFR/(TimeICUDeath*FracCritical)
    output$p1 <- (1 - FracMild)/DurMildInf
    output$p2 <- FracCritical/(DurHosp*(FracSevere + FracCritical))
    output$u <- CFR/(TimeICUDeath*FracCritical)
    return(output)
  })
}

## Solves the ODEs above.
GetSpread <- function(pars, Tmax, int_times, int_params, y0, t_initial = 0) {
  initial_vals <- y0
  interventions <- c(1, int_params)
  Tset <- c(t_initial, int_times, Tmax)
  out_data <- array(0, dim = c(Tmax - t_initial + 1, length(y0)+1))
  out_data[1,] <- c(t_initial, y0)
  for (i in 1:(length(Tset)-1)) {
    param_vals <- getParams(pars, interventions[i])
    t = seq(from = Tset[i], to = Tset[i+1])
    out <- ode(y = initial_vals, times = t, func = SetOdes, parms = param_vals)
    for (j in t){
      out_data[j+1-t_initial,] <- out[j-t[1]+1,]
    }
    initial_vals <- unlist(out[length(out[,1]), -1])
  }
  return(setNames(data.frame(out_data), c('t', names(y0))))
}

## Generates model runs for multiple parameter points.
# Built-in interventions at:
# t = 40: Public Awareness
# t = 52: Full lockdown
# t = 69: Start of Easter weekend (public less careful about social distancing)
# t = 72: End of Easter weekend
# t = 124: Gradual loosening of lockdown (as of June 1st)
genResults <- function(points, input_names, times, end_time, out_names, desired_out = 'D', start_time = 0, int_times = c(40, 52, 69, 72, 124), Spop = 56678470) {
  if (!is.null(dim(points)))
  {
    results_arr <- array(0, dim = c(end_time - start_time + 1, length(out_names)+1, length(points[,1])))
    lim <- length(points[,1])
  }
  else {
    results_arr <- array(0, dim = c(end_time - start_time + 1, length(out_names)+1, 1))
    lim <- 1
  }
  for (i in 1:lim) {
    ifelse(!is.null(dim(points)), p <- as.list(points[i,]), p <- as.list(points))
    params = list(
      PreSymPeriod = p$presym,
      IncubPeriod = p$incub,
      DurMildInf = p$mild,
      DurAsym = p$asym,
      FracAsym = p$AF,
      FracSevere = p$SF,
      FracCritical = p$CF,
      FracMild = 1-p$AF-p$SF-p$CF,
      CFR = p$CFR,
      TimeICUDeath = p$icu,
      DurHosp = p$hosp,
      be = p$betae,
      b0 = p$beta0,
      b1 = p$beta1,
      b2 = p$beta2,
      b3 = p$beta3
    )
    initial_vals <- c(
      S = Spop - p$I0 - p$I1 - p$I2 - p$I3 - p$E0 - p$Ea - p$Eb - p$E1,
      E0 = p$E0,
      Ea = p$Ea,
      Eb = p$Eb,
      E1 = p$E1,
      I0 = p$I0,
      I1 = p$I1,
      I2 = p$I2,
      I3 = p$I3,
      R = 0,
      D = 0
    )
    gammas <- unlist(p[grep('gamma.', names(p))], use.names = F)
    tryCatch(results_arr[,,i] <- as.matrix(GetSpread(params, end_time, int_times, gammas, initial_vals, start_time)),
             error = function(cond){
               print(cond)
               print(p)
               stop("Couldn't solve differential equations: see above for details of point.")
             }
    )
  }
  wanted_arr <- results_arr[times+1-start_time, c(TRUE, out_names %in% desired_out), ]
  return(wanted_arr)
}

## Generate the region bounds
# Given a set of fits, runs the model (potentially with another intervention) and gives forecast
# with upper and lower confidence intervals for prediction.
# The trajectory and the confidence intervals are given by mu +- 3*sigma, where mu is the weighted
# average and sigma the weighted standard deviation. Weighting is determined by the implausibility
# for each point: the lower the implausibility, the higher the weight.
# The function only considers the 100 best points provided by the model fit (even though the model
# fits themselves comprise 300 points).
region_values <- function(points, fitting, region_name, param = 'D', intervention = NULL, n_points = 100, end_time = 200) {
  param_names <- names(points)[1:(ncol(points)-1)]
  out_n <- c('S', 'E0', 'Ea', 'Eb', 'E1', 'I0', 'I1', 'I2', 'I3', 'R', 'D')
  points <- points[1:n_points,]
  imps <- points[1:(n_points-1),ncol(points)]
  weights <- max(imps)/imps
  all_data <- fitting$data
  d_obs <- fitting$deaths
  c_obs <- fitting$cases
  pop <- fitting$pop
  first_case <- fitting$first_case
  base_int <- c(40, 52, 69, 72, 124)
  if (!is.null(intervention)) {
    base_int <- c(base_int, intervention$time)
    points$gamma6 <- intervention$strength
    param_names <- c(param_names, 'gamma6')
  }
  params <- c('S', 'I0', 'I1', 'I2', 'I3', 'D')
  param_human_read <- c('Susceptible', 'Asymptomatic Cases', 'Mild Cases', 'Severe Cases', 'Critical Cases', 'Deaths')
  model_runs <- genResults(points[,param_names], param_names, first_case:end_time, end_time, out_n, params, first_case, Spop = pop, int_times = base_int)
  model_vals <- data.frame(model_runs[,1,1], model_runs[,match(param, params)+1,]) %>% setNames(c('t', paste0('r',1:(length(points[,1])))))
  model_vals$t <- seq(from = as.Date('2020-02-01')+first_case-2, to = as.Date('2020-02-01')+end_time-2, by = 1)
  model_vals <- model_vals[,1:(ncol(model_vals)-1)]
  weight_mean <- apply(model_vals[,2:ncol(model_vals)], 1, function(x) sum(weights * x)/sum(weights))
  squared_diff <- apply(sweep(model_vals[,2:ncol(model_vals)], 1, weight_mean)^2, 1, sum)
  weight_sd <- sqrt(sum(weights)/(sum(weights)^2 - sum(weights^2)) * squared_diff)
  lower_lim <- map_dbl(weight_mean - 3*weight_sd, ~max(0, .))
  output <- data.frame(t = model_vals$t, mean = weight_mean, lower = lower_lim, upper = weight_mean + 3*weight_sd)
  return(output)
}
