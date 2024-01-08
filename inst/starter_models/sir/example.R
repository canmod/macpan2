#source("inst/starter_models/sir/tmb.R")
library(dplyr)
library(macpan2)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","sir",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
sir = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = "I"
)

## -------------------------
## specify objective function
## -------------------------

# function must be specified as a valid argument to macpan2::ObjectiveFunction()
# spec$after shows how log_likelihood is computed
obj_fn = ~ -sum(log_likelihood)

# update simulator to include this function
sir$replace$obj_fn(obj_fn)

## -------------------------
## parameterize model
## -------------------------

sir$update$transformations(Log("beta"))

# choose which parameter(s) to estimate
sir$replace$params(log(spec$default$beta), "log_beta")
sir

## -------------------------
## simulate fake data
## -------------------------

# beta value to simulate data with
true_beta = 0.4

## feed log(true_beta) to the simulator because we have
## already specified log-transformation of this parameter
observed_data = sir$report(log(true_beta))

## compute incidence for observed data
I_obs = rpois(time_steps, subset(observed_data, matrix == "I", select = c(value)) %>% pull())
I_obs_times = subset(observed_data, matrix == "I", select = c(time)) %>% pull()

if (interactive()) {
  plot(I_obs, type = "l", las = 1)
}

## -------------------------
## update simulator with fake data to fit to
## -------------------------

sir$update$matrices(
    I_obs = I_obs
  , I_obs_times = I_obs_times
)

## -------------------------
## plot likelihood surface (curve)
## -------------------------

if (interactive()) {
  log_betas = seq(from = log(0.1), to = log(1), length = 100)
  ll = vapply(
      log_betas
    , sir$objective
    , numeric(1L)
  )
  plot(exp(log_betas), ll, type = "l", las = 1)
  abline(v = true_beta)
}


## -------------------------
## fit parameters
## -------------------------

sir$optimize$nlminb()

## plot observed vs predicted value
if (interactive()) {
  print(sir$current$params_frame())
  print(paste0("exp(default) ",exp(sir$current$params_frame()$default)))
  print(paste0("exp(current) ",exp(sir$current$params_frame()$current)))
  plot(I_obs, type = "l", las = 1)
  lines(sir$report_values(), col = "red")
}
