#source("inst/starter_models/sir_demog/tmb.R")
library(macpan2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","sir_demog",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
sir_demog = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("I","N")
)

## -------------------------
## specify objective function
## -------------------------

# negative log likelihood
obj_fn = ~ -sum(dpois(I_obs, rbind_time(I, I_obs_times)))

# update simulator to create new variables 
# I_obs and I_obs_times and initialize
sir_demog$update$matrices(
    I_obs = empty_matrix
  , I_obs_times = empty_matrix
)

# update simulator to include this function
sir_demog$replace$obj_fn(obj_fn)

## -------------------------
## parameterize model
## -------------------------

sir_demog$update$transformations(Log("beta"))

# choose which parameter(s) to estimate
sir_demog$replace$params(log(spec$default$beta), "log_beta")
sir_demog

## -------------------------
## simulate fake data
## -------------------------

# beta value to simulate data with
true_beta = 0.3

## feed log(true_beta) to the simulator because we have
## already specified log-transformation of this parameter
observed_data = sir_demog$report(log(true_beta))

## compute incidence for observed data
I_obs = rpois(time_steps, subset(observed_data, matrix == "I", select = c(value)) %>% pull())
I_obs_times = subset(observed_data, matrix == "I", select = c(time)) %>% pull()

if (interactive()) {
  plot(I_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

sir_demog$update$matrices(
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
    , sir_demog$objective
    , numeric(1L)
  )
  plot(exp(log_betas), ll, type = "l", las = 1)
  abline(v = true_beta)
}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
sir_demog$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  print(sir_demog$current$params_frame())
  print(paste0("exp(default) ",exp(sir_demog$current$params_frame()$default)))
  print(paste0("exp(current) ",exp(sir_demog$current$params_frame()$current)))
  plot(I_obs, type = "l", las = 1)
  lines(sir_demog$report() %>% filter(matrix=="I") %>% select(value), col = "red")
}

## -------------------------
## exploring
## -------------------------

## plot population size (should be exponential)
if (interactive()) {
  times_to_plot = 1:time_steps
  pop_change = spec$default$birth_rate-spec$default$death_rate
  plot(spec$default$N*((1+pop_change)^times_to_plot), type = "l", las = 1, ylab='N')
  lines(sir_demog$report() %>% filter(matrix=="N") %>% select(value), col = "red")
  legend("topleft",legend=c("theoretical","observed"), lty = 1, col=c("black","red"))
}

                   