source("inst/starter_models/sir_demo/tmb.R")
library(ggplot2)
library(dplyr)


## -------------------------
## define simulator
## -------------------------

# define objective function
obj_fn = ObjectiveFunction(~ -sum(log_likelihood))

# simulator object
tmb_simulator = TMBModel(
    init_mats = init_mats
  , expr_list = expr_list
  , obj_fn = obj_fn
)$simulator()

## mp_tmb_simulator

## -------------------------
## parameterize model
## -------------------------

tmb_simulator$update$transformations(Log("beta"))
tmb_simulator$replace$params(log(init_mats$get("beta")), "log_beta")
tmb_simulator

## -------------------------
## simulate fake data
## -------------------------

true_beta = 0.3
time_steps = 100L

## set time_steps value
tmb_simulator$replace$time_steps(time_steps)

## feed log(true_beta) to the simulator because we have
## already specified log-transformation of this parameter
observed_data = tmb_simulator$report(log(true_beta))
# observed_data = tmb_simulator$report()

## .mats_to_return is set to c("I", "N")
## compute incidence for observed data
I_obs = rpois(time_steps, subset(observed_data, matrix == "I", select = c(value)) %>% pull())
I_obs_times = subset(observed_data, matrix == "I", select = c(time)) %>% pull()

if (interactive()) {
  plot(I_obs, type = "l", las = 1)
}

## -------------------------
## update simulator with fake data to fit to
## -------------------------

tmb_simulator$update$matrices(
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
    , tmb_simulator$objective
    , numeric(1L)
  )
  plot(exp(log_betas), ll, type = "l", las = 1)
  abline(v = true_beta)
}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
tmb_simulator$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  print(tmb_simulator$current$params_frame())
  print(paste0("exp(default) ",exp(tmb_simulator$current$params_frame()$default)))
  print(paste0("exp(current) ",exp(tmb_simulator$current$params_frame()$current)))
  plot(I_obs, type = "l", las = 1)
  lines(tmb_simulator$report_values()[1:time_steps], col = "red")
}

## -------------------------
## exploring
## -------------------------

## plot population size (should be exponential)
if (interactive()) {
  times_to_plot = 1:time_steps
  pop_change = init_mats$get("birth_rate")-init_mats$get("death_rate")
  plot((init_mats$get("N"))*((1+pop_change)^times_to_plot), type = "l", las = 1, ylab='N')
  lines(tmb_simulator$report_values()[time_steps + (1:time_steps)], col = "red")
}

