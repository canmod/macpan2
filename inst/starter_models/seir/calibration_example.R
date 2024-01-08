source("inst/starter_models/seir/tmb.R")
library(ggplot2)
library(dplyr)


## -------------------------
## define simulator
## -------------------------

true_beta = 0.5
time_steps = 100L

# define objective function
obj_fn = ObjectiveFunction(~ -sum(log_likelihood))

# old simulator object
old_tmb_simulator = TMBModel(
  init_mats = init_mats
  , expr_list = expr_list
  , obj_fn = obj_fn
)$simulator()

# simulator object
tmb_simulator = mp_simulator(  
  model = model_spec
 , time_steps = time_steps
 , outputs = c("I","E")
 # if you want to update defaults (use this for log_beta?)
 , default = list(beta ~ log(model_spec$default$beta))
)


## -------------------------
## parameterize model
## -------------------------

tmb_simulator$update$transformations(Log("beta"))
#tmb_simulator$replace$params(log(model_spec$default$beta), "log_beta")
tmb_simulator

old_tmb_simulator$update$transformations(Log("beta"))
old_tmb_simulator$replace$params(log(model_spec$default$beta), "log_beta")
old_tmb_simulator

identical(old_tmb_simulator, tmb_simulator)

# better way to update transformations in new simulator?

## -------------------------
## simulate fake data
## -------------------------

## feed log(true_beta) to the simulator because we have
## already specified log-transformation of this parameter
observed_data = tmb_simulator$report(log(true_beta))

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
  # no output?
  print(tmb_simulator$current$params_frame())
  # print(paste0("exp(default) ",exp(tmb_simulator$current$params_frame()$default)))
  # print(paste0("exp(current) ",exp(tmb_simulator$current$params_frame()$current)))
  plot(I_obs, type = "l", las = 1)
  lines(tmb_simulator$report_values(), col = "red")
}

## -------------------------
## exploring
## -------------------------

## plot exposed density
if (interactive()) {
  plot(tmb_simulator$report_values()[time_steps + (1:time_steps)], type = "l", las = 1, ylab='E')
}

