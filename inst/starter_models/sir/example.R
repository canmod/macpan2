source("inst/starter_models/sir/tmb.R")
sir = mp_simulator(spec, time_steps = 50L, outputs = "I")

## -------------------------
## parameterize model
## -------------------------

sir$update$transformations(Log("beta"))
sir$replace$params(log(init_mats$get("beta")), "log_beta")
sir  ## note the new expression before the simulation loop

## -------------------------
## simulate fake data
## -------------------------

time_steps = 100L
true_beta = 0.4

## set time_steps value
sir$replace$time_steps(time_steps)

## feed log(true_beta) to the simulator because we have
## already specified log-transformation of this parameter
observed_data = sir$report(log(true_beta))

## .mats_to_return is set to "I", so observed_data$value is
## the prevalence (density of I) over time
observed_data$value = rpois(time_steps, observed_data$value)

if (interactive()) {
  plot(observed_data$value, type = "l", las = 1)
}

## -------------------------
## update simulator with fake data to fit to
## -------------------------

sir$update$matrices(
    I_obs = observed_data$value
  , I_obs_times = observed_data$time
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
  plot(observed_data$value, type = "l", las = 1)
  lines(sir$report_values(), col = "red")
}
