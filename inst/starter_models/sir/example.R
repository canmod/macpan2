source("inst/starter_models/sir/model.R")

## -------------------------
## parameterize model
## -------------------------

tmb_simulator$update$transformations(Log("beta"))
tmb_simulator$replace$params(log(init_mats$get("beta")), "log_beta")
tmb_simulator  ## note the new expression before the simulation loop

## -------------------------
## simulate fake data
## -------------------------

time_steps = 100L
true_beta = 0.4

tmb_simulator$replace$time_steps(100L)
observed_data = tmb_simulator$report(log(true_beta))
observed_data$value = rpois(100, observed_data$value)

if (interactive()) {
  plot(observed_data$value, type = "l")
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

tmb_simulator$update$matrices(
    I_obs = observed_data$value
  , I_obs_times = observed_data$time
)


## -------------------------
## plot likelihood surface
## -------------------------

if (interactive()) {
  log_betas = seq(from = log(0.1), to = log(1), length = 100)
  ll = vapply(
      log_betas
    , tmb_simulator$objective
    , numeric(1L)
  )
  plot(exp(log_betas), ll, type = "l")
  abline(v = true_beta)
}


## -------------------------
## optimize the model
## -------------------------

tmb_simulator$optimize$nlminb()

if (interactive()) {
  plot(observed_data$value, type = "l")
  lines(tmb_simulator$report_values(), col = "red")
}
