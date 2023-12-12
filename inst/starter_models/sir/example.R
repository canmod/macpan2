source("inst/starter_models/sir/model.R")

## -------------------------
## simulate fake data
## -------------------------

true_beta = 0.4
observed_data = tmb_simulator$report(true_beta)
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
  betas = seq(from = 0.01, to = 1, length = 100)
  ll = vapply(
      betas
    , tmb_simulator$objective
    , numeric(1L)
  )
  plot(betas, ll, type = "l")
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
