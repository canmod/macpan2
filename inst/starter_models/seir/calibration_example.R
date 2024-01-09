#source("inst/starter_models/seir/tmb.R")
library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","seir",package="macpan2")
spec 

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
seir = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("S", "E", "I","R")
)

## -------------------------
## specify objective function
## -------------------------

# negative log likelihood
# choosing to use E to estimate alpha
obj_fn = ~ -sum(dpois(E_obs, rbind_time(E, E_obs_times)))

# update simulator to create new variables 
seir$update$matrices(
    E_obs = empty_matrix
  , E_obs_times = empty_matrix
)

# update simulator to include this function
seir$replace$obj_fn(obj_fn)


## -------------------------
## parameterize model
## -------------------------

# choose which parameter(s) to estimate
# 1/alpha = time spent in E compartment
seir$replace$params(spec$default$alpha,"alpha")


## -------------------------
## simulate fake data
## -------------------------

# alpha value to simulate data with
true_alpha = 1/5

## simulate observed data using true parameters
observed_data = seir$report(true_alpha)

## compute exposure for each time step
E_obs = rpois(time_steps, subset(observed_data, matrix == "E", select = c(value)) %>% pull())
E_obs_times = subset(observed_data, matrix == "E", select = c(time)) %>% pull()

if (interactive()) {
  plot(E_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

seir$update$matrices(
    E_obs = E_obs
  , E_obs_times = E_obs_times
)


## -------------------------
## plot likelihood surface (curve)
## -------------------------

if (interactive()) {
  alphas = seq(from = 1/100, to = 1, length = 100)
  ll = vapply(
      alphas
    , seir$objective
    , numeric(1L)
  )
  plot(alphas, ll, type = "l", las = 1)
  abline(v = true_alpha)
}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
## converges with a warning
seir$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  print(seir$current$params_frame())
  print(paste0("default alpha ",seir$current$params_frame()$default))
  print(paste0("current alpha ",seir$current$params_frame()$current))
  plot(E_obs, type = "l", las = 1)
  lines(seir$report() %>% filter(matrix=="E") %>% select(time,value), col = "red")
}

## -------------------------
## exploring
## -------------------------

## plot all densities
if (interactive()) {
  ggplot(seir$report(), aes(x=time, y=value, colour=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}

