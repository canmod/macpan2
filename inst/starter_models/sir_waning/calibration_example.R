#source("inst/starter_models/sir_waning/tmb.R")
library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","sir_waning",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
sir_waning = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("I","S", "waning_immunity")
)

## -------------------------
## specify objective function
## -------------------------

# negative log likelihood
obj_fn = ~ -sum(dpois(I_obs, rbind_time(I, I_obs_times)))

# update simulator to create new variables 
# I_obs and I_obs_times and initialize
sir_waning$update$matrices(
    I_obs = empty_matrix
  , I_obs_times = empty_matrix
)

# update simulator to include this function
sir_waning$replace$obj_fn(obj_fn)

## -------------------------
## parameterize model
## -------------------------

sir_waning$update$transformations(Log("beta"))

# choose which parameter(s) to estimate - log(beta) and phi
sir_waning$replace$params(c(log(spec$default$beta),spec$default$phi),
                          c("log_beta","phi")
                          )

sir_waning


## -------------------------
## simulate fake data
## -------------------------

# beta value to simulate data with
true_beta = 0.3

# phi value to simulate data with
true_phi = 0.09

## simulate observed data using true parameters
observed_data = sir_waning$report(c(log(true_beta),true_phi))

## compute incidence for observed data
I_obs = rpois(time_steps, subset(observed_data, matrix == "I", select = c(value)) %>% pull())
I_obs_times = subset(observed_data, matrix == "I", select = c(time)) %>% pull()

if (interactive()) {
  plot(I_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

sir_waning$update$matrices(
    I_obs = I_obs
  , I_obs_times = I_obs_times
)

## -------------------------
## plot likelihood surface (curve)
## -------------------------

# plot surface as contours
if (interactive()) {
  log_betas = seq(from = log(0.1), to = log(1), length = 100)
  phis = seq(from = 1e-3, to = 0.2, length = 100)
  x_y = expand.grid(log_betas, phis) %>% setNames(c("log_betas","phis"))

  ll = apply(
      x_y
    , 1
    , function(z) {sir_waning$objective(z["log_betas"], z["phis"])}
  ) 
  
  dat_for_plot <- cbind(x_y, ll)
  
  ggplot(dat_for_plot, aes(log_betas, phis, z=ll)) +
    geom_contour_filled()+
    ## add true parameter values to compare
    geom_vline(xintercept = log(true_beta), col='red')+
    geom_hline(yintercept = true_phi, col='red')


}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
## warning message, but converges
sir_waning$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  
  ## estimates are close to true values
  print(sir_waning$current$params_frame())
  print(paste0("exp(default beta) ",exp(sir_waning$current$params_frame()$default[1])))
  print(paste0("exp(current beta) ",exp(sir_waning$current$params_frame()$current[1])))
  print(paste0("default phi ",sir_waning$current$params_frame()$default[2]))
  print(paste0("current phi ",sir_waning$current$params_frame()$current[2]))
  
  
  data_to_plot <- (cbind(as.numeric(I_obs),1:time_steps)
                   %>% data.frame()
                   %>% setNames(c("value","time"))
                   %>% mutate(type="observed")
  ) %>% union(sir_waning$report() 
              %>% filter(matrix=="I") 
              %>% select(time,value)
              %>% mutate(type="predicted")
  )

  ggplot(data_to_plot, aes(x=time, y=value, col=type))+
    geom_line()+
    theme_bw()+
    ylab("I")

}

## -------------------------
## exploring
## -------------------------

## plot S
if (interactive()) {
  plot(sir_waning$report() %>% filter(matrix=="S") %>% select(time,value),
       type="l", las = 1, ylab='S')
}

## plot waning immunity (phi*R)
if (interactive()) {
  plot(sir_waning$report() %>% filter(matrix=="waning_immunity") %>% select(time,value), type = "l", las = 1, ylab='Waning Immunity')
}