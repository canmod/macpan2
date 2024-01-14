source("inst/starter_models/lotka_volterra/predator_prey/tmb.R")
library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

#spec = mp_tmb_library("starter_models","lotka_volterra","predator_prey",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
lv_pred_prey = mp_simulator(
    model = spec
  , time_steps = time_steps
  , outputs = c("X","Y")
)

## -------------------------
## parameterize model
## -------------------------

# interested in estimating delta - predator growth from predation
lv_pred_prey$update$transformations(Log("delta"))
lv_pred_prey$replace$params(log(spec$default$delta),"log_delta")
lv_pred_prey


## -------------------------
## specify objective function
## -------------------------

# negative log likelihood
obj_fn = ~ -sum(dpois(X_obs, rbind_time(X, X_obs_times)))

# update simulator to create new variables 
lv_pred_prey$update$matrices(
    X_obs = empty_matrix
  , X_obs_times = empty_matrix
)

# update simulator to include this function
lv_pred_prey$replace$obj_fn(obj_fn)


## -------------------------
## simulate fake data
## -------------------------

# delta value to simulate data with
true_delta = 2.5/10

## simulate observed data using true parameters
observed_data = lv_pred_prey$report(log(true_delta))

## compute incidence for observed data
X_obs = rpois(time_steps, subset(observed_data, matrix == "X", select = c(value)) %>% pull())
X_obs_times = subset(observed_data, matrix == "X", select = c(time)) %>% pull()

if (interactive()) {
  plot(X_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

lv_pred_prey$update$matrices(
    X_obs = X_obs
  , X_obs_times = X_obs_times
)

## -------------------------
## plot likelihood surface (curve)
## -------------------------

if (interactive()) {

  log_delta = seq(from = log(0.05), to = log(0.5), length = 100)

  ll = vapply(
      log_delta
    , lv_pred_prey$objective
    , numeric(1L)
  )
  dat_for_plot <- (cbind(log_delta, ll)
                   %>% data.frame()

  )

  ggplot(dat_for_plot, aes(log_delta, ll)) +
    geom_line()+
    ## add true parameter values to compare
    geom_vline(xintercept = log(true_delta), col='red')+
    xlab("log(delta)")

}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
lv_pred_prey$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {

  print(lv_pred_prey$current$params_frame())
  print(paste0("exp(default delta) ",exp(lv_pred_prey$current$params_frame()$default)))
  print(paste0("exp(current delta) ",exp(lv_pred_prey$current$params_frame()$current)))
  print(paste0("true delta ",true_delta))

  data_to_plot <- (cbind(as.numeric(X_obs),1:time_steps)
                   %>% data.frame()
                   %>% setNames(c("value","time"))
                   %>% mutate(type="observed")
  ) %>% union(lv_pred_prey$report()
              %>% filter(matrix=="X")
              %>% select(time,value)
              %>% mutate(type="predicted")
  )

  ggplot(data_to_plot, aes(x=time, y=value, col=type))+
    geom_line()+
    theme_bw()+
    ylab("X")

}


## -------------------------
## exploring
## -------------------------

## dynamics
if (interactive()) {
  ggplot(lv_pred_prey$report() %>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
  
}

## exponential prey growth instead of logistic
## set K_inverse=0
lv_pred_prey$get$initial("K_inverse")

## define new simulator and update default
lv_exp_prey = mp_simulator(
    model = spec
  , time_steps = time_steps
  , outputs = c("X","Y")
  , default = list("K_inverse" = 0, "alpha"=0.1)
)
lv_exp_prey$get$initial("K_inverse")

## better way to get initial population size
#lv_exp_prey$report() %>% filter(matrix=="X" & time==0) %>% select(value)
## set Y0=0
ggplot(lv_exp_prey$report() %>% filter(matrix=="X"), aes(time,value))+
  geom_line()+
  theme_bw()+
  ylab("prey")



## Holling functions

