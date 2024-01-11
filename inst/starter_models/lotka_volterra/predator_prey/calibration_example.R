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
pred_prey = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("X","Y")
)

## -------------------------
## parameterize model
## -------------------------

# interested in estimating delta - predator growth from predation
pred_prey$replace$params(spec$default$delta,"delta")

pred_prey

## -------------------------
## specify objective function
## -------------------------

# negative log likelihood
obj_fn = ~ -sum(dpois(X_obs, rbind_time(X, X_obs_times)))

# update simulator to create new variables 
pred_prey$update$matrices(
    X_obs = empty_matrix
  , X_obs_times = empty_matrix
)

# update simulator to include this function
pred_prey$replace$obj_fn(obj_fn)


## -------------------------
## simulate fake data
## -------------------------

# delta value to simulate data with
true_delta = 2/200

## simulate observed data using true parameters
observed_data = pred_prey$report(true_delta)

## compute incidence for observed data
X_obs = rpois(time_steps, subset(observed_data, matrix == "X", select = c(value)) %>% pull())
X_obs_times = subset(observed_data, matrix == "X", select = c(time)) %>% pull()

if (interactive()) {
  plot(X_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

# pred_prey$update$matrices(
#     X_obs = X_obs
#   , X_obs_times = X_obs_times
# )

## -------------------------
## plot likelihood surface (curve)
## -------------------------
# 
# if (interactive()) {
#   
#   Ca_seq = seq(from = 0.1, to = 1, length = 100)
#   
#   ll = vapply(
#     Ca_seq
#     , pred_prey$objective
#     , numeric(1L)
#   )
#   dat_for_plot <- (cbind(Ca_seq, ll)
#                    %>% data.frame()
#                    
#   )
#   
#   ggplot(dat_for_plot, aes(Ca_seq, ll)) +
#     geom_line()+
#     ## add true parameter values to compare
#     geom_vline(xintercept = true_Ca, col='red')+
#     xlab("Ca")
#   
# }

## -------------------------
## fit parameters
## -------------------------
# 
# ## optimize and check convergence
# pred_prey$optimize$nlminb()
# 
# ## plot observed vs predicted
# if (interactive()) {
#   
#   ## estimate is close to true
#   print(pred_prey$current$params_frame())
#   print(paste0("default Ca ",pred_prey$current$params_frame()$default))
#   print(paste0("current Ca ",pred_prey$current$params_frame()$current))
#   
#   data_to_plot <- (cbind(as.numeric(X_obs),1:time_steps)
#                    %>% data.frame()
#                    %>% setNames(c("value","time"))
#                    %>% mutate(type="observed")
#   ) %>% union(pred_prey$report() 
#               %>% filter(matrix=="Ia") 
#               %>% select(time,value)
#               %>% mutate(type="predicted")
#   )
#   
#   ggplot(data_to_plot, aes(x=time, y=value, col=type))+
#     geom_line()+
#     theme_bw()+
#     ylab("Ia")
#   
# }
# 


