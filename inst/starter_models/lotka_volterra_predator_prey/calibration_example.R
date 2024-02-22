library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

#source(system.file("starter_models/lotka_volterra_predator_prey/tmb.R", package = "macpan2"))
specs = mp_tmb_library("starter_models","lotka_volterra_predator_prey", package = "macpan2", alternative_specs = TRUE)
spec = specs[[1L]]

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
lv_pred_prey$get$initial("Y")

## define new simulator and update defaults
lv_exp_prey = mp_simulator(
    model = spec
  , time_steps = 10L
  , outputs = c("X","Y")
  # by setting initial predator population to 0
  # prey dynamics should be strictly exponential growth
  , default = list("K_inverse" = 0,"Y" = 0)
) 
lv_exp_prey$get$initial("K_inverse")
lv_exp_prey$get$initial("Y")
lv_exp_prey

## compute exponential function 
exp_growth <- (cbind(1:10, as.numeric(lv_exp_prey$get$initial("X"))*(1+as.numeric(lv_exp_prey$get$initial("alpha")))^c(1:10))
               %>% data.frame()
               %>% setNames(c("time","value"))
)

if (interactive()) {
ggplot(lv_exp_prey$report() %>% filter(matrix=="X"), aes(time,value))+
  geom_point()+
  geom_line(data=exp_growth,col="red")+
  theme_bw()+
  ylab("prey")
}

## Holling functions

## check current functional response
spec$during$fr

## simulating data with different functional responses
fr_names <- c("holling_1", "holling_2", "holling_3")

if (interactive()) {
fr_sims <- lapply(fr_names,function(x){
  (mp_simulator(
      model = specs[[x]]
    , time_steps = 100L
    , outputs = c("X","Y"))
  %>% mp_trajectory()
  %>% mutate(matrix = case_match(matrix
                                 , "X" ~ "Prey"
                                 , "Y" ~ "Predator"),
             response = x)
  )
 }
) %>% bind_rows()
}
## visualizing simulated data
if (interactive()) {
  ggplot(fr_sims, aes(time,value,linetype=matrix,col=response))+
    geom_line()+
    theme_bw()+
    ylim(c(0,1/spec$default$K_inverse)) #ymax = carrying capacity of prey
}

