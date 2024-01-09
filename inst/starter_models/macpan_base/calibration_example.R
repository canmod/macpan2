#source("inst/starter_models/macpan_base/tmb.R")
library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","macpan_base",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
macpan_base = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("Ia", "Ip", "Im", "Is")
)

## -------------------------
## parameterize model
## -------------------------

# interested in estimating asymptomatic relative transmission rate
macpan_base$replace$params(spec$default$Ca,"Ca")

macpan_base

## -------------------------
## specify objective function
## -------------------------


# negative log likelihood
obj_fn = ~ -sum(dpois(Ia_obs, rbind_time(Ia, Ia_obs_times)))

# update simulator to create new variables 
macpan_base$update$matrices(
    Ia_obs = empty_matrix
  , Ia_obs_times = empty_matrix
)

# update simulator to include this function
macpan_base$replace$obj_fn(obj_fn)


## -------------------------
## simulate fake data
## -------------------------

# Ca value to simulate data with
true_Ca = 0.8

## simulate observed data using true parameters
observed_data = macpan_base$report(true_Ca)

## compute incidence for observed data
Ia_obs = rpois(time_steps, subset(observed_data, matrix == "Ia", select = c(value)) %>% pull())
Ia_obs_times = subset(observed_data, matrix == "Ia", select = c(time)) %>% pull()

if (interactive()) {
  plot(Ia_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

macpan_base$update$matrices(
    Ia_obs = Ia_obs
  , Ia_obs_times = Ia_obs_times
)

## -------------------------
## plot likelihood surface (curve)
## -------------------------

# plot surface as contours
if (interactive()) {

  Ca_seq = seq(from = 0.1, to = 1, length = 100)
  
  ll = vapply(
      Ca_seq
    , macpan_base$objective
    , numeric(1L)
  )
  dat_for_plot <- (cbind(Ca_seq, ll)
                   %>% data.frame()

  )
  
  ggplot(dat_for_plot, aes(Ca_seq, ll)) +
    geom_line()+
    ## add true parameter values to compare
    geom_vline(xintercept = true_Ca, col='red')+
    xlab("Ca")

}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
macpan_base$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  
  ## estimate is close to true
  print(macpan_base$current$params_frame())
  print(paste0("default Ca ",macpan_base$current$params_frame()$default))
  print(paste0("current Ca ",macpan_base$current$params_frame()$current))
  
  data_to_plot <- (cbind(as.numeric(Ia_obs),1:time_steps)
                   %>% data.frame()
                   %>% setNames(c("value","time"))
                   %>% mutate(type="observed")
  ) %>% union(macpan_base$report() 
              %>% filter(matrix=="Ia") 
              %>% select(time,value)
              %>% mutate(type="predicted")
  )
  
  ggplot(data_to_plot, aes(x=time, y=value, col=type))+
    geom_line()+
    theme_bw()+
    ylab("Ia")
  
}

## -------------------------
## exploring
## -------------------------

## all infectious compartments
if (interactive()) {
  ggplot(macpan_base$report() %>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}
