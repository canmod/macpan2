#source("inst/starter_models/ww/tmb.R")
library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","ww",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
ww = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("Ia", "Ip", "Im", "Is", "W", "A")
)

## -------------------------
## parameterize model
## -------------------------

# interested in estimating asymptomatic relative transmission rate
ww$replace$params(spec$default$Ca,"Ca")
ww

## -------------------------
## specify objective function
## -------------------------


# negative log likelihood
obj_fn = ~ -sum(dpois(Ia_obs, rbind_time(Ia, Ia_obs_times)))

# update simulator to create new variables 
ww$update$matrices(
    Ia_obs = empty_matrix
  , Ia_obs_times = empty_matrix
)

# update simulator to include this function
ww$replace$obj_fn(obj_fn)


## -------------------------
## simulate fake data
## -------------------------

# Ca value to simulate data with
true_Ca = 0.8

## simulate observed data using true parameters
observed_data = ww$report(true_Ca)

## compute incidence for observed data
Ia_obs = rpois(time_steps, subset(observed_data, matrix == "Ia", select = c(value)) %>% pull())
Ia_obs_times = subset(observed_data, matrix == "Ia", select = c(time)) %>% pull()

if (interactive()) {
  plot(Ia_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

ww$update$matrices(
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
    , ww$objective
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
ww$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  
  ## estimate is close to true
  print(ww$current$params_frame())
  print(paste0("default Ca ",ww$current$params_frame()$default))
  print(paste0("current Ca ",ww$current$params_frame()$current))
  
  data_to_plot <- (cbind(as.numeric(Ia_obs),1:time_steps)
                   %>% data.frame()
                   %>% setNames(c("value","time"))
                   %>% mutate(type="observed")
  ) %>% union(ww$report() 
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
  ggplot(ww$report() %>% filter(grepl("I",matrix))%>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}

## W
if (interactive()) {
  ggplot(ww$report() %>% filter(matrix=="W")%>% select(time,value), aes(time,value))+
    geom_line()+
    theme_bw()+
    ylab("W")
}

## A
if (interactive()) {
  ggplot(ww$report() %>% filter(matrix=="A")%>% select(time,value), aes(time,value))+
    geom_line()+
    theme_bw()+
    ylab("A")
}

