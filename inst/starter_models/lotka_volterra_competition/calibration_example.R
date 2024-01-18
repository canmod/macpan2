library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","lotka_volterra_competition",package="macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# simulator object
lv_comp = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = c("X","Y")
)

## -------------------------
## parameterize model
## -------------------------

# interested in estimating ayx - effect of species X on Y
lv_comp$update$transformations(Log("ayx"))
lv_comp$replace$params(log(spec$default$ayx),"log_ayx")
lv_comp

## -------------------------
## specify objective function
## -------------------------

# negative log likelihood
obj_fn = ~ -sum(dpois(Y_obs, rbind_time(Y, Y_obs_times)))

# update simulator to create new variables 
lv_comp$update$matrices(
    Y_obs = empty_matrix
  , Y_obs_times = empty_matrix
)

# update simulator to include this function
lv_comp$replace$obj_fn(obj_fn)


## -------------------------
## simulate fake data
## -------------------------

# ayx value to simulate data with (species X has a carrying capacity of 200)
true_ayx = 0.8/200

## simulate observed data using true parameters
observed_data = lv_comp$report(log(true_ayx))

## compute observed data - convert density to number of individuals
Y_obs = rpois(time_steps, subset(observed_data, matrix == "Y", select = c(value)) %>% pull())
Y_obs_times = subset(observed_data, matrix == "Y", select = c(time)) %>% pull()

if (interactive()) {
  plot(Y_obs_times, Y_obs, type = "l", las = 1)
}


## -------------------------
## update simulator with fake data to fit to
## -------------------------

lv_comp$update$matrices(
    Y_obs = Y_obs
  , Y_obs_times = Y_obs_times
)

## -------------------------
## plot likelihood surface (curve)
## -------------------------


if (interactive()) {
  
  log_ayx = log(seq(from = 0.1, to = 1.5, length = 100)/200)
  
  ll = vapply(
      log_ayx
    , lv_comp$objective
    , numeric(1L)
  )
  dat_for_plot <- (cbind(log_ayx, ll)
                   %>% data.frame()
                   
  )
  
  ggplot(dat_for_plot, aes(log_ayx, ll)) +
    geom_line()+
    ## add true parameter values to compare
    geom_vline(xintercept = log(true_ayx), col='red')+
    theme_bw()+
    xlab("log(ayx)")
  
}

## -------------------------
## fit parameters
## -------------------------

## optimize and check convergence
lv_comp$optimize$nlminb()

## plot observed vs predicted
if (interactive()) {
  
  ## estimate is close to true
  print(lv_comp$current$params_frame())
  print(paste0("exp(default ayx) ",exp(lv_comp$current$params_frame()$default)))
  print(paste0("exp(current ayx) ",exp(lv_comp$current$params_frame()$current)))
  print(paste0("true ayx ",true_ayx))
  
  data_to_plot <- (cbind(as.numeric(Y_obs), Y_obs_times)
                   %>% data.frame()
                   %>% setNames(c("value","time"))
                   %>% mutate(type="observed")
  ) %>% union(lv_comp$report() 
              %>% filter(matrix=="Y") 
              %>% select(time,value)
              %>% mutate(type="predicted")
  )
  
  ggplot(data_to_plot, aes(x=time, y=value, col=type))+
    geom_line()+
    theme_bw()+
    ylab("Y")
  
}

## -------------------------
## exploring
## -------------------------

## both species
if (interactive()) {
  ggplot(lv_comp$report() %>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    
    # plot carrying capacity of each species
    geom_hline(aes(yintercept = lv_comp$get$initial("axx")^(-1),col='X'), linetype="dashed")+
    annotate("text",label="Kx",x=time_steps,y=lv_comp$get$initial("axx")^(-1),vjust=-1)+
    geom_hline(aes(yintercept = lv_comp$get$initial("ayy")^(-1),col='Y'), linetype="dashed")+
    annotate("text",label="Ky",x=time_steps,y=lv_comp$get$initial("ayy")^(-1),vjust=-1)+
    
    guides(label="none")+
    labs(col = "species")+
    ylab("individuals")

}

