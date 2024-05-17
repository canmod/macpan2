library(macpan2)
library(ggplot2)
library(dplyr)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models","macpan_base",package="macpan2")
spec

## -------------------------
## simulate fake data
## -------------------------

# set number of time steps in simulation
time_steps = 100L

# Ca value to simulate data with
true_Ca = 0.8

# infectious compartment names
I_labels = c("Ia", "Ip", "Im", "Is")

# simulator object
macpan_base = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = I_labels
  , default = list(Ca = true_Ca)
)

# simulated data
sim_data = (macpan_base 
            |> mp_trajectory() 
            |> mutate(across(value, ~ rpois(n(), .)))
)

# visualize simulated prevalence
if (interactive()) {
  ggplot(sim_data)+
    geom_point(aes(time,value))+
    theme_bw()+
    facet_wrap(vars(matrix),scales = 'free')
  
}


## -------------------------
## parameterize model
## -------------------------

mb_calib = mp_tmb_calibrator(
  # add log transformation for Ca parameter
    spec = (spec 
      |> mp_tmb_insert(phase = "before", at = 1L
                       , expressions = list(Ca ~ exp(log_Ca))
                       , default = list(log_Ca = log(1e-2)))
    )
  , data = sim_data
  , traj = I_labels
  , par = "log_Ca"
)

## -------------------------
## fit model
## -------------------------

# optimize
mp_optimize(mb_calib)

# get fitted data
fitted_data = mp_trajectory_sd(mb_calib, conf.int = TRUE)

# check estimate, close to true
mp_tmb_coef(mb_calib) %>% mutate(estimate = exp(estimate))

# fit looks reasonable
if (interactive()) {
(ggplot(sim_data, aes(time,value))
  + geom_point()
  + geom_line(aes(time, value)
              , data = fitted_data
              , colour = "red"
  )
  + geom_ribbon(aes(time, ymin = conf.low, ymax = conf.high)
                , data = fitted_data
                , alpha = 0.2
                , colour = "red"
  )
  + facet_wrap(vars(matrix),scales = 'free')
  + theme_bw()
  + ylab("prevalence")
)
}


## -------------------------
## exploring
## -------------------------

## plotting Ia(t) vs. Ia(t+1) - should be linear?
if (interactive()) {
  
  Ia_shifted <- (macpan_base
                 %>% mp_trajectory() 
                 %>% filter(matrix=="Ia") 
                 %>% mutate(Ia_lead = lead(value))
                 %>% rename(Ia=value)
                )
  
  # linear model might not be appropriate
  # what are the assumptions about error here?
  lm_fit <- lm(data = Ia_shifted,Ia_lead ~ Ia)
  summary(lm_fit)
  
  ggplot(Ia_shifted, aes(Ia, Ia_lead))+
    geom_line()+
    
    #add lm
    # geom_abline(intercept = lm_fit$coefficients[1],
    #             slope = lm_fit$coefficients[2], col="red")+
  
    geom_smooth(method = "lm", se = FALSE, col="blue")+
    theme_bw()
  
}


## all infectious compartments
if (interactive()) {
  ggplot(macpan_base %>% mp_trajectory() %>% select(time,value,matrix), aes(time,value,col=matrix))+
    geom_line()+
    theme_bw()+
    ylab("individuals")
}
