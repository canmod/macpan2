# I have set this script up so that it should run. If you want to produce the error, uncomment line 188.

library(macpan2)
library(dplyr)
library(tidyverse)
library(lubridate)

# get macpan_base model with additional wastewater compartments
macpan_ww = Compartmental(file.path("../../../", "inst", "starter_models", "ww"))
N = 100
macpan_ww$labels$state()
macpan_ww$labels$flow()
macpan_ww$labels$other()
macpan_ww$flows()

# define zero vector function
zero_vector = function(nms) setNames(rep(0, length(nms)), nms)

# initiate state and flow vectors, and assign initial state values matching macpan 1.5 model to macpan 2 model
state = zero_vector(macpan_ww$labels$state())
state["S"] = 999995; state["E"] = 3; state["Ia"] = 1; state["Im"] = 1;
flow = zero_vector(macpan_ww$labels$flow())

# get simulator with initial states as in ICU1.csv
simulator = macpan_ww$simulators$tmb(
  time_steps = 100L
  , state = state
  , flow = flow
  , alpha = 1/3
  , beta0 = 1
  , sigma = 1/5.2
  , mu = 0.956
  , gamma_a = 1/7
  , gamma_p = 2
  , gamma_m = 1/7
  , gamma_s = 1/5.72
  , nonhosp_mort = 0
  , phi1 = 0.76
  , phi2 = 1/2
  , psi1 = 1/20
  , psi2 = 1/8
  , psi3 = 1/5
  , rho = 1/10
  , Ca = 2/3
  , Cp = 1
  , Cm = 1
  , Cs = 1
  , iso_m = 0
  , iso_s = 0
  , nu = 0.003
  , xi = 0.0333
  , N = 1e6
)
simulator$print$matrix_dims()
simulator$print$expressions()

## read champredon data so that i can fit the generating
## model to these data
champ_data <- read_csv("ChampData.csv")
clean_data <- (champ_data
   %>% arrange(ymd(champ_data$date))
   %>% filter(wwloc == "OTW")
   %>% select(date, event.type, n, sarscov2.conc.ww)
   %>% pivot_longer(!c(date, event.type))
   %>% filter(!(event.type == "hosp.adm" & name == "sarscov2.conc.ww"))
   %>% mutate(event.type = ifelse(name == "n", event.type, name))
   %>% select(!name)
   %>% rename(var = event.type)
   %>% mutate(var = ifelse(var == "clinical.cases", "report", var))
   %>% mutate(var = ifelse(var == "hosp.adm", "hosp", var))
   %>% mutate(var = ifelse(var == "sarscov2.conc.ww", "W", var))
   %>% mutate(var = ifelse(var == "hosp.occ", "H", var))
   %>% mutate(date = interval(ymd("2020-04-08"), ymd(date)) %/% days())
   %>% distinct()
   %>% mutate(time = date + 1)
)

# get observed waste vector and the corresponding time vector
obs_W = (clean_data %>% filter(var == "W"))$value
obs_W_time_steps = (clean_data %>% filter(var == "W"))$time

# obs_report = (clean_data %>% filter(var == "report"))$value
# obs_report_time_steps = (clean_data %>% filter(var == "report"))$time


# the following code is used if we are calibrating to simulated data instead of observed data:
# sims = simulator$report(.phases = "during")
# deterministic_prevalence = (sims
#                             %>% filter(row == "Ia")
#                             %>% pull(value)
# )[obs_time_steps]
# set.seed(1L)
# observed_I = rpois(30, deterministic_prevalence)

## Step 0: set the number of time-steps required to fit the model
simulator$replace$time_steps(460)

## Step 1: add observed data and declare matrices storing
##         the simulation history of variables to compare
##         with observed data, as well as a matrix for
##         storing log-likelihood values. need to make sure
##         that the simulation histories are saved, and
##         if you want you can return them as well as the
##         log likelihood values.
simulator$add$matrices(
  obs_W = obs_W
  , obs_W_time_steps = obs_W_time_steps
  , W_sd = 1
  # , obs_report = obs_report
  # , obs_report_time_steps = obs_report_time_steps
  , simulated_W = empty_matrix
  # , simulated_report = empty_matrix
  , log_lik = empty_matrix
  , .mats_to_save = c("simulated_W", "total_inflow") # "simulated_report"
  , .mats_to_return = c("log_lik", "simulated_W", "total_inflow") # "simulated_report"
  #, .dimnames = list(total_inflow = list(macpan_ww$labels$state(), ""))
)
simulator$print$matrix_dims()

## Step 2: collect simulated values into matrices to be
##         compared with data. the `.at = Inf` and
##         `.phase = "during"` indicates that this expression
##         should come at the end of the expressions evaluated
##         during each iteration of the simulation loop.
simulator$insert$expressions(
  #trajectory = simulated_I ~ 0.1 * sum(total_inflow[c(20, 21)])
  trajectory = simulated_W ~ W
  , .at = Inf  ## place the inserted expressions at the end of the expression list
  , .phase = "during"
)
simulator$print$expressions()

## Step 3: compute any values that will be part of the
##         objective function to be optimized. here we
##         have the log of the Poisson density of the
##         observed `W` values with mean (i.e. predicted)
##         value at the simulated `W` values. the
##         `rbind_time` function gathers together the
##         full simulation history of the `simulated_W`
##         matrix by binding together the rows at each
##         iteration.
simulator$insert$expressions(
  likelihood = log_lik ~
    (
      dnorm(
        log(obs_W),  ## observed values
        log(clamp(rbind_time(simulated_W, obs_W_time_steps))),  ## simulated values
        W_sd
      )
      # + dpois(
      #   obs_H,
      #   rbind_time(simulated_H, obs_H_time_steps)
      # )
    )
  , .at = Inf
  , .phase = "after"
)
simulator$print$expressions()

## Step 4: specify the objective function (very often
##         this will be minus the sum of the log likelihoods).
simulator$replace$obj_fn(~ -sum(log_lik))

## Step 5: declare (and maybe transform) parameters to be optimized,
##         as well as starting values for the parameters to be optimized

# simulator$add$matrices(
#   log_beta0 = log(0.6)
#   #, log_xi = log(0.6)
#   #, logit_gamma = qlogis(0.4)
# )
#
# simulator$insert$expressions(
#   beta0 ~ exp(log_beta0)
#   #, xi ~ exp(log_xi)
#   #, gamma ~ 1 / (1 + exp(-logit_gamma))
#   , .phase = "before"
# )

simulator$add$transformations(Log("beta0"))
simulator$add$transformations(Log("W_sd"))
simulator$replace$params(
  default = c(log(0.6), 0), # qlogis(0.4)),
  mat = c("log_beta0", "log_W_sd") #, "logit_gamma")
)

# simulator$replace$random(
#   default = qlogis(0.2),
#   mat = "logit_gamma"
# )

simulator$print$expressions()

## Step 6: use the engine object
plot(obs_W_time_steps, obs_W)

## THIS IS WHERE ERROR OCCURS, MACPAN ERROR #245:
## cannot recycle rows and/or columns because the
## input is inconsistent with the recycling request
#length(obs_W)
#length(filter(simulator$report(.phases = "after"), matrix == "log_lik")$value)

#simulator$report()

# (simulator$report()
#   %>% filter(matrix == "simulated_W")
#   %>% View
# )
simulator$optimize$nlminb()
simulator$optimize$optim()
simulator$current$params_frame()


# lines(1:100, filter(simulator$report(.phases = "during"), matrix == "state", row == "W")$value)
# simulator$cache$invalidate()
# lines(1:100, filter(simulator$report(.phases = "during"), matrix == "state", row == "W")$value, col = "red")
# simulator$optimization_history$get()
