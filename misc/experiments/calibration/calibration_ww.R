# currently, calibrating to hospital occupancy data is working, but it requires 
# seemingly random breakpoints, where small changes ruin the fit, and the waste combined 
# hosp occ and waste fit is bad with these breakpoints

library(macpan2)
library(dplyr)
library(tidyverse)
library(lubridate)

# set breakpoint times based on Champredon breakpoint dates

breakpoint_dates = c(ymd(20200310), ymd(20200330), ymd(20200419), ymd(20200608),
                     ymd(20200713), ymd(20200728), ymd(20200827), ymd(20201016),
                     ymd(20201115), ymd(20210124), ymd(20210213), ymd(20210305),
                     ymd(20210330), ymd(20210504), ymd(20210514))

# add 120 days to breakpoint times and two random 
# start breakpoint times, so that calibration works, 
# does not make sense to me at all
breakpoint_times = interval(ymd(20200310), breakpoint_dates) %/% days() + 1 + 120
breakpoint_times = c(25, 60, breakpoint_times)

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
simulator <- macpan_ww$simulators$tmb(
  time_steps = 460L
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
   %>% mutate(time = date + 1 + 60)
)

# get observed waste vector and the corresponding time vector
obs_W = (clean_data %>% filter(var == "W"))$value
obs_W_time_steps = (clean_data %>% filter(var == "W"))$time

# get observed hosp_occ vector and the corresponding time vector
obs_H = (clean_data %>% filter(var == "H"))$value
obs_H_time_steps = (clean_data %>% filter(var == "H"))$time

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
simulator$replace$time_steps(460 + 60)

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
  , obs_H = obs_H
  , obs_H_time_steps = obs_H_time_steps
  #, H_sd = 1
  # , obs_report = obs_report
  # , obs_report_time_steps = obs_report_time_steps
  , simulated_W = empty_matrix
  , simulated_H = empty_matrix
  , clamped_H = empty_matrix
  , clamped_W = empty_matrix
  #, simulated_Adm = empty_matrix
  #, simulated_reports = empty_matrix
  # , simulated_report = empty_matrix
  , log_lik = empty_matrix
  , .mats_to_save = c("simulated_W", "simulated_H", "total_inflow", "beta_changepoints", "xi", "nu") # "simulated_report"
  , .mats_to_return = c("log_lik", "simulated_W", "simulated_H", "total_inflow", "beta_changepoints", "xi", "nu") # "simulated_report"
  #, .dimnames = list(total_inflow = list(macpan_ww$labels$state(), ""))
)
simulator$print$matrix_dims()

# add time varying parameter tools
simulator$add$matrices(
  beta_changepoints = breakpoint_times # sort(c(5, 10, 20, 30, 40, breakpoint_times, 100, 110, 120, 130, 140)) #, 100, 120, 130, 140, 150, 190, 200, 210))# breakpoint_times)) # c(0,10,20,30,40,50,100,200,300) #breakpoint_times
  , beta_values = 1:(length(breakpoint_times)) #*0.09 #c(0.1, 0.11, 0.2, 0.3, 0.4, 0.5, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15)
  , beta_pointer = 0
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

simulator$insert$expressions(
  trajectory = simulated_H ~ H + H2 + ICUs + ICUd
  , .at = Inf  ## place the inserted expressions at the end of the expression list
  , .phase = "during"
)
simulator$print$expressions()

# add time-varying parameter expressions
simulator$insert$expressions(
  beta_pointer ~ time_group(beta_pointer, beta_changepoints),
  beta0 ~ beta_values[beta_pointer],
  .phase = "during"
)
# simulator$insert$expressions(
#   .phase = "during"
# )
simulator$print$expressions()

## Step 3: compute any values that will be part of the
##         objective function to be optimized. here we
##         have the log of the Poisson density of the
##         observed `H` values with mean (i.e. predicted)
##         value at the simulated `H` values. the
##         `rbind_time` function gathers together the
##         full simulation history of the `simulated_H`
##         matrix by binding together the rows at each
##         iteration.
simulator$insert$expressions(
    simulated_H ~ rbind_time(simulated_H, obs_H_time_steps)
  , simulated_W ~ rbind_time(simulated_W, obs_W_time_steps)
    #simulated_H ~ log(1 + 1e-12 + exp(rbind_time(simulated_H, obs_H_time_steps)))
  #, simulated_H ~ exp(rbind_time(simulated_H, obs_H_time_steps))
  , clamped_H ~ 1e-12 + 0.5 * ((simulated_H - 1e-12) + ((simulated_H - 1e-12)^2 + (2*1e-6 - 1e-12)^2 - (1e-12)^2)^0.5)
  , clamped_W ~ 1e-12 + 0.5 * ((simulated_W - 1e-12) + ((simulated_W - 1e-12)^2 + (2*1e-6 - 1e-12)^2 - (1e-12)^2)^0.5)
  , likelihood = log_lik ~
    (
      # dnorm(
      #   log(obs_W),  ## observed values
      #   log(clamped_W), #log(clamp(rbind_time(simulated_W, obs_W_time_steps))),  ## simulated values
      #   W_sd
      # )
      # +
      # dpois(
      #   obs_W,
      #   clamp(rbind_time(simulated_W, obs_W_time_steps))
      # )
      #+
      #dpois(obs_W, clamped_W)
      #+
      dpois(obs_H, clamped_H)
      #+
      # dnorm(
      #   log(obs_H),
      #   log(clamp(rbind_time(simulated_H, obs_H_time_steps))),
      #   H_sd
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

#simulator$add$transformations(Log("beta0"))
simulator$add$transformations(Log("beta_values"))
simulator$add$transformations(Log("xi"))
simulator$add$transformations(Log("nu"))
simulator$add$transformations(Log("mu"))
simulator$add$transformations(Log("W_sd"))
#simulator$add$transformations(Log("H_sd"))
simulator$replace$params_frame(readr::read_csv("opt_parameters.csv", comment = "#"))
# simulator$replace$params(
#   default = c(rep(log(mean(simulator$get$initial("beta_values"))), 15), 1), #, 1),
#   mat = c(rep("log_beta_values", 15), "log_W_sd"), #, "log_H_sd"), #length(breakpoint_times)
#   row = c(0:14, 0) # 16 #length(breakpoint_times)
# )

simulator$print$expressions()
# simulator$report() %>% pull(matrix) %>% unique
# View(simulator$report(.phases = "after") %>% filter(matrix == "log_lik"))
# View(simulator$report(.phases = "after") %>% filter(matrix == "simulated_H"))

## Step 6: use the engine object
#plot(obs_W_time_steps, obs_W, xlim = c(1, 460 + 60))
plot(obs_H_time_steps, obs_H, xlim = c(1, 460 + 60))

simulator$optimize$nlminb()
#simulator$optimize$optim()
simulator$current$params_frame()

simulator$optimization_history$get()

#lines(1:(460+60), filter(simulator$report(.phases = "during"), matrix == "state", row == "W")$value, col = "red")


# create plot for hosp occ calibration which includes H, H2, ICUs, and ICUd
my_tib <- pivot_wider(filter(simulator$report(.phases = "during"),
                             matrix == "state",
                             row %in% c("H", "H2", "ICUd", "ICUs")),
                      names_from = row) %>% mutate(sum = H + ICUs + ICUd + H2)

lines(1:(460+60), my_tib$sum, col = "red")








# extra code

#breakpoint_times = c(20, 60, 121, 130, 141, 161, 246, 261, 291, 300, 310, 341, 371, 441, 461)

#lines(1:(460+60), filter(simulator$report(.phases = "during"), matrix == "state", row %in% c("H", "H2", "ICUd", "ICUs"))$value, col = "red")

#520
# simulator$cache$invalidate()
# lines(1:460, filter(simulator$report(.phases = "during"), matrix == "state", row == "W")$value, col = "red")
#simulator$optimization_history$get()

# the following code will be utilized when calibrating to hosp admission and/or reports
# simulator$insert$expressions(
#   trajectory = simulated_Adm ~ total_inflow[7]  ## 7 corresponds to H in this case
#   , .at = Inf  ## place the inserted expressions at the end of the expression list
#   , .phase = "during"
# )
# simulator$print$expressions()
#
# simulator$insert$expressions(
#   trajectory = simulated_reports ~ 0.1 * total_inflow[1]  ## 1 corresponds to E in this case -- this `0.1 *` bit needs to be replaced by a convolution
#   , .at = Inf  ## place the inserted expressions at the end of the expression list
#   , .phase = "during"
# )
# simulator$print$expressions()
# match("H", macpan_ww$labels$state()) - 1L  ## plug this into the square brackets for total_inflow below to get inflow into H
# match("E", macpan_ww$labels$state()) - 1L  ## plug this into the square brackets for total_inflow below to get inflow into E
# simulator$insert$expressions(
#     #X ~ X + IsH
#     #hosp ~ X - lag(X)
#     hosp ~ Is * IsH
#     total_inflow[7]  ## this is admissions
#     0.1 * total_inflow[1]  ## this is incidence
# )


#simulator$add$transformations(Log("W_sd"))
# simulator$replace$params(
#   default = c(log(mean(simulator$get$initial("beta_values"))), 0),
#   mat = c(rep("log_beta_values", 11L), "log_W_sd")#,
#   #row = 0:2
# )

# simulator$add$transformations(Log("W_sd"))
# simulator$replace$params(
#   default = 0,
#   mat = "log_W_sd"
# )


# simulator$replace$params(
#   default = c(log(0.6), 0), # qlogis(0.4)),
#   mat = c("log_beta0", "log_W_sd") #, "logit_gamma")
# )

# simulator$replace$random(
#   default = qlogis(0.2),
#   mat = "logit_gamma"
# )

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
# library(macpan2)
# x = seq(from = -100, to = 100, by = 0.1)
# y = engine_eval(~ clamp(x, 10), x = x)
# plot(x, y, xlim = c(0, max(x)), ylim = c(0, max(y)), type = "l")
# abline(a = 0, b = 1, col = 2, lty = 2)
# 
# cl = function(x, eps) {
#   # x + eps * (1.0 / (1.0 - (x - eps) / eps + ((x - eps) * (x - eps)) / (eps * eps)))
#   x + eps * (1.0 / (1.0 - (x - eps) / eps + ((x - eps) * (x - eps)) / (eps * eps)))
# }
# lines(x, cl(x, 10), col = 3)
# 
# plot(x, plogis(x))
# 
# cdf = function(x) plogis(x, mu, s)
# cdf = function(x) 1 / (1 + exp(-(x - mu)/s))
# cdf = function(x, mu, s) plogis(x, mu, s)
# cc = function(mu, s, x0) {
#   function(x) {
#     x0 * (1 - cdf(x, mu, s)) + x * cdf(x, mu, s)
#     #log(1 + exp(x))
#   }
# }
# plot(x, cc(1, 1, 1e-10)(x), type = 'l')
# plot(x, log(1 + 1e-12 + exp(x)), type = "l")
# sp = function(eps) function(x) (x + sqrt(x^2 + 4 * eps^2)) / 2
# plot(x, sp(1/10)(x), type = "l")
# sp(1e-6)(-1)
# abline(a = 0, b = 1)
# 
# spp = function(eps_neg_inf, eps_0) {
#   function(x) {
#     eps_neg_inf + 0.5 * ((x - eps_neg_inf) + sqrt((x - eps_neg_inf)^2 + (2*eps_0 - eps_neg_inf)^2 - eps_neg_inf^2))
#   }
# }
# plot(x, spp(1e-12, 1e-6)(x), type = "l")
# spp(1e-12, 1e-6)(0.0001)
# spp(1e-12, 1e-6)(0)
# spp(1e-12, 1e-6)(-1e5)
