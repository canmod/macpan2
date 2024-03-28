library(macpan2)
library(dplyr)

source("misc/experiments/sir_with_vectors/tmb.R")

state_labels  = c("S","I","R")

# basic simulator
sim = (
  spec
  |> mp_simulator(time_steps = 50, outputs = "I")
)
# simulated I
sim_data = (
  sim
  |> mp_trajectory()
  |> mutate(value = rpois(n(),value))
)

## testing out different theta formats
## see misc/experiments/sir_with_vectors/tmb.R for scenarios

## before running this script I needed to comment out these lines
## https://github.com/canmod/macpan2/blob/70f6ca0b7b699d6cdc06d789145d3d53a27ff38a/R/mp_tmb_calibrator.R#L774-L786
## and rebuild

## -------------------------
## make calibrators
## -------------------------

# high-level interface calibrator
## -------------------------
cal_high = mp_tmb_calibrator(
    spec = spec 
  , data = sim_data
  , traj = "I"
  , par = "theta"
  , outputs = state_labels 
  , default = list(new=0)
)
## -------------------------

# low-level interface calibrator
## -------------------------
cal_low = sim
cal_low$update$matrices(

  ## observed data
  I_obs = sim_data$value,

  ## simulated trajectory to compare with data
  I_sim = empty_matrix,

  ## matrix to contain the log likelihood values at
  ## each time step
  log_lik = empty_matrix,

  ## need to save the simulation history of each of these matrices
  .mats_to_save = c("I_sim", "log_lik")
)
cal_low$insert$expressions(
  I_sim ~ I,
  .phase = "during"
)
cal_low$insert$expressions(
  log_lik ~ dpois(I_obs, clamp(rbind_time(I_sim))),
  .phase = "after"
)
cal_low$replace$obj_fn(~ -sum(log_lik))
params_to_fit = data.frame(
    mat = "theta"
    #, row = c(0,1)
   , row = c("beta","R_initial")
  , default = mp_default_list(spec)$theta
)
cal_low$replace$params_frame(params_to_fit)
## -------------------------

## -------------------------
## optimize
## -------------------------


opt_low = mp_optimize(cal_low)
opt_high = mp_optimize(cal_high)

# compare convergence & iterations
opt_low$converge == opt_high$converge 
opt_low$iterations == opt_high$iterations

# compare estimates
mp_tmb_coef(cal_low, conf.int=TRUE)
mp_tmb_coef(cal_high, conf.int=TRUE)

