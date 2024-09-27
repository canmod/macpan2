library(macpan2)
library(dplyr)
library(ggplot2)

## -------------------------
## get model spec from library
## -------------------------

spec = mp_tmb_library("starter_models", "sir_demog", package = "macpan2")
spec

## -------------------------
## define simulator
## -------------------------

# set number of time steps in simulation
time_steps = 100L


## -------------------------
## simulate fake data
## -------------------------

# modify the spec so that it is different from the default library
# model that we will calibrate. we will use an rk4 ode solver
spec_for_making_fake_data = mp_tmb_insert(
    spec |> mp_rk4()
  , default = list(mu = 0.1, beta = 0.4)
)

# simulator object
sim = mp_simulator(  
    model = spec_for_making_fake_data
  , time_steps = time_steps
  , outputs = "infection"
)

# simulate data (known 'true' trajectory)
true_traj = obs_traj = mp_trajectory(sim)

# add noise (simulated observed and noisy trajectory)
set.seed(1L)
obs_traj$value = rpois(time_steps, true_traj$value)

## -------------------------
## calibrate to fake data
## -------------------------

# calibrator object -- fit beta and mu to the 
# simulated infection flow (i.e., incidence).
# to be consistent we use an rk4 ode solver
cal = mp_tmb_calibrator(mp_rk4(spec), obs_traj, "infection", c("beta", "mu"))

# capture the 'default' trajectory that we would
# simulate before our model is calibrated
# (represents ignorance). this is useful to 
# show that calibration 'did something'
default_traj = mp_trajectory(cal)

# calibrate the model and check for convergence (convergence = 0 is good)
mp_optimize(cal)

# check the fitted parameter values
mp_tmb_coef(cal, conf.int = TRUE)

# they are similar to the true values
spec_for_making_fake_data$default[c("beta", "mu")]

# calibrated trajectory with confidence intervals
cal_traj = mp_trajectory_sd(cal, conf.int = TRUE) 

data = (nlist(true_traj, obs_traj, default_traj, cal_traj)
  |> bind_rows(.id = "data_type")
)


## -------------------------
## explore the calibration
## -------------------------

# the calibrated trajectory and confidence interval are consistent
# with the true trajectory and go through the observed trajectory.
# the default trajectory is much different, indicating that 
# calibration really did do what it should do.
(data
  |> ggplot()
  + geom_line(aes(time, value, colour = data_type))
  + geom_ribbon(aes(x = time, ymin = conf.low, ymax = conf.high)
      , alpha = 0.5
      , colour = "lightgrey"
      , fill = "lightgrey"
      , data = cal_traj
    ) 
  + theme_bw()
)
