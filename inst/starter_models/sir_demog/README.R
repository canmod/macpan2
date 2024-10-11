## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.path = "./figures/"
)
system.file("utils", "round-coef.R", package = "macpan2") |> source()


## ----packages, message=FALSE, warning=FALSE-----------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(macpan2)


## ----options------------------------------------------------------------------
options(macpan2_verbose = FALSE)


## ----model_spec---------------------------------------------------------------
spec = mp_tmb_library(
    "starter_models"
  , "sir_demog"
  , package = "macpan2"
)


## ----diagram, echo = FALSE, fig.height = 2, fig.width = 8---------------------
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_paths(spec)
(layout
  |> plot_flow_diagram(show_flow_names = TRUE)
  |> draw_inflows(layout, show_labels = TRUE)
  |> draw_outflows(layout, show_labels = TRUE, pattern_mutate = "^([a-z]*)(_.*)", pattern_replace = "\\1")
)


## ----defaults-----------------------------------------------------------------
true = list(mu = 0.1, beta = 0.4)
spec_for_making_fake_data = mp_tmb_insert(
    spec |> mp_rk4()
  , default = true
)


## ----simulate-----------------------------------------------------------------
time_steps = 100L
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


## ----calibrator---------------------------------------------------------------
cal = mp_tmb_calibrator(mp_rk4(spec), obs_traj, "infection", c("beta", "mu"))


## ----default_traj-------------------------------------------------------------
default_traj = mp_trajectory(cal)


## ----optimize-----------------------------------------------------------------
mp_optimize(cal)


## ----fits---------------------------------------------------------------------
coef = mp_tmb_coef(cal) |> round_coef_tab()
coef$true = true[coef$mat]
print(coef)


## ----ci-----------------------------------------------------------------------
cal_traj = mp_trajectory_sd(cal, conf.int = TRUE) 
data = (nlist(true_traj, obs_traj, default_traj, cal_traj)
  |> bind_rows(.id = "data_type")
)


## ----plot_fit-----------------------------------------------------------------
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

