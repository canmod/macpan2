library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
sim = mp_simulator(spec, 50, "infection")
data = mp_trajectory(sim)
cal = mp_tmb_calibrator(
    spec
  , data
  , traj = "infection"
  , par = "beta"
  , default = list(beta = 0.25)
)
mp_default(cal)
mp_initial(cal)

mp_optimize(cal)
mp_default(cal)
mp_initial(cal)

mp_initial(cal)

cal$simulator$current$update_matrix_list(cal$cal_spec$default)
cal$new_spec$default$beta
debug(mp_default)
mp_default(cal)
