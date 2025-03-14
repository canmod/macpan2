library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
options(macpan2_verbose = FALSE)

spec = mp_tmb_library("starter_models", "sir", package = "macpan2")
sim = mp_simulator(spec, 50, "infection")
data = mp_trajectory(sim)
cal = mp_tmb_calibrator(
    spec
  , data
  , traj = "infection"
  , par = mp_par(
        param = list(beta = mp_normal(0.25, 1))
      , random = list(gamma = mp_normal(0.25, 1))
    )
  , default = list(beta = 0.25, gamma = 0.17)
)
mp_optimize(cal)
mp_tmb_coef(cal, effects = c("fixed", "random"))
mp_trajectory_par(cal, list(beta = 0.2), list(gamma = 0.1))
cal$simulator$current$params_frame()
cal$simulator$current$random_frame()
ff = cal$simulator$ad_fun()
ff$report
ff$env$random
cal$simulator$report(0.2)
cal$simulator$.runner(c(0.4, 0.2), .method = "sdreport")$sd
macpan2:::mp_trajectory_par.TMBSimulator(cal$simulator, list(beta = 0.5), list())
