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
mp_tmb_coef(cal)
mp_tmb_coef(sim)
mp_trajectory_replicate(sim, 10, list(gamma = 0.2))
mp_trajectory_par(cal, list(gamma = 0.2))
mp_functions_used(cal)
mp_functions_used(sim)
mp_generates_randomness(spec |> mp_euler_multinomial())
mp_generates_randomness(sim)

el = spec$expr_list()$formula_list()
el |> lapply(macpan2:::formula_components) |> lapply(getElement, "functions") |> unlist(recursive = TRUE) |> unique()
