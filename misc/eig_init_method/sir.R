library(macpan2)
library(ggplot2)
library(dplyr)
library(broom.mixed)
options(macpan2_verbose =  FALSE)
sir_focal = ("starter_models"
  |> mp_tmb_library("sir", package = "macpan2")
)
sir_eigen = (sir_focal
  |> mp_tmb_delete(phase = "during", at = 3) ## do  not drain S
  |> mp_tmb_update(default = list(N = 1, I = 1/100))
  |> mp_tmb_insert(at = Inf
    , expressions = list(
        notS_i ~ I + R
      , I ~ notS * (I / notS_i)
      , R ~ notS * (R / notS_i)
    )
    , default = list(notS = 1)
  )
  |> mp_tmb_update(must_not_save = c("S", "I", "R"))
)

sir_focal_simulator = (sir_focal
  |> mp_simulator(100, "I")
)
sir_focal_simulator$replace$params(
    c(beta = 0.25, I = 1, R = 0)
  , c("beta", "I", "R")
)
sir_eigen_simulator = mp_simulator(sir_eigen
  , time_steps = 100
  , outputs = c("I", "R")
)
sir_eigen_simulator$replace$params(
    c(beta = 0.7, notS = 5)
  , c("beta", "notS")
)


sir_focal_ad = mp_tmb(sir_focal_simulator)
sir_eigen_ad = mp_tmb(sir_eigen_simulator)
theta_eigen = function(param_list) c(param_list$both, param_list$eigen)
theta_focal = function(param_list) c(param_list$both, param_list$focal)
get_eigen_vec = function(param_list) {
  sir_eigen_ad$report(theta_eigen(param_list))$values[, 5]
}
expand_param = function(param_list) c(theta_focal(param_list), get_eigen_vec(param_list))
sim_traj = function(param_list) {
  sir_focal_ad$report(expand_param(param_list))$values[2:101, 5]
}
sim_theta = (
  list(both = c(beta = 0.25), eigen = c(notS = 5), focal = numeric())
  |> as.relistable()
)
sir_results = data.frame(time = 1:100, mat = "I", value = rpois(100, sim_traj(sim_theta)))

guess_theta = list(both = c(beta = 0.5), eigen = c(notS = 1), focal = numeric())
sir_focal_calibrator = mp_tmb_calibrator(sir_focal
  , data = sir_results
  , traj = "I"
  , par = c("beta", "I", "R")
)
sir_focal_cal_ad = mp_tmb(sir_focal_calibrator)


obj_fn = function(theta) {
  p = expand_param(relist(theta, sim_theta))
  sir_focal_cal_ad$fn(p)
}
obj_fn(unlist(guess_theta))
obj_fn(unlist(sim_theta))
opt = optim(unlist(guess_theta), obj_fn)

plot(sir_results$value)
lines(sim_traj(relist(opt$par, sim_theta)))
