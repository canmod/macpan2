suppressPackageStartupMessages(library(macpan2))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
options(macpan2_verbose = FALSE)
system.file("utils", "test-cache.R", package = "macpan2") |> source()
test_cache = test_cache_wipe()
all_specs = mp_tmb_entire_library()

for (obj in names(all_specs)) {
  test_cache_write(all_specs[[obj]], sprintf("SPEC-%s.rds", obj))
}

sims = list(
    sir_5_infection = mp_simulator(all_specs$sir, 5L, "infection")
  , sir_5_I = mp_simulator(all_specs$sir, 5L, "I")
  , sir_5_state = mp_simulator(all_specs$sir, 5L, c("S", "I", "R"))
  , sir_10_infection = mp_simulator(all_specs$sir, 10L, "infection")
  , sir_10_I = mp_simulator(all_specs$sir, 10L, "I")
  , sir_50_infection = mp_simulator(all_specs$sir, 50L, "infection")
  , sir_50_I = mp_simulator(all_specs$sir, 50L, "I")
)

for (obj in names(sims)) {
  test_cache_write(sims[[obj]], sprintf("SIM-%s.rds", obj))
}

trajs = sapply(sims, mp_trajectory, simplify = FALSE, USE.NAMES = TRUE)

for (obj in names(trajs)) {
  test_cache_write(trajs[[obj]], sprintf("TRAJ-%s.rds", obj))
}

cals = list(
    sir_50_beta_infection = mp_tmb_calibrator(
        all_specs$sir
      , data = mp_trajectory(sims$sir_50_infection)
      , traj = "infection"
      , par = "beta"
      , default = list(beta = 0.25)
    )
  , sir_50_log_beta_infection = mp_tmb_calibrator(
        all_specs$sir
      , data = mp_trajectory(sims$sir_50_infection)
      , traj = "infection"
      , par = "log_beta"
      , default = list(beta = 0.25)
    )
)

for (obj in names(cals)) {
  test_cache_write(cals[[obj]], sprintf("CAL-%s.rds", obj))
}
