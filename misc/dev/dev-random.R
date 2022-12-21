library(macpan2)
library(TMB)
library(tidyr)
library(dplyr)
#compile('dev.cpp')
#dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(
    location = empty_matrix,
    scale = empty_matrix,
    x_pois = empty_matrix,
    x_nb = empty_matrix,
    x_norm = empty_matrix,
    avg_pois = empty_matrix,
    avg_nb = empty_matrix,
    avg_norm = empty_matrix,
    .mats_to_return = c("x_pois", "x_nb", "x_norm", "avg_pois", "avg_nb", "avg_norm"),
    .mats_to_save = c("x_pois", "x_nb", "x_norm")
  ),
  expr_list = ExprList(
    before = list(
      location ~ 2,
      scale ~ 2
    ),
    during = list(
      x_pois ~ rpois(location),
      x_nb ~ rnbinom(location, scale),
      x_norm ~ rnorm(location, scale)
    ),
    after = list(
      x_pois ~ matrix(0,0,0),
      x_nb ~ matrix(0,0,0),
      x_norm ~ matrix(0,0,0),
      avg_pois ~ sum(rbind_time(x_pois, 1:1000)) / 1000,
      avg_nb ~ sum(rbind_time(x_nb, 1:1000)) / 1000,
      avg_norm ~ sum(rbind_time(x_norm, 1:1000)) / 1000
    )
  ),
  time_steps = Time(1000L)
)
s = TMBSimulator(m, "macpan2")
r = pivot_wider(s$report(0, .phases = "during"), time, names_from = matrix, values_from = value)
all(unlist(summarise(r
  , abs(mean(x_pois) - 2) < qnorm(0.999) * 2/sqrt(1000)
  , abs(mean(x_norm) - 2) < qnorm(0.999) * 2/sqrt(1000)
  , abs(mean(x_nb) - 2) < qnorm(0.999) * 2/sqrt(1000)
), use.names = FALSE))
s$report(0, .phases = "after")
