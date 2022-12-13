library(macpan2)
library(TMB)

clamp = function(x) {
  eps = 1e-12
  x + eps * (1.0 / (1.0-(x-eps)/eps + ((x-eps)*(x-eps))/(eps*eps)))
}

compile('dev.cpp')
dyn.load(dynlib("dev"))
set.seed(1L)
m = TMBModel(
  MatsList(
    x = rpois(10, 1) + 1e-12,
    y = 1,
    .mats_to_save = "y",
    .mats_to_return = "y"
  ),
  ExprList(before = list(y ~ dpois(x, x))),
  OptParamsList(1, par_id = 0, mat = "y", row_id = 0, col_id = 0),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(0)
)
m$data_arg()
m$param_arg()
f = m$make_ad_fun("dev")

actual = f$report()$mats_returned[[1L]][,2]
correct = dpois(c(m$.init_mats$.mats()[[1L]]), clamp(c(m$.init_mats$.mats()[[1L]])), TRUE)

data.frame(actual, correct)
