library(macpan2)
library(TMB)

clamp = function(x) {
  eps = 1e-12
  x + eps * (1.0 / (1.0-(x-eps)/eps + ((x-eps)*(x-eps))/(eps*eps)))
}

compile('dev.cpp')
dyn.load(dynlib("dev"))
set.seed(1L)
xx = rpois(10, 1)
m = TMBModel(
  MatsList(
    x = xx,
    y = rep(0, 10),
    z = rep(0, 10),
    #.mats_to_save = "y",
    .mats_to_return = "y"
  ),
  ExprList(before = list(
    z ~ x + 1,
    y ~ dpois(x, z)
  )),
  OptParamsList(xx
    , par_id = 0:9
    , mat = rep("x", 10)
    , row_id = 0:9
    , col_id = rep(0L, 10)
  ),
  OptParamsList(),
  ObjectiveFunction(~ sum(y)),
  Time(0)
)
m$data_arg()
m$param_arg()
fn = m$make_ad_fun("dev")

actual = c(fn$report()$mats_returned[[1]])
x = c(m$.init_mats$.mats()[[1L]])
correct = dpois(x, x + 1, TRUE)

#clamp(c(m$.init_mats$.mats()[[1L]]))

d = data.frame(actual, correct)
p = fn$par
f = fn$fn()
g = fn$gr()
h = fn$he()


d
p
f
g
h
