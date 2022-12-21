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
    yy = empty_matrix,
    z = rep(0, 10),
    #.mats_to_save = "y",
    .mats_to_return = c("y", "yy")
  ),
  ExprList(before = list(
    z ~ exp(rnorm(rep(0, 10), rep(1, 10))),
    y ~ -dpois(x, 10),
    yy ~ -dpois(x, clamp(z))
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
f = m$make_ad_fun("dev")
print(matrix(f$report()$values[,5], 10))
