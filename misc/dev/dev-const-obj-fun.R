library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

set.seed(1L)
x = rnorm(10)
y = rpois(10, exp(2 + 3 * x))
m = TMBModel(
  MatsList(
    #y = y,
    #x = x,
    a = 0,
    b = 1,
    #e = exp(1),
    eta = 0
    #mu = 0
  ),
  ExprList(
    before = list(
      eta ~ 1
      #eta ~ a + b * x
      #mu ~ e^eta
    )
  ),
  OptParamsList(0, 1
    , par_id = c(0L, 1L)
    , mat = c("a", "b")
    , row_id = c(0L, 0L)
    , col_id = c(0L, 0L)
  ),
  OptParamsList(),
  ObjectiveFunction(~ 0),#-sum(dpois(y, mu))),
  Time(0)
)
m$data_arg()
#f = m$make_ad_fun("dev")
#print("obj")
#f$fn()
