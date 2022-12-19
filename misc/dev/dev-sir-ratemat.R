## Not done
## TODO -- implement #9 triplet_to_matrix
##      -- finish sir definition in the standard macpan way with flowmat

library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(
    state = c(99, 1, 0),
    N = 100,
    S = 0, I = 1, R = 2,
    beta = 0, gamma = 0,
    foi = 0,
    from = c(0, 1),  # indices into state
    to = c(1, 2),  # indices into state
    rates = c(0, 0),  # numerical flow rates
    ratemat = matrix(0, 2, 2),
    .mats_to_return = "ratemat"
  ),
  ExprList(
    #before = list(x ~ x),
    during = list(
      foi ~ beta * state[I,0] / N,
      rates ~ c(foi, gamma),
      ratemat ~ triplets_to_matrix(ratemat, rates, cbind(from,to,0:1)),
    )
    #after = list()
  ),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~0),
  Time(0)
)
f = m$make_ad_fun("dev")
