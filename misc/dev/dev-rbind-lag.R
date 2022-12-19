library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))


m = TMBModel(
  MatsList(
    x = 1,
    y = 0,
    z = 1,
    .mats_to_save = c("x", "y", "z"),
    .mats_to_return = c("x", "y")
  ),
  ExprList(
    during = list(
      x ~ 1.3 * x - 0.2,
      y ~ rbind_lag(z, 0:5)  # sum(rbind_lag(x, 0:5)) /
    )
    # after = list(
    #   y ~ rbind_time(x, 1:15)
    # )
  ),
  OptParamsList(0.9, par_id = 0L, mat = "x", row_id = 0L, col_id = 0L),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(15)
)
m$data_arg()
m$param_arg()
f = m$make_ad_fun("dev")
f$report()
