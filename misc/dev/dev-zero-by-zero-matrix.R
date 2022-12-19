library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(
    x = matrix(numeric(0L), 0L, 0L),
    y = 1,
    .mats_to_save = c("x", "y"),
    .mats_to_return = c("x", "y")
  ),
  ExprList(
    during = list(
      y ~ y + 1
    )
  ),
  OptParamsList(1, par_id = 0L, mat = "y", row_id = 0L, col_id = 0L),
  OptParamsList(),
  ObjectiveFunction(~y),
  Time(15)
)
m$data_arg()
f = m$make_ad_fun("dev")
f$fn()
f$report()
