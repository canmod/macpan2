library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

set.seed(1L)
m = TMBModel(
  MatsList(
    x = rnorm(10),
    .mats_to_save = "x",
    .mats_to_return = "x"
  ),
  ExprList(
    before = list(
      x ~ x + 1
    )
  ),
  OptParamsList(0, par_id = 0, mat = "x", row_id = 0, col_id = 0),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(15)
)
m$data_arg()
m$param_arg()
f = m$make_ad_fun("dev")
f$report()
