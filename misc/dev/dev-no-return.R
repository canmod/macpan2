library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(x = 0, a = 0, b = 1),
  ExprList(before = list(x ~ 1 + a * b)),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~0),
  Time(0)
)
m$data_arg()
m$param_arg()
m$random_arg()
f = m$make_ad_fun("dev")
f$report()
