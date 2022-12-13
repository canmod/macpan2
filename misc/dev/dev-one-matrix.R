library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(x = 0, .mats_to_return = "x", .mats_to_save = "x"),
  ExprList(before = list(x ~ 1)),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~0),
  Time(2)
)
f = m$make_ad_fun("dev")
print(f$report())
