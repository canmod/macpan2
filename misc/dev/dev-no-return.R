library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(x = 0),
  ExprList(before = list(x ~ x)),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~0),
  Time(0)
)
f = m$make_ad_fun("dev")
print(f$report())
