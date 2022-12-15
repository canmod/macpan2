library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(x = matrix(1:(7*7), 7, 7), y = 7, z = 7,
    .mats_to_return = "x", .mats_to_save = "x"
  ),
  ExprList(during = list(
    y ~ y - 1,
    z ~ z - 2,
    x ~ matrix(1:(y * z), y, z)
  )),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~0),
  Time(2)
)

f = m$make_ad_fun("dev")

print(f$report())
