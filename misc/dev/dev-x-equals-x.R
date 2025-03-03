library(macpan2)
library(TMB)
setwd("misc/dev")
compile('dev.cpp')
dyn.load(dynlib("dev"))
m = macpan2:::TMBModel(
  macpan2:::MatsList(x = 0),
  macpan2:::ExprList(before = list(x ~ x)),
  macpan2:::OptParamsList(0),
  macpan2:::OptParamsList(),
  macpan2:::ObjectiveFunction(~0),
  macpan2:::Time(0)
)
m$ad_fun("dev")
f = m$make_ad_fun("dev")
