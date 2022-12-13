library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

correct_answer = function(x = 0.1) {
  if (interactive()) browser()
  x = matrix(1:12, 4, 3)
  y = x[c(2, 4), c(3, 3, 2)]
  return(y)
}
correct_answer()
m = TMBModel(
  MatsList(
    x = matrix(1:12, 4, 3),
    y = 0.0,
    .mats_to_save = character(0L),
    .mats_to_return = "y"
  ),
  ExprList(before = list(
    y ~ x[c(2, 4), c(3, 3, 2)]
  )),
  OptParamsList(0),
  OptParamsList(),
  ObjectiveFunction(~0),
  Time(1)
)
f = m$make_ad_fun("dev")
print(f$report()$mats_returned)
