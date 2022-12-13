library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  MatsList(
    x = 10,
    .mats_to_save = "x",
    .mats_to_return = "x"
  ),
  ExprList(during = list(x ~ x + 1)),
  OptParamsList(10, par_id = 0, mat = "x", row_id = 0, col_id = 0),
  OptParamsList(),
  ObjectiveFunction(~x),
  Time(15)
)
m$data_arg()
m$param_arg()
f = m$make_ad_fun("dev")
correct_answer = 10 + (0:15)
correct_answer = append(correct_answer, correct_answer[length(correct_answer)])

data.frame(
  actual = c(f$report()$mats_returned[[1L]]),
  correct = correct_answer
)
