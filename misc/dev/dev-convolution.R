library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

correct_answer = function(x = 0.1) {
  if (interactive()) browser()
  x = c(x, numeric(14))
  for (i in 2:16) {
    x[i] = 1 + x[i - 1] / 2
  }
  y = rep(0.5, 2)
  z = numeric(16)
  for (i in 2:16) {
    z[i] = sum(y * x[i - c(1, 0)])
  }
  z[17] = z[16]
  z
}
correct_answer()
m = TMBModel(
  MatsList(
    x = 0.1,
    y = rep(0.5, 2),
    z = 0.0,
    .mats_to_save = c("x", "z"),
    .mats_to_return = c("z")
  ),
  ExprList(
    before = list(),
    during = list(
      x ~ 1 + x/2,
      z ~ convolution(x, y)
    ),
    after = list(),
    .simulate_exprs = character(0L)
  ),
  params = OptParamsList(0.1
    , par_id = 0L
    , mat = "x"
    , row_id = 0L
    , col_id = 0L
  ),
  random = OptParamsList(),
  ObjectiveFunction(~0),
  Time(15)
)
as.data.frame(m$data_arg()[c("p_table_x", "p_table_n", "p_table_i")])

f = m$make_ad_fun("dev")
actual = f$report()$table_to_return
data.frame(
   actual = actual[,5],
   correct = correct_answer()
)
