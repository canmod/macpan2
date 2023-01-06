library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(x = runif(3), y = 3, z = empty_matrix
    , .mats_to_return = c("x", "y", "z")
  ),
  expr_list = ExprList(before = list(z ~ rep(x, y)))
)
f = m$make_ad_fun("dev")
f$report()$value
