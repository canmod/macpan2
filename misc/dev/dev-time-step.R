library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(tau = empty_matrix, .mats_to_return = "tau", .mats_to_save = "tau"),
  expr_list = ExprList(before = )
)
