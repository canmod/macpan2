library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(x = matrix(runif(12), 4, 3), y = empty_matrix
    , .mats_to_return = c("x", "y")
  ),
  expr_list = ExprList(before = list(y ~ block(x, 1, 1, 3, 2)))
)
s = TMBSimulator(m, "dev")
x = s$matrix(0, matrix_name = "x", time_step = 1L)
y = s$matrix(0, matrix_name = "y", time_step = 1L)
print(x)
print(y)
