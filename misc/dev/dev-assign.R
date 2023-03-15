library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

# m = TMBModel(
#     init_mats = MatsList(
#         m = matrix(1:12, 4, 3)
#       , dummy = empty_matrix
#       , .mats_to_return = c("m", "a", "b", "c")
#     )
#   , expr_list = ExprList(before = list(dummy ~ assign(m, c(2, 1, 0), c(2, 2, 0), c(3.14, 100000, 1/40))))
#   , params = OptParamsList(0)
#   , random = OptParamsList()
#   , obj_fn = ObjectiveFunction(~0)
#   , time_steps = Time(0L)
# )
# m$make_ad_fun("dev")$report()

m = TMBModel(
    init_mats = MatsList(
        m = matrix(1:12, 4, 3)
      , dummy = empty_matrix
      , .mats_to_return = "m"
    )
  , expr_list = ExprList(before = list(dummy ~ assign(m, c(2, 1, 0), 1, c(3.14, 100000, 1/40))))
  , time_steps = Time(0L)
)
s = TMBSimulator(m, "dev")
s$matrix(matrix_name = "m", time_step = 1L)

