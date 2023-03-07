# library(macpan2)
# compile('misc/dev/dev.cpp')
# dyn.load(dynlib("misc/dev/dev"))
# m = TMBModel(
#     init_mats = MatsList(x = 0, z = c(5, 6), y = c(1, 2, 3), .mats_to_save = "x", .mats_to_return = "x"),
#     expr_list = ExprList(during = list(x ~ c(x, 1))),
#     time_steps = Time(10),
#     params = OptParamsList(0, par_id = 0L, mat = "x", row_id = 0L, col_id = 0L)
# )
# a = TMBSimulator(m, tmb_cpp = "dev")
# a$report(0)
