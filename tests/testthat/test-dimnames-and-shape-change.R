library(macpan2)
m = TMBModel(
    init_mats = MatsList(x = 0, z = matrix(1:6, 3, 2), y = 7:10, .mats_to_save = "x", .mats_to_return = "x"),
    expr_list = ExprList(during = list(x ~ c(z, y))),
    time_steps = Time(10),
    params = OptParamsList(0, par_id = 0L, mat = "x", row_id = 0L, col_id = 0L)
)
a = TMBSimulator(m)
a$report(0)
