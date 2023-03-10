library(macpan2)

m = TMBModel(
    init_mats = MatsList(z = empty_matrix, x = rnorm(10), y = rnorm(10), .mats_to_save = "z", .mats_to_return = "z"),
    expr_list = ExprList(during = list(z ~ t(x) %*% y)),
    time_steps = Time(10)
)
m$make_ad_fun()$report()
