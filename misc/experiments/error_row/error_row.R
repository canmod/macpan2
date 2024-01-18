library(macpan2)
engine_eval(~ x[-1], x = 0)

m = TMBModel(
  expr_list = ExprList(
    during = list(
      z ~ 1 + x[1] + 1,
      y ~ rbind_time(z)
    )
  ),
  init_mats = MatsList(
    x = 1:10,
    y = empty_matrix,
    z = empty_matrix,
    .mats_to_save = "y",
    .mats_to_return = "y"
  ),
  time_steps = Time(10)
)

undebug(TMBSimulator)
m$simulator()$report()
