test_that("failing expression is reported in c++ side errors", {
  expect_error(
    engine_eval(~ x[-1], x = 0),
    "output_x \\~ x\\[-1\\]"
  )
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
  expect_error(
    m$simulator()$report(),
    "y \\~ rbind_time\\(z\\)"
  )
})
