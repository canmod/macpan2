test_that("a selection of the iterations in the simulation history of a matrix that doesn't change shape can be rbinded at the end", {
  steps = 10
  x = matrix(1:12, 4, 3)
  y = empty_matrix
  s = TMBModel(
    init_mats = MatsList(
      x = x,
      y = y,
      t = seq(from = 1, to = 9, by = 2),
      steps = steps,
      .mats_to_save = "x",
      .mats_to_return = "y"
    ),
    expr_list = ExprList(
      during = list(x ~ x * 0.9),
      after = list(y ~ rbind_time(x, t, 1))
    ),
    time_steps = Time(steps)
  )$simulator()
  y_tmb = s$matrix(time_step = 11, matrix_name = "y")

  y_r = matrix(numeric(), nrow = 0, ncol = 3)
  for (i in 1:steps) {
    x = x * 0.9
    if (i %% 2 == 1L) {
      y_r = rbind(y_r, x)
    }
  }

  expect_equal(y_tmb, y_r)
})