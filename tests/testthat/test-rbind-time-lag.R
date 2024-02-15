test_that("a selection of the iterations in the simulation history of a matrix that doesn't change shape can be rbinded at the end", {
  steps = 10
  x = matrix(1:12, 4, 3)
  y = empty_matrix
  s = macpan2:::TMBModel(
    init_mats = macpan2:::MatsList(
      x = x,
      y = y,
      t = seq(from = 1, to = 9, by = 2),
      steps = steps,
      .mats_to_save = "x",
      .mats_to_return = "y"
    ),
    expr_list =mp_tmb_expr_list(
      during = list(x ~ x * 0.9),
      after = list(y ~ rbind_time(x, t, 1))
    ),
    time_steps = macpan2:::Time(steps)
  )$simulator()
  y_tmb = s$matrix(time_step = 11, matrix_name = "y", .phases = c("before", "during", "after"))

  y_r = matrix(numeric(), nrow = 0, ncol = 3)
  for (i in 1:steps) {
    x = x * 0.9
    if (i %% 2 == 1L) {
      y_r = rbind(y_r, x)
    }
  }

  expect_equal(y_tmb, y_r)
})

test_that("use of rbind_lag works for lags = 1 but not overwise", {
  y = simple_sims(
      list(x ~ x + 1, y ~ rbind_lag(x, 1))
    , 10
    , mats = list(x = 0, y = empty_matrix)
  ) |> macpan2:::filter(matrix == "y")
  expect_equal(y$value, 0:9)
  expect_error(
      simple_sims(
          list(x ~ x + 1, y ~ rbind_lag(x, 2))
        , 10
        , mats = list(x = 0, y = empty_matrix)
      ),
      regexp = "Lag functionality is conceptually flawed at the moment for lags greater than 1"
  )
})
