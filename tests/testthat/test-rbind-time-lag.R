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
    expr_list = mp_tmb_expr_list(
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

test_that("use of rbind_lag works for lags = 1 but not otherwise", {
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

test_that("rbind_time without times is the same as with all times", {
  with_all_times = simple_sims(
      list(x ~ x + 1, y ~ rbind_time(x, i))
    , time_steps = 5
    , mats = list(x = 0, y = empty_matrix)
    , int_vecs = list(i = 1:5)
  )
  without_times = simple_sims(
      list(x ~ x + 1, y ~ rbind_time(x))
    , time_steps = 5
    , mats = list(x = 0, y = empty_matrix)
  )
  expect_equal(with_all_times, without_times)
})

test_that("a minimum time step can be returned from the simulation history", {
  min_time = simple_sims(
      iteration_exprs = list(x ~ time_step(0),y ~ rbind_lag(x,1,5))
    , time_steps = 6
    , mats = list(x = empty_matrix,y = empty_matrix)
  ) |> dplyr::filter(matrix=="y") |> dplyr::select(value) |> dplyr::pull()
  # specific_time = simple_sims(
  #     iteration_exprs = list(x ~ time_step(0),y ~ rbind_time(x,5))
  #   , time_steps = 6
  #   , mats = list(x = empty_matrix,y = empty_matrix)
  # )
  # expect_equal(min_time, specific_time)
  expect_equal(min_time,5)
})

s = mp_tmb_model_spec(
    during = list(x ~ x / 2)
  , after = list(y ~ rbind_time(x, c(0, 2, 4, 5), 0))
  , default = list(x = 10)
) |> mp_simulator(10, c("y"))
s$report(.phases = c("before", "during", "after"))

# - mp_initial bug
# - breaking rk4
# - engine function help file
# - performance tests of c++
