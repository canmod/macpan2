test_that("concatenation works with many different shapes of input", {
  set.seed(1L)
  z = rnorm(4L)
  w = matrix(rnorm(12L), 3L, 4L)
  answer = c(pi, z, w)
  m = TMBModel(
      init_mats = MatsList(
          answer = empty_matrix
        , x = empty_matrix
        , y = pi
        , z = z
        , w = w
        , .mats_to_return = "answer"
      ),
      expr_list = ExprList(before = list(answer ~ c(x, y, z, w)))
  )
  s = TMBSimulator(m)
  expect_equal(
    s$matrix(matrix_name = "answer", time_step = 1L, .phases = "after"),
    matrix(answer)
  )
})
