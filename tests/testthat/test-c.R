test_that("concatenation works with many different shapes of input", {
  set.seed(1L)
  z = rnorm(4L)
  w = matrix(rnorm(12L), 3L, 4L)
  answer = c(pi, z, w)
  m = macpan2:::TMBModel(
      init_mats = macpan2:::MatsList(
          answer = empty_matrix
        , x = empty_matrix
        , y = pi
        , z = z
        , w = w
        , .mats_to_return = "answer"
      ),
      expr_list = mp_tmb_expr_list(before = list(answer ~ c(x, y, z, w)))
  )
  s = macpan2:::TMBSimulator(m)
  expect_equal(
    s$matrix(matrix_name = "answer", time_step = 1L, .phases = "after"),
    matrix(answer)
  )
})

test_that("String objects can be concatenated", {
  x = macpan2:::StringUndottedVector("S", "E", "I", "R")
  y = macpan2:::StringUndottedVector("D")
  z = macpan2:::StringUndottedVector("S", "E", "I", "R", "D")
  expect_identical(c(x, y), z) # failing now
})
