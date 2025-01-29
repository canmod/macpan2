test_that("all types of literal NA tokens are passed to the engine as real", {
  na_mat = matrix(NA_real_)
  expect_equal(engine_eval(~ NA_real_), na_mat)
  expect_equal(engine_eval(~ NA_integer_), na_mat)
  expect_equal(engine_eval(~ NA), na_mat)
})
