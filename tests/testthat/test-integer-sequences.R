test_that("integer sequence contruction is correct", {
  expect_equal(matrix(3:17), engine_eval(~3:17))
  expect_equal(
    matrix(seq(from = 5, length = 10, by = 3)),
    engine_eval(~seq(from = 5, length = 10, by = 3))
  )
  expect_equal(matrix(rep(1, 5)), engine_eval(~rep(1, 5)))
})
