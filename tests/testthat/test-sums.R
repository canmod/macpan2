test_that("summation works correctly", {
  expect_equal(engine_eval(~sum(x, 5), x = 1:4)[,], 15)
  A = matrix(1:12, 4, 3)
  expect_equal(engine_eval(~rowSums(A), A = A), cbind(c(15, 18, 21, 24)))
  expect_equal(engine_eval(~colSums(A), A = A), rbind(c(10, 26, 42)))
  expect_equal(
    engine_eval(~ groupSums(x, f, n), x = 1:10, f = rep(0:3, 1:4), n = 4),
    matrix(c(1, 5, 15, 34))
  )
})
