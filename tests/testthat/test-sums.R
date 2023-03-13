test_that("summation works correctly", {
  expect_equal(engine_eval(~sum(x, 5), x = 1:4)[,], 15)
  A = matrix(1:12, 4, 3)
  expect_equal(engine_eval(~rowSums(A), A = A), cbind(c(15, 18, 21, 24)))
  expect_equal(engine_eval(~colSums(A), A = A), rbind(c(10, 26, 42)))
})
