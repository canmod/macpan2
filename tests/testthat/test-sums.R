test_that("summation works correctly", {
  expect_equal(engine_eval(~sum(x, 5), x = 1:4)[,], 15)
  A = matrix(1:12, 4, 3)
  expect_equal(engine_eval(~row_sums(A), A = A), cbind(c(15, 18, 21, 24)))
  expect_equal(engine_eval(~col_sums(A), A = A), rbind(c(10, 26, 42)))
  expect_equal(
    engine_eval(~ group_sums(x, f, rep(0, n)), x = 1:10, f = rep(0:3, 1:4), n = 4),
    matrix(c(1, 5, 15, 34))
  )
})

test_that("cumulative summation works correctly", {
  expect_equal(
      engine_eval(~cumsum(1:5)) |> c()
    , cumsum(1:5)
  )
  expect_equal(
      engine_eval(~cumsum(matrix(1:20, 5, 4)))
    , apply(matrix(1:20, 5, 4), 2, cumsum)
  )
})
