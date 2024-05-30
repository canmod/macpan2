test_that("old segfaults do not show up again", {
  set.seed(1L)
  x = matrix(1:3)
  expect_error(
      engine_eval(~ x[5], x = x)
    , "Illegal index to square bracket"
  )
  expect_error(
      engine_eval(~ assign(x, 1, c(0, 0), 100:102), x = x, .matrix_to_return = "x")
    , "cannot recycle rows and/or columns because the input is inconsistent with the recycling request"
  )
  expect_error(
      engine_eval(~ rnorm(x, x[0:1]), x = x)
    , "cannot recycle rows and/or columns because the input is inconsistent with the recycling request"
  )
  expect_error(
      simple_sims(list(dummy ~ i+1), 1L, int_vecs = list(i = 2))
    , "All arguments to math functions must be numeric matrices, but at least one is an integer"
  )
  expect_error(
      simple_sims(list(dummy ~ log(i)), 1L, int_vecs = list(i = 2))
    , "All arguments to math functions must be numeric matrices, but at least one is an integer"
  )
  expect_error(
      simple_sims(list(dummy ~ sum(i)), 1L, int_vecs = list(i = 2))
    , "All arguments to math functions must be numeric matrices, but at least one is an integer"
  )
})
