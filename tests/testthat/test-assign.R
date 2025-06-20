test_that("assign function assignment indices are checked", {
  x = rep(1, 5)
  y = replace(x, c(2, 3), c(100, 200))
  expect_equal(
    engine_eval(~assign(x, c(1, 2), 0, c(100, 200))
      , x = x
      , .matrix_to_return = "x"
    ),
    matrix(y)
  )
  expect_error(
    engine_eval(~assign(x, c(1, 5), 0, c(100, 200))
      , x = x
      , .matrix_to_return = "x"
    ),
    "The following error was thrown by the TMB engine"
  )
})

test_that("combine assignment works", {
  r = simple_sims(
      list(c(x, y) ~ a)
    , time_steps = 1L
    , mats = list(a = matrix(1:6, 3, 2), x = empty_matrix, y = empty_matrix)
  )
  expect_equal(
      r[r$matrix == "a", "value"]
    , c(r[r$matrix == "x", "value"], r[r$matrix == "y", "value"])
  )
})

test_that("square bracket assignment works", {
  i = c(1, 2)
  j = c(2, 0, 3)
  A = matrix(1:12, 3, 4)
  B = matrix(100:95, 2, 3)
  r = simple_sims(
      list(A[i, j] ~ B)
    , time_steps = 1L
    , mats = nlist(A, B)
    , int_vecs = nlist(i, j)
  )
  A_expected = A
  A_expected[i + 1, j + 1] = B
  A_expected = macpan2:::melt_matrix(A_expected)
  A_actual = r[r$matrix == "A", c("row", "col", "value")]
  A_expected$row = as.numeric(A_expected$row)
  A_expected$col = as.numeric(A_expected$col)
  expect_equal(A_actual, A_expected)
})


## this case is wrong -- it should recycle to the other column of A, 
## but we do not want to always recycle. we probably do not need to
## recycle because we probably can just loop over the columns.
r = simple_sims(
    list(A[i, j] ~ x)
  , time_steps = 1L
  , mats = list(A = matrix(1:6, 3, 2), x = matrix(99:102, 2, 2))
  , int_vecs = list(i = c(1, 2), j = c(1, 0))
)
r
