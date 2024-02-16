test_that("group identifiers need to be valid integer indices", {
  
  set.seed(1L)
  answer = empty_matrix
  x = rnorm(3L)
  
  n_good = rep(0,2)
  n_bad = rep(0,1)
  f_good = c(0L,0L,1L)
  f_bad = c(0L,0L,2L)

  expr = list(answer ~ group_sums(x, f, n))
  
  # valid indices
  good = simple_sims(
      iteration_exprs = expr
    , time_steps = 1L
    , int_vecs = list(f = f_good)
    , mats = list(
        x = x
      , n = n_good
      , answer = answer
    )
  )
  
  # invalid indices (f is out of range)
  expect_error(
    simple_sims(
      iteration_exprs = expr
    , time_steps = 1L
    , int_vecs = list(f = f_bad)
    , mats = list(
        x = x
      , n = n_good
      , answer = answer
    )
  ), regexp = "Group indexes are out of range.")
  
  # invalid indices (n can't fit all groups)
  expect_error(
    simple_sims(
        iteration_exprs = expr
      , time_steps = 1L
      , int_vecs = list(f = f_good)
      , mats = list(
          x = x
        , n = n_bad
        , answer = answer
      )
    ), regexp = "Group indexes are out of range.")
  
  answer = subset(good, matrix == "answer")$value
  x = subset(good, matrix == "x")$value
  expect_identical(answer, c(sum(x[1:2]),x[3]))

})

