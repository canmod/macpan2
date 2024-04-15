test_that("change pointers can be incremented", {
  sims = simple_sims(
    iteration_exprs = list(pointer ~ time_group(pointer, change_points)),
    time_steps = 10,
    mats = list(
      pointer = 0,
      change_points = c(0, 4, 7)
    )
  )
  expect_equal(
    sims[sims$matrix == "pointer", "value"],
    c(0, 0, 0, 1, 1, 1, 2, 2, 2, 2)
  )
})

test_that("time_var grabs entire matrix rows each time step", {
  A = matrix(1:12, 3, 4)
  result = simple_sims(
      iteration_exprs = list(
        x ~ time_var(A, cp)
      )
    , time_steps = 6L
    , int_vecs = list(cp = c(0, 2, 4))
    , mats = list(A = A, x = empty_matrix)
  ) |> filter(matrix == "x") |> pull(value)
  answer = A[rep(1:3, 1:3), ] |> t() |> as.vector()
  expect_equal(result, answer)
})
