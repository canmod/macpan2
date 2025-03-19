test_that("engine function last() works", {
  set.seed(1)
  b = rnorm(3)
  s = simple_sims(list(a1 ~ last(b), a2 ~ b[2])
    , time_steps = 1
    , mats = list(a1 = empty_matrix, a2 = empty_matrix, b = b)
  ) |> filter(startsWith(matrix, "a"))
  expect_equal(s$value[1], s$value[2])
})
