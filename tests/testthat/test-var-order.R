test_that("arguments and argument dots produce the same results", {
  skip("test uses Compartmental")
  dot_args = system.file("testing_models", "dot_args", package = "macpan2")
  rand_order_args = system.file("testing_models", "vars_rand_order", package = "macpan2")
  dot_args = Compartmental(dot_args)
  rand_order = Compartmental(rand_order_args)
  dot_sims = dot_args$simulators$tmb(time_steps = 3
    , state = c(S = 99, I = 1, R = 0)
    , flow = c(alpha = 0.1, beta = 0.2, gamma = 0.3)
    , pop = empty_matrix
  )$report()
  rand_order_sims = rand_order$simulators$tmb(time_steps = 3
    , state = c(R = 0, S = 99, I = 1)
    , flow = c(beta = 0.2, alpha = 0.1, gamma = 0.3)
    , pop = empty_matrix
  )$report()
  matched_values = merge(dot_sims, rand_order_sims, by = c("time", "row"))
  expect_equal(matched_values$value.x, matched_values$value.y)
})
