test_that("arguments and argument dots produce the same results", {
  dot_args = system.file("testing_models", "dot_args", package = "macpan2")
  nodot_args = system.file("testing_models", "nodot_args", package = "macpan2")
  dot_args = Compartmental(dot_args)
  nodot_args = Compartmental(nodot_args)
  dot_sims = dot_args$simulators$tmb(time_steps = 3
    , state = c(S = 99, I = 1, R = 0)
    , flow = c(alpha = 0.1, beta = 0.1, gamma = 0.1)
    , pop = empty_matrix
  )$report()
  nodot_sims = nodot_args$simulators$tmb(time_steps = 3
    , state = c(S = 99, I = 1, R = 0)
    , flow = c(alpha = 0.1, beta = 0.1, gamma = 0.1)
    , pop = empty_matrix
  )$report()
  expect_equal(dot_sims, nodot_sims)
})
