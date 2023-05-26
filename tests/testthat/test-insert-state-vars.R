test_that("inserted expressions are able to refer to single states/flows", {
  m = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
  s = m$simulators$tmb(time_steps = 3
    , state = c(S = 99, I = 1, R = 0)
    , flow = c(foi = 0, gamma = 0.2)
    , beta = 0.4
    , N = empty_matrix
    , test_ratio = empty_matrix
    , .mats_to_return = c("test_ratio", "state")
  )
  s$insert$expressions(test_ratio ~ I / S, .at = Inf, .phase = "during")
  s$insert$expressions(foi ~ (I/S)^0.5 * beta, .at = 2L, .phase = "during")
  v = s$report(.phases = "during")
  expect_equal(
    v[v$row == "I", "value"] / v[v$row == "S", "value"],
    v[v$matrix == "test_ratio", "value"]
  )
})
