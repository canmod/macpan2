test_that("the order of state variables and flow variables is defined by settings not code", {
  library(macpan2)
  sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
  N = 100
  state = c(S = N - 1, I = 1, R = 0)
  flow = c(foi = 0, gamma = 0.1)
  simulator_mixup = sir$simulators$tmb(time_steps = 100
    , state = state[c(2, 3, 1)]
    , flow = rev(flow)
    , N = N
    , beta = 0.2
  )
  simulator = sir$simulators$tmb(time_steps = 100
    , state = state
    , flow = flow
    , N = N
    , beta = 0.2
  )
  expect_equal(simulator$tmb_model, simulator_mixup$tmb_model)
  sims_mixup = simulator_mixup$report()
  sims = simulator$report()
  expect_equal(sims_mixup, sims)
})

test_that("the order of state variables and flow variables in settings does not impact simulations", {
  library(macpan2)
  library(dplyr)
  sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
  sir_mixup = Compartmental(system.file("testing_models", "sir", package = "macpan2"))
  N = 100
  state = c(S = N - 1, I = 1, R = 0)
  flow = c(foi = 0, gamma = 0.1)
  simulator = sir$simulators$tmb(time_steps = 100
    , state = state
    , flow = flow
    , N = N
    , beta = 0.2
  )
  simulator_mixup = sir_mixup$simulators$tmb(time_steps = 100
    , state = state
    , flow = flow
    , N = N
    , beta = 0.2
  )
  sims = simulator$report()
  sims_mixup = simulator_mixup$report()
  (sims
    |> left_join(sims_mixup, by = c("matrix", "time", "row", "col"))
    |> summarise(agree = all(value.x == value.y))
    |> pull(agree)
    |> expect_true()
  )
})
