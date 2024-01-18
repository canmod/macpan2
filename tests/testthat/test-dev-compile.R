#expect_that("utils to find dev.cpp work", {
  # macpan2:::dev_compile(suffix = "", ext = "cpp") ## compile dev.cpp
  # sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
  # N = 100
  # simulator = sir$simulators$tmb(time_steps = 100
  #   , state = c(S = N - 1, I = 1, R = 0)
  #   , flow = c(foi = 0, gamma = 0.1)
  #   , N = N
  #   , beta = 0.2
  #   , .tmb_cpp = "dev" ## use dev.cpp
  # )
  # sir_sims = simulator$report()
#})
