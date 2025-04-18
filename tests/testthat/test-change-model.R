library(macpan2); library(testthat); library(dplyr); library(tidyr)
spec = mp_tmb_model_spec(
  during = list(
      k1_S ~ 1 ## rk4 coefficient name to test for name conflict avoidance
    , mp_per_capita_flow("S", "I", "beta * I", "incidence")
  )
)
spec_expanded = spec |> mp_rk4() |> mp_expand()

spec = mp_tmb_model_spec(
  during = list(a ~ aakjhsadfkjlhasdflkjhasdflkjhasdfkjlhadsfkjhasdjhfgasdhgfasdhgfhasdgfjhagsdf + jsdhfajhksdgfjakhsdgfkjahsdgfkjhasdfjhkagsdfkhjas + asdjhfbaksdjhfaksjdhfaskdjhf, b ~ d),
  default = list(aa = 1, d = 1)
)

spec = mp_tmb_model_spec(
    before = list(S ~ N - I - R)
  , during = list(
        N ~ S + I + R
      , mp_per_capita_flow("S", "I", infection ~ beta * I / N)
      , mp_per_capita_flow("I", "R", recovery ~ gamma)
    )
  , default = list(N = 100, I = 1, R = 0, beta = 0.25, gamma = 0.1)
)

set.seed(10)
data = (spec 
  |> mp_euler_multinomial()
  |> mp_simulator(50, "infection") 
  |> mp_trajectory()
)
cal = (spec
  |> mp_rk4()
  |> mp_tmb_calibrator(data, "infection", "beta")
)
cal = (mp_rk4(spec)$expand()
  |> mp_tmb_calibrator(data, "infection", "beta")
)

opt_results = mp_optimize(cal)

simulator = (spec
  |> mp_hazard()
  |> mp_simulator(10, "infection")
)

args = simulator$tmb_model$make_ad_fun_arg()
ad_fun = do.call(
    TMB::MakeADFun
  , c(args, list(inner.control = list(maxit = 10000)))
)
opt = nlminb(ad_fun$par
  , ad_fun$fn
  , ad_fun$gr
  , ad_fun$he
  , control = list(eval.max = 1000000, iter.max = 1000000)
)




macpan_base = mp_tmb_library(
    "starter_models"
  , "macpan_base"
  , package = "macpan2"
)
em = (macpan_base
  |> mp_euler_multinomial()
  |> mp_expand()
  |> mp_simulator(20L, "S.E")
  |> mp_trajectory()
)
el = mp_simulator(macpan_base, 20L, "S.E") |> mp_trajectory()


