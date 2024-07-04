library(macpan2)

spec = mp_tmb_model_spec(
    before = S ~ N - 1
  , during = mp_per_capita_flow("S", "I", "beta * I / N", "infection")
  , default = list(N = 100, beta = 0.2, I = 1)
)
specs = list(
    euler = spec
  , euler_multinomial = mp_euler_multinomial(spec)
  , hazard = mp_hazard(spec)
  , rk4 = mp_rk4(spec)
)
