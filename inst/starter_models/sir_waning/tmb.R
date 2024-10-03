library(macpan2)

computations = list(
  S ~ N - I - R
)

flows = list(
    mp_per_capita_flow("S", "I", "I * beta / N", "infection")
  , mp_per_capita_flow("I", "R", "gamma", "recovery")
  , mp_per_capita_flow("R", "S", "phi", "waning_immunity")
)

# model spec
spec = mp_tmb_model_spec(
    before = computations
  , during = flows
  # defaults
  , default = list(
      N = 100, I = 1, R = 0
    , beta = 0.2, gamma = 0.2, phi = 0.01
  )
)
