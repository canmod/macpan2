library(macpan2)

initialize_state = list(
  S ~ N - I - R
)

computations = list(
  N ~ S + I + R
)

during = list(
    mp_per_capita_inflow("N", "S", "nu", "birth")
  , mp_per_capita_outflow("S", "mu", "death_S")
  , mp_per_capita_outflow("I", "mu", "death_I")
  , mp_per_capita_outflow("R", "mu", "death_R")
  , mp_per_capita_flow("S", "I", "beta * I / N", "infection")
  , mp_per_capita_flow("I", "R", "gamma", "recovery")
)

## set defaults
default = list(
      beta = 0.2
    , gamma = 0.1
    , nu = 0.1
    , mu = 0.095
    , N = 100
    , I = 1
    , R = 0

)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(computations, during)
  , default = default
)
