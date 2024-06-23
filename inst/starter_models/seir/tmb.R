library(macpan2)

initialize_state = list(
  S ~ N - I - R - E
)

flows = list(
    mp_per_capita_flow("S", "E", exposure ~ I * beta / N)
  , mp_per_capita_flow("E", "I", infection ~ alpha)
  , mp_per_capita_flow("I", "R", recovery ~ gamma)
)

## set defaults
default = list(
    beta = 0.2
  , alpha = 1/2
  , gamma = 0.1
  , N = 100
  , I = 1
  , E = 0
  , R = 0
  )

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = flows
  , default = default
)
