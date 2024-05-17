library(macpan2)

initialize_state = list(S ~ N - I - R)

flows = list(
    mp_per_capita_flow("S", "I", infection ~ I * beta / N)
  , mp_per_capita_flow("I", "R", recovery ~ gamma)
)

default = list(
    beta = 0.2
  , gamma = 0.1
  , N = 100
  , I = 1
  , R = 0
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = flows
  , default = default
)
