library(macpan2)

initialize_state = list(
    I ~ 1
  , R ~ 0
  , E ~ 0
  , S ~ N - I
)

computations = list(
  N ~ sum(S, E, I, R)
)

flow_rates = list(
    exposure ~ S * I * beta / N
  , infection ~ alpha * E
  , recovery ~ gamma * I
)

update_state = list(
    S ~ S - exposure
  , E ~ E + exposure - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

## set defaults
default = list(
    beta = 0.2
  , alpha = 1/2
  , gamma = 0.1
  , N = 100
  )

## model specification
spec = mp_tmb_model_spec(
    before = c(initialize_state, computations)
  , during = c(flow_rates, update_state)
  , default = default
)
