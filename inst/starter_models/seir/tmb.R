library(macpan2)

initialize_state = list(
  S ~ N - I - R - E
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
  , I = 1
  , E = 0
  , R = 0
  )

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, update_state)
  , default = default
)
