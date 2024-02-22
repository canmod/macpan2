library(macpan2)

initialize_state = list(
  S ~ N - I - R
)

flow_rates = list(
    infection ~ S * I * beta / N
  , recovery ~ gamma * I
)

update_state = list(
    S ~ S - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

## set defaults
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
  , during = c(flow_rates, update_state)
  , default = default
)
