library(macpan2)

initialize_state = list(
    R ~ theta[R_initial]
  , S ~ N - I - R
)

flow_rates = list(
    infection ~ S * I * theta[beta] / N
  , recovery ~ gamma * I
)

update_state = list(
    S ~ S - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

## set defaults
default = list(
    theta = c(beta = 0.2, R_initial = 0)
  , gamma = 0.1
  , N = 100
  , I = 1
)

integers = list(
    R_initial = mp_positions("R_initial", names(default$theta))
  , beta = mp_positions("beta", names(default$theta))
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, update_state)
  , default = default
  , integers = integers
)
