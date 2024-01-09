library(macpan2)

initialize_state = list(
    I ~ 1
  , R ~ 0
  , S ~ N - I
)

computations = list(
  N ~ sum(S, I, R)
)

flow_rates = list(
    birth ~ birth_rate * N
  , infection ~ S * I * beta / N
  , recovery ~ gamma * I
)

update_state = list(
    S ~ S - infection + birth - death_rate * S
  , I ~ I + infection - recovery - death_rate * I
  , R ~ R + recovery - death_rate * R
)

## set defaults
default = list(
      beta = 0.2
    , gamma = 0.1
    , N = 100
    , birth_rate = 0.1
    , death_rate = 0.08
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(computations, flow_rates, update_state)
  , default = default
)
