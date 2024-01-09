library(macpan2)

computations = list(
  N ~ sum(S, I, R)
)

flow_rates = list(
    infection ~ S * I * beta / N
  , recovery ~ gamma * I
  , waning_immunity ~ phi * R
)

state_updates = list(
    S ~ S - infection + waning_immunity
  , I ~ I + infection - recovery
  , R ~ R + recovery - waning_immunity
)

# model spec
spec = mp_tmb_model_spec(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
  # defaults
  , default = list(
      S = 99, I = 1, R = 0
    , beta = 0.2, gamma = 0.2, phi = 0.01
  )
)

