library(macpan2)

initialize_state = list(
    I ~ 1
  , R ~ 0
  , S ~ N - I
)

compute_flow_rates = list(
    infection ~ S * I * beta / N
  , recovery ~ gamma * I
)

update_state = list(
    S ~ S - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(compute_flow_rates, update_state)
  , default = list(N = 100, beta = 0.2, gamma = 0.1)
)
