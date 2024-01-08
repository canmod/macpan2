library(macpan2)

initialize_state = list(
    I ~ 1
  , R ~ 0
  , S ~ N - I
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

model_evaluation = list(
  log_likelihood ~ dpois(I_obs, rbind_time(I, I_obs_times))
)

## set defaults
default = list(  beta = 0.2
               , gamma = 0.2
               , N = 100
               , I_obs = 0
               , I_obs_times = 0
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, update_state)
  , after = model_evaluation
  , default = default
)
