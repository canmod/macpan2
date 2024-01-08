library(macpan2)

initialize_states = list(I ~ 1, S ~ N - I, E ~ 0, R ~ 0)

computations = list(
  N ~ sum(S, E, I, R)
)

flow_rates = list(
    exposure ~ S * I * beta / N
  , infection ~ alpha * E
  , recovery ~ gamma * I
)

state_updates = list(
    S ~ S - exposure
  , E ~ E + exposure - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)
model_evaluation = list(
  log_likelihood ~ dpois(I_obs, rbind_time(I, I_obs_times))
)

## set defaults
default = list(  beta = 0.2
               , alpha = 1/2
               , gamma = 0.2
               , N = 100
               , I_obs = 0
               , I_obs_times = 0)

## model specification
model_spec = mp_tmb_model_spec(
    before = c(initialize_states, computations)
  , during = c(flow_rates, state_updates)
  , after = model_evaluation
  , default = default
)

##########################################
## old way to specify model and defaults

expr_list =  ExprList(
    before = computations
  , during = c(
    flow_rates
    , state_updates
  )
  , after = model_evaluation
)

# set defaults
init_mats = MatsList(
    S = 99
  , E = 0
  , I = 1
  , R = 0
  , beta = 0.2
  , alpha = 1/2
  , gamma = 0.2
  , N = 100
  , exposure = empty_matrix
  , infection = empty_matrix
  , recovery = empty_matrix
  , log_likelihood = empty_matrix
  , I_obs = empty_matrix
  , I_obs_times = empty_matrix
  , .mats_to_save = c("I","E")
  , .mats_to_return = c("I","E")
)