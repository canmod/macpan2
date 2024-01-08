library(macpan2)

## define model as expression list
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

model_evaluation = list(
  log_likelihood ~ dpois(I_obs, rbind_time(I, I_obs_times))
)

expr_list =  ExprList(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
  , after = model_evaluation
)

## set defaults
init_mats = MatsList(
    S = 99
  , I = 1
  , R = 0
  , beta = 0.2
  , gamma = 0.2
  , phi = 0.01
  , N = 100
  , infection = empty_matrix
  , recovery = empty_matrix
  , waning_immunity = empty_matrix
  , log_likelihood = empty_matrix
  , I_obs = empty_matrix
  , I_obs_times = empty_matrix
  , .mats_to_save = c("I","R","waning_immunity")
  , .mats_to_return = c("I","R","waning_immunity")
)

