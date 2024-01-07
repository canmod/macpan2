library(macpan2)

## define model as expression list
computations = list(
  N ~ sum(S, I, R)
)

flow_rates = list(
    birth ~ birth_rate * N
  , infection ~ S * I * beta / N
  , recovery ~ gamma * I
)

state_updates = list(
    S ~ S - infection + birth - death_rate * S
  , I ~ I + infection - recovery - death_rate * I
  , R ~ R + recovery - death_rate * R
)

model_evaluation = list(
  log_likelihood ~ dpois(I_obs, rbind_time(I, I_obs_times))
)

expr_list =  ExprList(
  during = c(
      computations
    , flow_rates
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
  , gamma = 0.1
  , N = 100
  , birth_rate = 0.1
  , death_rate = 0.08
  , birth = empty_matrix
  , infection = empty_matrix
  , recovery = empty_matrix
  , log_likelihood = empty_matrix
  , I_obs = empty_matrix
  , I_obs_times = empty_matrix
  , .mats_to_save = c("I", "N")
  , .mats_to_return = c("I", "N")
)