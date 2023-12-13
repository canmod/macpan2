library(macpan2)

## expression lists
###################################################

## free form computations for convenience
computations = list(
    N ~ sum(S, I, R)
)

## absolute flow rates (per time only)
flow_rates = list(
    infection ~ S * I * beta / N
  , recovery ~ gamma * I
)

## state updates
state_updates = list(
    S ~ S - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

model_evaluation = list(
  log_likelihood ~ dpois(I_obs, rbind_time(I, I_obs_times))
)

## simple unstructured scalar expression
expr_list =  ExprList(
    before = computations
  , during = c(flow_rates, state_updates)
  , after = model_evaluation
)

## defaults
init_mats = MatsList(
     S = 99
   , I = 1
   , R = 0
   , beta = 0.2
   , gamma = 0.1
   , N = 100
   , infection = empty_matrix
   , recovery = empty_matrix
   , log_likelihood = empty_matrix
   , I_obs = empty_matrix
   , I_obs_times = empty_matrix
   , .mats_to_save = "I"
   , .mats_to_return = "I"
)

obj_fn = ObjectiveFunction(~ -sum(log_likelihood))

## simulator
###################################################

tmb_simulator = TMBModel(
    init_mats = init_mats
  , expr_list = expr_list
  , obj_fn = obj_fn
)$simulator()
