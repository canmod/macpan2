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
  , recovered ~ gamma * I
)

## state updates
state_updates = list(
    S ~ S - infection
  , I ~ I + infection - recovered
  , R ~ R + recovered
)

model_evaluation = list(
  log_likelihood ~ dpois(I_obs, rbind_time(I, I_obs_times))
)

## data model comparison
obj_fn = ~ -sum(log_likelihood)

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
   , recovered = empty_matrix
   , log_likelihood = empty_matrix
   , I_obs = empty_matrix
   , I_obs_times = empty_matrix
   , .mats_to_save = "I"
   , .mats_to_return = "I"
)

## simulator
###################################################

tmb_simulator = TMBModel(
    init_mats = init_mats
  , expr_list = expr_list
  , time_steps = Time(100L)
  , params = OptParamsList(
       init_mats$get("beta")
     , par_id = c(0L)
     , mat = c("beta")
     , row_id = c(0L)
     , col_id = c(0L)
  )
  , obj_fn = ObjectiveFunction(obj_fn)
)$simulator()
