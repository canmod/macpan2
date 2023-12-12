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

## simple unstructured scalar expression
expr_list =  ExprList(
    before = computations
  , during = c(
      flow_rates
    , state_updates
    )
)

## simulate
###################################################

tmb_simulator = TMBModel(
    init_mats = MatsList(  S = 99
                         , I = 1
                         , R = 0
                         , beta = 0.2
                         , gamma = 0.1
                         , N = 100
                         , infection = empty_matrix
                         , recovered = empty_matrix
                         , .mats_to_save = c("I")
                         , .mats_to_return = c("I")
                       )
  , expr_list = expr_list
  , time_steps = Time(100L)
  , params = OptParamsList(  0.2, 0.3
                           , par_id = c(0L,1L)
                           , mat = c("beta","gamma")
                           , row_id = c(0L,0L)
                           , col_id = c(0L,0L)
                         )
  
)$simulator()

plot(tmb_simulator$report_values(0.4,0.4))



