library(macpan2)

## expression lists
###################################################

## free form computations for convenience
computations = list(
  N ~ sum(S, E, I, R)
)

## absolute flow rates (per time only)
flow_rates = list(
    exposure ~ S * I * beta / N
  , infection ~ alpha * E
  , recovery ~ gamma * I
)

## state updates
state_updates = list(
    S ~ S - exposure
  , E ~ E + exposure - infection
  , I ~ I + infection - recovery
  , R ~ R + recovery
)

## simple unstructured scalar expression
expr_list =  ExprList(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
)
