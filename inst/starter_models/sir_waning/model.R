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
  , waning_immunity ~ wane * R
)

## state updates
state_updates = list(
    S ~ S - infection + waning_immunity
  , I ~ I + infection - recovery
  , R ~ R + recovery - waning_immunity
)

## simple unstructured scalar expression
expr_list =  ExprList(
    before = computations
  , during = c(
      flow_rates
    , state_updates
  )
)
