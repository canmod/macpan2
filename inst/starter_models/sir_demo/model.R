library(macpan2)

## expression lists
###################################################

## free form computations for convenience
computations = list(
  N ~ sum(S, I, R)
)

## absolute flow rates (per time only)
flow_rates = list(
    birth ~ birth_rate * N
  , infection ~ S * I * beta / N
  , recovery ~ gamma * I
)

## state updates
state_updates = list(
    S ~ S - infection + birth - death_rate * S
  , I ~ I + infection - recovery - death_rate * I
  , R ~ R + recovery - death_rate * R
)

## simple unstructured scalar expression
## in the general case,
## birth rate and death rate can differ, N might not be constant
## nothing can be computed before simulation loop
expr_list =  ExprList(
    during = c(
        computations
      , flow_rates
      , state_updates
  )
)

