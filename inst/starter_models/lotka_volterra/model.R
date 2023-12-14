library(macpan2)

## expression lists
###################################################

## free form computations for convenience
## none

## absolute flow rates (per time only)
## reference 
flow_rates = list(
  ## growth rate of species X and Y
    growth_x ~ rx * X
  , growth_y ~ ry * Y
  ## intraspecific effect
  ## species X on X
  , intraspecific_x ~ growth_x * axx * X
  ## species Y on Y
  , intraspecific_y ~ growth_y * ayy * Y
  ## interspecific effect
  ## species X on Y
  , interspecific_xy ~ growth_x * ayx * Y
  ## species Y on X
  , interspecific_yx ~ growth_y * axy * X
)

## state updates
state_updates = list(
    X ~ X + growth_x - intraspecific_x - interspecific_xy
  , Y ~ Y + growth_y - intraspecific_y - interspecific_yx
)

## simple unstructured scalar expression
expr_list =  ExprList(
  during = c(
      flow_rates
    , state_updates
  )
)
