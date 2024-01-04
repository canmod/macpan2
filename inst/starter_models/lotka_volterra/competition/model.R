library(macpan2)

## expression lists
###################################################

## free form computations for convenience
## none

## absolute flow rates (per time only)
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
  ## species Y on X
  , interspecific_xy ~ growth_x * axy * Y
  ## species X on Y
  , interspecific_yx ~ growth_y * ayx * X
)

## alternate parameterization of model by carrying 
## capacity and relative interspecific effects
flow_rates = list(
  ## growth rate of species X and Y
    growth_x ~ rx * X
  , growth_y ~ ry * Y
  ## intraspecific effect
  ## species X on X
  , intraspecific_x ~ growth_x * X / Kx
  ## species Y on Y
  , intraspecific_y ~ growth_y * Y / Ky
  ## interspecific effect
  ## species Y on X
  , interspecific_xy ~ growth_x * alpha_xy * Y / Kx
  ## species X on Y
  , interspecific_yx ~ growth_y * alpha_yx * X / Ky
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
