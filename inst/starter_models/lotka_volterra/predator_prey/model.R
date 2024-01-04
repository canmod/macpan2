library(macpan2)

## expression lists
###################################################

## free form computations for convenience (none)

## functional responses
## Holling type I is trivial (modify beta as needed)

#' Holling type II
#'
#' @param a predator attack rate; prey/time unit?
#' @param h handling time (ex. 1 day)
#' 
holling_ii <- function(a, h, X){
  a * X / (1 + a * h * X)
}

#' Holling type III
#' 
#' @param a predator attack rate; prey/time unit?
#' @param h handling time (ex. 1 day)
#' @param k exponent of prey density, k > 1 
#'
holling_iii <- function(a, h, X, k){
  a * (X ^ k) / (1 + a * h * (X ^ k))
}


## absolute flow rates (per time only)

## exponential prey growth
flow_rates = list(
  ## growth rate of prey
    growth_x ~ alpha * X
  ## mortality rate of predator
  , mortality_y ~ gamma * Y
  ## effects from predation
  ## mortality rate of prey (due to predation)
  , mortality_x ~ beta * X * Y
  ## growth rate of predator (due to predation)
  , growth_y ~ delta * X * Y
)

## alternate parameterization of model
## logistic prey growth, and non-linear functional responses
flow_rates = list(
  ## growth rate of prey
    growth_x ~ alpha * X * (1 - (X/K))
  ## mortality rate of predator
  , mortality_y ~ gamma * Y
  ## effects from predation
  ## compute functional response
  , functional_response ~ holling_iii(a = 5, h = 1, X = X, k = 3)
  #, functional_response ~ holling_ii(a = 5, h = 1, X = X)
  ## mortality rate of prey (due to predation)
  , mortality_x ~ beta *  functional_response * Y
  ## growth rate of predator (due to predation)
  , growth_y ~ delta *  functional_response * Y
)


## state updates
state_updates = list(
    X ~ X + growth_x - mortality_x
  , Y ~ Y + growth_y - mortality_y
)

## simple unstructured scalar expression
expr_list =  ExprList(
  during = c(
      flow_rates
    , state_updates
  )
)
