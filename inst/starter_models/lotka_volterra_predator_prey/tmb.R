library(macpan2)

## functional responses

#' Holling type I
#'
#' @param a predator attack rate; prey/predator/time_unit
#' 
holling_i <- function(a, X){
  engine_eval(~ a * X, a=a, X=X)
}

#' Holling type II
#'
#' @param a predator attack rate; prey/predator/time_unit
#' @param h handling time (ex. 1 day)
#' 
holling_ii <- function(a, h, X){
  a * X / (1 + a * h * X)
}

#' Holling type III
#' 
#' @param a predator attack rate; prey/predator/time_unit
#' @param h handling time (ex. 1 day)
#' @param k exponent of prey density, k > 1 
#'
holling_iii <- function(a, h, X, k){
  a * (X ^ k) / (1 + a * h * (X ^ k))
}


## model 
initialize_state = list(
    X ~ 100 # prey
  , Y ~ 100 # predator
)


flow_rates = list(
    ## growth rate of prey
    growth_x ~ alpha * X * (1 - K_inverse*(X))
    ## mortality rate of predator
  , mortality_y ~ gamma * Y
    ## effects from predation
    ## compute functional response (f(X) = X, default)
  , functional_response ~ a * X
  
  # , functional_response ~ holling_i(a = a, X = X)
  # , functional_response ~ holling_ii(a = 5, h = 1, X = X)
  # , functional_response ~ holling_iii(a = 5, h = 1, X = X, k = 3)
  
    ## mortality rate of prey (due to predation)
  , mortality_x ~ functional_response * Y
    ## growth rate of predator (due to predation)
  , growth_y ~ delta * functional_response * Y
)

state_updates = list(
    X ~ X + growth_x - mortality_x
  , Y ~ Y + growth_y - mortality_y
)

## set defaults
default = list(   
    alpha = 1/5       # prey growth
  #, beta = 1/100     # prey loss from predation
  , gamma = 1/50      # predator mortality
  , delta = 1/10      # predator gain from predation
  , K_inverse = 1/500 # prey carrying capacity
  , a = 1/500         # predator attack rate
  , h = 1             # handling time
  
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, state_updates)
  , default = default
)





