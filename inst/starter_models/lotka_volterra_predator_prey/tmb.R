library(macpan2)
functional_response = "holling_1" # one of "holling_1", "holling_2" or "holling_3"

## LV predator prey flow rates with type 1
## Holling functional response
holling_1_flow_rates = list(
    ## growth rate of prey
    growth_x ~ alpha * X * (1 - K_inverse*(X))
    ## mortality rate of predator
  , mortality_y ~ gamma * Y
  
    ## effects from predation
    ## compute functional response
    ## not sure if named list elements will create problems
  , fr = functional_response ~ a * X
    ## mortality rate of prey (due to predation)
  , mortality_x ~ functional_response * Y
    ## growth rate of predator (due to predation)
  , growth_y ~ delta * functional_response * Y
)

flow_rates = list(
  
    ## type 1 Holling response
    holling_1 = holling_1_flow_rates
    
    ## type 2 Holling response  
  , holling_2 = modifyList(holling_1_flow_rates, list(fr = functional_response ~ a * X / (1 + a * h * X)))
    
    ## type 3 Holling response
  , holling_3 = modifyList(holling_1_flow_rates, list(fr = functional_response ~ a * (X ^ k) / (1 + a * h * (X ^ k))))
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
  , a = 1/500         # predator attack rate (Holling type 1,2,3)
  , h = 1             # handling time (Holling type 2,3)
  , k = 2             # Holling type 3 exponent parameter (k > 1)
  # initialize state
  , X = 100           # prey
  , Y = 100           # predators
  
)

## model specification
specs = lapply(flow_rates, \(flow_rates) {
    mp_tmb_model_spec(
    during = c(flow_rates, state_updates)
  , default = default
)})


spec = specs[[functional_response]]


## functional responses
## these functions are not currently used
## instead see flow_rates list for all
## three types of functional responses

#' Holling type I
#'
#' @param a predator attack rate; prey/predator/time_unit
#' 
#' @return 
holling_i <- function(a, X){
  # return("a * X")
  a * X
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


