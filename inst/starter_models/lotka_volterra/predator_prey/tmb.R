library(macpan2)

initialize_state = list(
    X ~ 100 # prey
  , Y ~ 100 # predator
)

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

state_updates = list(
    X ~ X + growth_x - mortality_x
  , Y ~ Y + growth_y - mortality_y
)

## set defaults
default = list(   
    alpha = 0.1 # prey growth
  , beta = 0.08 # prey loss from predation
  , gamma = 0.05 # predator mortality
  , delta = 0.1 # predator gain from predation
)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(flow_rates, state_updates)
  , default = default
)





## alternate parameterization of model
## logistic prey growth, and non-linear functional responses
# flow_rates = list(
#   ## growth rate of prey
#   growth_x ~ alpha * X * (1 - (X/K))
#   ## mortality rate of predator
#   , mortality_y ~ gamma * Y
#   ## effects from predation
#   ## compute functional response
#   , functional_response ~ holling_iii(a = 5, h = 1, X = X, k = 3)
#   #, functional_response ~ holling_ii(a = 5, h = 1, X = X)
#   ## mortality rate of prey (due to predation)
#   , mortality_x ~ beta *  functional_response * Y
#   ## growth rate of predator (due to predation)
#   , growth_y ~ delta *  functional_response * Y
# )


##########################################
## old way to specify model and defaults

expr_list =  ExprList(
    during = c(
      flow_rates
    , state_updates
  )
  , after = model_evaluation
)

# set defaults
init_mats = MatsList(
    X = 100
  , Y = 100
  , alpha = 0.1
  , beta = 0.08
  , gamma = 0.05
  , delta = 0.1
  , growth_x = empty_matrix
  , growth_y = empty_matrix
  , mortality_x = empty_matrix
  , mortality_y = empty_matrix
  , I_obs = empty_matrix
  , I_obs_times = empty_matrix
  , .mats_to_save = c("X","Y")
  , .mats_to_return = c("X","Y")
)
