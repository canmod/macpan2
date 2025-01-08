library(macpan2)

## flow diagram specification
flows = list(
    mp_per_capita_flow("S", "I", "beta * I / N", "infection")
  , mp_per_capita_flow("I", "R", "gamma", "recovery")
)

## default values for quantities required to run simulations
default = list(
    beta = 0.2   ## transmission rate
  , gamma = 0.1  ## recovery rate
  , N = 100      ## total population size (constant in this model)
  , I = 1        ## initial number of infectious individuals
  , R = 0        ## initial number of recovered individuals
)

## compute the initial number of susceptible individuals
initialize_state = list(S ~ N - I - R)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = flows
  , default = default
)
