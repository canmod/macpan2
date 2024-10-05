library(macpan2)

initialize_state = list(
  S ~ N - V - E - I - H - R
)

constant_computations = list(
  N ~ sum(S, V, E, I, H, R)
)

dynamic_computations = list(
  N_mix ~ N - H
)


flow_rates = list(
    mp_per_capita_flow("S", "V", "((a * S)/(b + S))/S", "vaccination")
  , mp_per_capita_flow("V", "S", "rho"                , "vaccine_waning")
  , mp_per_capita_flow("S", "E", "I * beta_s/N_mix"   , "unvaccinated_infection")
  , mp_per_capita_flow("V", "E", "I * beta_v/N_mix"   , "vaccinated_infection")
  , mp_per_capita_flow("E", "I", "alpha"              , "progression")
  , mp_per_capita_flow("I", "R", "gamma_i"            , "infectious_recovery")
  , mp_per_capita_flow("I", "H", "sigma"              , "hospitalizations")
  , mp_per_capita_flow("H", "R", "gamma_h"            , "hospital_recovery")
)

## set defaults
default = list(
    a = 10
  , b = 10
  , rho = 0.05
  , beta_s = 0.2
  , beta_v = 0.05
  , alpha = 1/2
  , gamma_i = 0.1
  , gamma_h = 0.07
  , sigma = 0.05
  , N = 100
  , I = 1
  , V = 0
  , E = 0
  , H = 0
  , R = 0
  )

spec = mp_tmb_model_spec(
  before = c(initialize_state, constant_computations)
  , during = c(dynamic_computations, flow_rates)
  , default = default
)
