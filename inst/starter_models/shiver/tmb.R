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

# flow_rates = list(
#     vaccination ~ phi * S
#   , vaccine_waning ~ rho * V
#   , unvaccinated_exposure ~ S * I * beta_s / N_mix
#   , vaccinated_exposure ~ V * I * beta_v / N_mix
#   , infection ~ alpha * E
#   , infectious_recovery ~ gamma_i * I
#   , hospitalizations ~ sigma * I
#   , hospital_recovery ~ gamma_h * H
# )

flow_rates = list(
    mp_per_capita_flow("S", "V", vaccination ~ phi)
  , mp_per_capita_flow("V", "S", vaccine_waning ~ rho)
  , mp_per_capita_flow("S", "E", unvaccinated_exposure ~ I * beta_s/N_mix)
  , mp_per_capita_flow("V", "E", vaccinated_exposure ~  I * beta_v/N_mix)
  , mp_per_capita_flow("E", "I", infection ~ alpha)
  , mp_per_capita_flow("I", "R", infectious_recovery ~ gamma_i)
  , mp_per_capita_flow("I", "H", hospitalizations ~ sigma)
  , mp_per_capita_flow("H", "R", hospital_recovery ~ gamma_h)
)

# update_state = list(
#     S ~ S - vaccination + vaccine_waning - unvaccinated_exposure
#   , V ~ V + vaccination - vaccine_waning - vaccinated_exposure
#   , E ~ E + unvaccinated_exposure + vaccinated_exposure - infection
#   , I ~ I + infection - infectious_recovery - hospitalizations
#   , H ~ H + hospitalizations - hospital_recovery
#   , R ~ R + infectious_recovery + hospital_recovery
# )

## set defaults
default = list(
    phi = 0.1
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

## model specification
# spec = mp_tmb_model_spec(
#     before = c(initialize_state, constant_computations)
#   , during = c(dynamic_computations, flow_rates, update_state)
#   , default = default
# )
spec = mp_tmb_model_spec(
  before = c(initialize_state, constant_computations)
  , during = c(dynamic_computations, flow_rates)
  , default = default
)
