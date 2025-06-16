fn = list(
    Itotal ~ I1 + I2 + I3 + I4
  , Atotal ~ A1 + A2 + A3 + A4
  , I ~ Itotal + Atotal
  , J ~ Itotal + epsilon * Atotal
  , N ~ S + I
  , P ~ I / N
  , lambda ~ lambda0 * exp(-alpha * P^n)
)
flows = list(
    mp_per_capita_flow("S", "I1", "lambda * J / N", "infection")
  , mp_per_capita_flow("I1", "I2", "rho", "progression1")
  , mp_per_capita_flow("I2", "I3", "rho", "progression2")
  , mp_per_capita_flow("I3", "I4", "rho", "progression3")
  , mp_per_capita_flow("A1", "A2", "sigma", "progression4")
  , mp_per_capita_flow("A2", "A3", "sigma", "progression5")
  , mp_per_capita_flow("A3", "A4", "sigma", "progression6")
  , mp_per_capita_flow("I4", "D", "rho", "death_I")
  , mp_per_capita_flow("A4", "D", "sigma", "death_A")
  , mp_per_capita_flow("I1", "A1", "tau", "protection1")
  , mp_per_capita_flow("I2", "A2", "tau", "protection2")
  , mp_per_capita_flow("I3", "A3", "tau", "protection3")
  , mp_per_capita_flow("I4", "A4", "tau", "protection4")
  , mp_per_capita_flow("A1", "I1", "phi", "unprotection1")
  , mp_per_capita_flow("A2", "I2", "phi", "unprotection2")
  , mp_per_capita_flow("A3", "I3", "phi", "unprotection3")
  , mp_per_capita_flow("A4", "I4", "phi", "unprotection4")
  , mp_per_capita_outflow("S", "mu", "death_bg_S")
  , mp_per_capita_outflow("I1", "mu", "death_bg_I1")
  , mp_per_capita_outflow("I2", "mu", "death_bg_I2")
  , mp_per_capita_outflow("I3", "mu", "death_bg_I3")
  , mp_per_capita_outflow("I4", "mu", "death_bg_I4")
  , mp_per_capita_outflow("A1", "mu", "death_bg_A1")
  , mp_per_capita_outflow("A2", "mu", "death_bg_A2")
  , mp_per_capita_outflow("A3", "mu", "death_bg_A3")
  , mp_per_capita_outflow("A4", "mu", "death_bg_A4")
  , mp_per_capita_inflow("N", "S", "beta", "birth")
)


## parameters inspired by the appendix of this paper:
## https://www.pnas.org/doi/full/10.1073/pnas.1301801110
## 
## all rates are per year.
default = list(
  
  # per-capita demographics
    mu = 0.018   ## death rate
  , beta = 0.02  ## birth rate
  
  # per-capita progression rates
  , rho = mean(c(1/0.271, 1/8.31, 1/1.184, 1/1.316)) ## untreated
  , sigma = mean(c(1/8.21, 1/54, 1/2.463, 1/2.737))  ## treated
  
  # per-capita status transition rates
  , tau = 0.015  ## untreated to treated
  , phi = 0.015  ## treated to untreated
  
  , epsilon = 0.01  ## relative infectiousness of treated individuals
  
  , lambda0 = 0.35  ## baseline transmission rate
  , n = 0  ## non-linearity exponent
  , alpha = 0.1 ## non-linearity parameter
)  
## initial conditions
inits = list( 
    S = 1 ## susceptible
  , I1 = 1/400, I2 = 1/400, I3 = 1/400, I4 = 1/400 ## untreated infectious per stage
  , A1 = 0, A2 = 0, A3 = 0, A4 = 0                 ## treated infectious per stage
  , D = 0     ## death due to disease
)

spec = mp_tmb_model_spec(
    during = c(fn, flows)
  , default = default
  , inits = inits
)
