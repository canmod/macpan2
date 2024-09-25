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
  , mp_per_capita_flow("I4", "D", "rho", "death01")
  , mp_per_capita_flow("A4", "D", "sigma", "death02")
  , mp_per_capita_flow("I1", "A1", "tau", "protection1")
  , mp_per_capita_flow("I2", "A2", "tau", "protection2")
  , mp_per_capita_flow("I3", "A3", "tau", "protection3")
  , mp_per_capita_flow("I4", "A4", "tau", "protection4")
  , mp_per_capita_flow("A1", "I1", "phi", "unprotection1")
  , mp_per_capita_flow("A2", "I2", "phi", "unprotection2")
  , mp_per_capita_flow("A3", "I3", "phi", "unprotection3")
  , mp_per_capita_flow("A4", "I4", "phi", "unprotection4")
  , mp_per_capita_flow("S", "D_bg", "mu", "death03")
  , mp_per_capita_flow("I1", "D_bg", "mu", "death04")
  , mp_per_capita_flow("I2", "D_bg", "mu", "death05")
  , mp_per_capita_flow("I3", "D_bg", "mu", "death06")
  , mp_per_capita_flow("I4", "D_bg", "mu", "death07")
  , mp_per_capita_flow("A1", "D_bg", "mu", "death08")
  , mp_per_capita_flow("A2", "D_bg", "mu", "death09")
  , mp_per_capita_flow("A3", "D_bg", "mu", "death10")
  , mp_per_capita_flow("A4", "D_bg", "mu", "death11")
  , mp_per_capita_inflow("S", "S", "beta", "birth1")
  , mp_per_capita_inflow("I1", "S", "beta", "birth2")
  , mp_per_capita_inflow("I2", "S", "beta", "birth3")
  , mp_per_capita_inflow("I3", "S", "beta", "birth4")
  , mp_per_capita_inflow("I4", "S", "beta", "birth5")
  , mp_per_capita_inflow("A1", "S", "beta", "birth6")
  , mp_per_capita_inflow("A2", "S", "beta", "birth7")
  , mp_per_capita_inflow("A3", "S", "beta", "birth8")
  , mp_per_capita_inflow("A4", "S", "beta", "birth9")
)

default = list(
    alpha = 0.1
  , lambda0 = 0.002
  , n = 1
  , beta = 0.02
  , mu = 0.01
  , epsilon = 0.2
  , rho = 0.15
  , tau = 0.8
  , phi = 0.015
  , sigma = 0.1
  , S = 999
  , I1 = 1, I2 = 0, I3 = 0, I4 = 0, A1 = 0, A2 = 0, A3 = 0, A4 = 0
  , D = 0
  , D_bg = 0
)

spec = mp_tmb_model_spec(
    during = c(fn, flows)
  , default = default
)
  