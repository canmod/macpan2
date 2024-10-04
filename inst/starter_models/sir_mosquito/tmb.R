# https://homepages.warwick.ac.uk/~masfz/ModelingInfectiousDiseases/Chapter4/Program_4.4/index.html

# H and M mean humans and mosquitos
# S and I mean susceptible and infectious
flows = list(
    # constant birth
    mp_per_capita_inflow("pool", "S_H", "nu_H", "birth_H")
  , mp_per_capita_inflow("pool", "S_M", "nu_M", "birth_M")
  
    # both species can get infected when M bites H
  , mp_per_capita_flow("S_H", "I_H", "r * beta_HM * I_M", "infection_H")
  , mp_per_capita_flow("S_M", "I_M", "r * beta_MH * I_H", "infection_M")
    
    # both species recover with perfect immunity and so we do not keep 
    # track of these individuals
  , mp_per_capita_outflow("I_H", "gamma_H", "recovery_H")
  , mp_per_capita_outflow("I_M", "gamma_M", "recovery_M")
    
    # everyone dies at the same per-capita rate
  , mp_per_capita_outflow("S_H", "mu_H", "death_S_H")
  , mp_per_capita_outflow("I_H", "mu_H", "death_I_H")
  , mp_per_capita_outflow("S_M", "mu_M", "death_S_M")
  , mp_per_capita_outflow("I_M", "mu_M", "death_I_M")
)

## parameters from https://homepages.warwick.ac.uk/~masfz/ModelingInfectiousDiseases/Chapter4/Program_4.4/Program_4_4.py
default = list(
    r = 0.5 / 1e3
  , beta_HM = 0.5
  , beta_MH = 0.8
  , mu_H = 5.5e-5
  , mu_M = 0.143
  , nu_H = 5.5e-2
  , nu_M = 1.443e3
  , gamma_H = 0.033
  , gamma_M = 0
  , S_H = 1e3
  , I_H = 1
  , S_M = 1e4
  , I_M = 1
  , pool = 1
)

spec = mp_tmb_model_spec(
    during = flows
  , default = default
)
