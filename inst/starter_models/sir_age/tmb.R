library(macpan2)

# SIR with age, where age structure only affects transmission, as 
# defined here: https://www.nature.com/articles/s41467-020-20544-y#Sec7

# define the model
spec = mp_tmb_model_spec(
  before = list(
    S ~ N - I - R # calculate population size before the simulation loop begins to avoid having to specify a value for it by hand in the defaults list
  ),
  during = list(
    # force of infection
    lambda ~ tau * M %*% (I / N),
    # infection
    mp_per_capita_flow(
      from = "S", to = "I",
      rate = "lambda",
      abs_rate = "infection"
    ),
    # recovery
    mp_per_capita_flow(
      from = "I", to = "R",
      rate = "gamma",
      abs_rate = "recovery"
    )
  ),
  default = list(
    tau = tau,
    M = M,
    gamma = gamma, # 5 days avg infectious period; same gamma across age groups 
    N = N,
    I = c(I0, 0, 0), # seed infection in age group 1
    R = rep(0, 3)
  )
)
