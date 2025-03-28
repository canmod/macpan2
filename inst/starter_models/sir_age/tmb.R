library(macpan2)

# SIR with age, where age structure only affects transmission, as 
# defined here: https://www.nature.com/articles/s41467-020-20544-y#Sec7

# set numerical values
N <- rep(1e5, 3) # total population by age
I0 <- 100 # initial infecteds

M <- matrix(c(
  10, 1, 1, 
  1, 5, 1, 
  1, 1, 2
  ), nrow = 3) # contact matrix in units of avg number of contacts/person/day (symmetric contact rates because we will pick two age groups of the same size)
gamma <- 1/5 # 5 days avg infectious period; same gamma across age groups

R0 <- 2
# spectral radius of contact matrix for transmissibility calculation
rho <- max(abs(eigen(M)$values))
# calculate transmissibility given R0, gamma, M
tau <- R0*gamma/rho # transmissibility (proportion of contacts with infectious individuals that yield infection)


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
