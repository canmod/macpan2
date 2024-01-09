library(macpan2)
integrator = "euler" ## could be euler, hazard, or rk4 (need to add demo_stoch)

initialize_state = list(
    I ~ 1
  , S ~ N - 1
)

flow_rates = list(
    euler = list(
      infection ~ beta * S * I / N
    )
  
  , hazard = list( ## expected value of the Euler-Binomial
      infection ~ S * (1 - exp(-beta * I / N))
    )
  , rk4 = list(
        S_rk4 ~ S
      , I_rk4 ~ I
      , k1_infection ~ beta * S_rk4 * I_rk4 / N
      
      , S_rk4 ~ S_rk4 - k1_infection / 2
      , I_rk4 ~ I_rk4 + k1_infection / 2
      , k2_infection ~ beta * S_rk4 * I_rk4 / N
      
      , S_rk4 ~ S_rk4 - k2_infection / 2
      , I_rk4 ~ I_rk4 + k2_infection / 2
      , k3_infection ~ beta * S_rk4 * I_rk4 / N
      
      , S_rk4 ~ S_rk4 - k3_infection
      , I_rk4 ~ I_rk4 + k3_infection
      , k4_infection ~ beta * S_rk4 * I_rk4 / N
      
      , infection ~ (
                k1_infection 
          + 2 * k2_infection 
          + 2 * k2_infection 
          +     k4_infection
        ) / 6
    )
  ## (demographic stochasticity)
  #, demo_stoch = flow_rates = list(
  #    infection ~ rbinom()  ## need to implement rbinom on the c++ side
  #)
)

update_state = list(
    S ~ S - infection
  , I ~ I + infection
)

specs = lapply(flow_rates, \(flow_rates) {
  mp_tmb_model_spec(
      before = initialize_state
    , during = c(flow_rates, update_state)
    , default = list(N = 100, beta = 0.2, gamma = 0.1)
  )
})

spec = specs[[integrator]]
