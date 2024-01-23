sir_sim = mp_dynamic_simulator(dynamic_model
  , vectors = list(
      state = c(S = 999, I = 1, R = 0),
      flow_rates = c(lambda = NA, gamma = 0.1),
      trans_rates = c(beta = 0.25)
    )
  , time_steps = 100L
)
mp_trajectory(sir_sim)
dynamic_model
