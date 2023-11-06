## collect information into a simulator -----------------------

sir_sim = mp_tmb_simulator(dynamic_model
  , vectors = init_vecs
  , time_steps = 100L
)
mp_report(sir_sim)
