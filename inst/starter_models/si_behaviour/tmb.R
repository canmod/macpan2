library(macpan2)
spec = mp_tmb_model_spec(
    during = list(
        sigma ~ invlogit(switch_slope * (I - threshold))
      , alpha_t ~ alpha_max * sigma
      , mp_per_capita_flow("S", "I", "beta * I / N", "infection")
      , mp_per_capita_flow("S", "P", "alpha_t", "protection")
    )
  , inits = list(S = 99, I = 1, P = 0, N = 100)
  , default = list(
        beta = 0.1        # transmission rate
      , alpha_max = 0.1   # maximum per-captia protection rate
      , threshold = 50    # threshold I, above which people protect themselves
      , switch_slope = 1  # larger slopes make alpha approach a step function
    )
)
