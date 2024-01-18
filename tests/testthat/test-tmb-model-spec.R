library(macpan2)
si = mp_tmb_model_spec(
    before = list(
        I ~ 1
      , S ~ N - I
    )
  , during = list(
        infection_rate ~ beta * S * I / N
      , S ~ S - infection_rate
      , I ~ I + infection_rate
    )
  , default = list(N = 100)
)
# si$all_default_vars()
# si$all_derived_vars()
# si$all_formula_vars()
mp_simulator(si, 10, "I", default = list(beta = 0.25))
