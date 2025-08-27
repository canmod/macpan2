library(macpan2)
spec = mp_tmb_model_spec(
    during = list(
          mp_inflow("N", "r", "importation")
        , mp_per_capita_outflow("N", "d", "death")
    )
  , default = list(N = 0, r = 0.3, d = 0.2)
)
