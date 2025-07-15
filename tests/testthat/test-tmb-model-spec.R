library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
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
