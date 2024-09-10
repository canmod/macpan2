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
mp_simulator(si, 10, "I", default = list(beta = 0.25)) |> mp_trajectory()

# s = mp_tmb_model_spec(
#     during = list(z ~ z + 1, y ~ proportions(c(0, 0), 1/2, 1e-8))
#   , default = list(x = runif(10), z = 0)
#   , must_save = "y"
# )
# s |> mp_simulator(1, "y") |> mp_trajectory() |> pull(value)
