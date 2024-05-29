library(macpan2); library(testthat); library(dplyr); library(tidyr)
scaler = macpan2:::ConvolutedScaler("y", "z")
#debug(scaler$user_formulas)
scaler$user_formulas()
scaler$other_generated_formulas()
spec = mp_tmb_model_spec(
  during = list(
      y ~ x + 1
    , scaler
  )
)
spec$expand()

spec = mp_tmb_model_spec(
  during = list(
      k1_S ~ 1 ## rk4 coefficient name to test for name conflict avoidance
    , mp_per_capita_flow("S", "I", "beta * I", "incidence")
  )
)
spec |> mp_rk4() |> mp_expand()

CC = function(matrix) {
  self = macpan2:::ChangeComponent()
  self$matrix = matrix
  self$user_formulas = function() {
    list(
      as.formula(sprintf("%s_sum ~ sum(%s)", self$matrix, self$matrix))
    )
  }
  return_object(self, "Formula")
}

mp_tmb_model_spec(
  during = list(
      macpan2:::Formula(y_sum ~ x + 1)
    , CC("y")
  )
) |> mp_expand()

spec = mp_tmb_model_spec(
  during = list(a ~ aakjhsadfkjlhasdflkjhasdflkjhasdfkjlhadsfkjhasdjhfgasdhgfasdhgfhasdgfjhagsdf + jsdhfajhksdgfjakhsdgfkjahsdgfkjhasdfjhkagsdfkhjas + asdjhfbaksdjhfaksjdhfaskdjhf, b ~ d),
  default = list(aa = 1, d = 1)
)
spec
spec = mp_tmb_model_spec(
    before = list(S ~ N - I - R)
  , during = list(
        N ~ S + I + R
      , mp_per_capita_flow("S", "I", infection ~ beta * I / N)
      , mp_per_capita_flow("I", "R", recovery ~ gamma)
    )
  , default = list(N = 100, I = 1, R = 0, beta = 0.25, gamma = 0.1)
)

set.seed(10)
data = (spec 
  |> mp_euler_multinomial()
  |> mp_simulator(50, "infection") 
  |> mp_trajectory()
)
cal = (spec
  |> mp_rk4()
  |> mp_tmb_calibrator(data, "infection", "beta")
)
cal = (mp_rk4(spec)$expand()
  |> mp_tmb_calibrator(data, "infection", "beta")
)
print(cal)
mp_optimize(cal)


# library(ggplot2)
# (ggplot(sim)
#   + geom_line(aes(time, `50%`))
#   + geom_ribbon(aes(time, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2)
# )
#plot(sim$value, type = "l")
#lines(sim$value, col = "red")
