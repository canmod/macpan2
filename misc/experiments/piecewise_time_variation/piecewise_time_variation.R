library(macpan2)
library(dplyr)
library(ggplot2)
sir = Compartmental(file.path("inst", "starter_models", "sir"))
N = 100
sir$labels$state()
sir$labels$flow()
sir$labels$other()
sir$flows()
# --------
simulator = sir$simulators$tmb(time_steps = 50
  , state = c(S = N - 1, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = empty_matrix
  , beta = empty_matrix
)
simulator$print$matrix_dims()
simulator$print$expressions()

beta_changepoints = c(0, 10, 15)
beta_values = c(0.8, 0.01, 0.4)

simulator$add$matrices(
    beta_values = beta_values
  , beta_changepoints = beta_changepoints
  , beta_pointer = 0
)

simulator$insert$expressions(
    beta_pointer ~ time_group(beta_pointer, beta_changepoints)
  , beta ~ beta_values[beta_pointer]
  , .phase = "during"
)

s = simulator$report(.phases = "during")
(s
  %>% rename(state = row)
  %>% mutate(state = factor(state, sir$labels$state()))
  %>% ggplot()
  + geom_line(aes(time, value, colour = state))
  + geom_vline(aes(xintercept = x), linetype = "dashed", alpha = 0.5, data = data.frame(x = beta_changepoints))
)
