library(macpan2)
library(tidyverse)

sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
simulator = sir$simulators$tmb(time_steps = 50
                               , state = c(S = 99, I = 1, R = 0)
                               , flow = c(foi = 0, gamma = 0.2)
                               , N = empty_matrix
                               , beta = 0.8
)
(simulator$report(.phases = "during")
  %>% rename(state = row)
  %>% mutate(state = factor(state, sir$labels$state()))
  %>% ggplot() + geom_line(aes(time, value, colour = state))
)

simulator$add$matrices(beta_changepoints = c(0, 10, 15))

simulator$add$matrices(beta_values = c(0.8, 0.01, 0.4))

simulator$add$matrices(beta_pointer = 0)

simulator$insert$expressions(
  beta_pointer ~ time_group(beta_pointer, beta_changepoints), 
  .phase = "during"
)

simulator$insert$expressions(
  beta ~ beta_values[beta_pointer],
  .phase = "during"
)

simulator$print$expressions()


s = simulator$report(.phases = "during")
(s
  %>% rename(state = row)
  %>% mutate(state = factor(state, sir$labels$state()))
  %>% ggplot()
  + geom_line(aes(time, value, colour = state))
  + geom_vline(
    aes(xintercept = x), 
    linetype = "dashed", 
    alpha = 0.5, 
    data = data.frame(x = simulator$get$initial("beta_changepoints"))
  )
)

set.seed(1L)
I_observed = rpois(
  50, 
  filter(s, matrix == "state", row == "I")$value
)
plot(I_observed)

simulator$add$matrices(
  
  ## observed data
  I_obs = I_observed,
  
  ## simulated trajectory to compare with data
  I_sim = empty_matrix, 
  
  ## location of I in the state vector
  ## (the `-1L` bit is to get 0-based indices instead of 1-based)
  I_index = match("I", sir$labels$state()) - 1L, 
  
  ## matrix to contain the log likelihood values at 
  ## each time step
  log_lik = empty_matrix, 
  
  ## need to save the simulation history of each of these matrices
  .mats_to_save = c("I_sim", "log_lik")
)

simulator$insert$expressions(
  I_sim ~ state[I_index],
  .phase = "during"
)

simulator$insert$expressions(
  log_lik ~ dpois(I_obs, clamp(rbind_time(I_sim))),
  .phase = "after"
)
simulator$replace$obj_fn(~ -sum(log_lik))

simulator$add$transformations(Log("beta_values"))
simulator$replace$params(
  default = log(mean(simulator$get$initial("beta_values"))),
  mat = rep("log_beta_values", 3L),
  row = 0:2
)

simulator$optimize$nlminb()
simulator$current$params_frame()
simulator$optimization_history$get()
lines(1:50, filter(simulator$report(.phases = "during"), matrix == "state", row == "I")$value)
