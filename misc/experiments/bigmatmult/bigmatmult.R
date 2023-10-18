library(macpan2)
library(dplyr)
library(ggplot2)
macpan2:::dev_compile()
#dyn.load("/usr/local/lib/libprofiler.0.dylib")

# d = 20
# X = rbf(1000, d)
# plot(X %*% rnorm(d), X %*% rnorm(d), type = "l")

# model_starter("sir", "misc/experiments/bigmatmult/model")

# obs_incidence = rbf(2661, 20) %*% runif(20, max = 5)
# plot(obs_incidence, type = "l")

d = 20 ## dimension of the radial basis
left = Sys.time()
sir = Compartmental(
  "misc/experiments/bigmatmult/model"
)$simulators$tmb(
      time_steps = 2661
    , state = c(S = 100000 - 500, I = 500, R = 0)
    , flow = c(foi = 0, gamma = 0.2, wane = 0.01)
    , beta = 1
    , N = 100000
    , X = rbf(2661, d)
    , b = rnorm(d, sd = 0.01)
    , incidence = empty_matrix
    , eta = empty_matrix
    , .mats_to_save = c("state", "incidence", "beta")
    , .mats_to_return = c("state", "incidence", "beta")
    , .tmb_cpp = "dev"
)$insert$expressions(
    eta ~ N * gamma * exp(X %*% b)
  , .phase = "before"
  , .at = Inf
)$insert$expressions(
    beta ~ eta[time_step(1)] / clamp(S, 0.01)
  , .phase = "during"
  , .at = 1
)$insert$expressions(
    incidence ~ I
  , .vec_by_states = "total_inflow"
  , .phase = "during"
  , .at = Inf
)$replace$params(
    default = rnorm(d, sd = 0.01)
  , mat = rep("b", d)
  , row = seq_len(d) - 1L
)

(sir$report(rnorm(d, sd = 0.01), .phases = "during")
  %>% mutate(variable = if_else(matrix == "state", row, matrix))
  %>% ggplot()
  + facet_wrap(~ variable, ncol = 1, scales = 'free')
  + geom_line(aes(time, value))
)

plot(sir$report(rnorm(d, sd = 0.01), .phases = "during")$value, type = "l")
plot(sir)
sir$current$params_frame()
sir$get$initial("flow")

engine_eval(~clamp(2, 1))

sir$insert$expressions(
    log_lik ~ dpois(obs_incidence, I)
  , .phase = "during"
  , .at = Inf
  , .vec_by_states = "total_inflow"
)
sir$insert$expressions(
    log_lik ~ sum(rbind_time(log_lik))
  , .phase = "after"
  , .at = Inf
)
sir$replace$obj_fn(~ -log_lik)
sir$add$transformations(Logit("flow"))
sir$replace$params(
  default = c(100, 100000, qlogis(0.2), rep(0.5, 10)),
  mat = c("state", "state",  "logit_flow", rep("b", 10)),
  row = c(1, 0, 1, 0:9)
)

sir$optimize$nlminb(control = list(iter.max = 1000, eval.max = 1000))
sir$current$params_frame()
sir$optimize$optim()
sir$optimization_history$get()
sir$ad_fun()$par
sir$report(sir$current$params_vector())
sir$objective(sir$current$params_vector())
plot(filter(sir$report(.phases = "after"), matrix == "beta_values")$value, type = "l")
(filter(sir$report(.phases = "during"), matrix == "state")
  %>% ggplot()
  + facet_wrap(~row, scales = 'free', ncol = 1)
  + geom_line(aes(time, value))
)

sim_incidence = filter(sir$report(.phases = "during"), matrix == "total_inflow", row == 1L)$value

plot(sim_incidence, obs_incidence[1:2661], type = "l")

#beta_values = filter(sir$report(.phases = "after"), matrix == "beta_values")
rr = sir$report(.phases = "during")
rr = sir$report(sir$current$params_vector(), .phases = "during")
right = Sys.time()
right - left

(ggplot(filter(rr, matrix == "state"))
  + facet_wrap(~row, ncol = 1, scales = 'free')
  + geom_line(aes(time, value))
)

(ggplot(filter(rr, matrix == "total_inflow"))
  + facet_wrap(~row, ncol = 1, scales = 'free')
  + geom_line(aes(time, value))
)

ggplot(data.frame(time = 0:2661, value = obs_incidence)) + geom_line(aes(time, value))


plot(beta_values$value, type = "l")

## nothing big -- 0.6819479
## big X small T -- 0.834451
## small X big T -- 2.698912
## big X and T -- 30.11578
## big X and T with resetting of X before the loop -- 3.078685
