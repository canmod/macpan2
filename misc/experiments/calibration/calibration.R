library(macpan2)
library(dplyr)
sir = Compartmental(file.path("inst", "starter_models", "sir"))
N = 100
sir$labels$state()
sir$labels$flow()
sir$labels$other()
sir$flows()
# --------
simulator = sir$simulators$tmb(time_steps = 100
  , state = c(S = N - 1, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = empty_matrix
  , beta = 0.4
)
simulator$print$matrix_dims()
simulator$print$expressions()

## generate simulated data so that i can fit the generating
## model to these data for a sanity check -- can we recover
## the parameters from the simulating model?
sims = simulator$report(.phases = "during")
deterministic_prevalence = (sims
  %>% filter(row == "I")
  %>% pull(value)
)
set.seed(1L)
observed_I = rpois(100, deterministic_prevalence)

sir$labels$all()

## Step 1: add observed data and declare matrices storing
##         the simulation history of variables to compare
##         with observed data, as well as a matrix for
##         storing log-likelihood values. need to make sure
##         that the simulation histories are saved, and
##         if you want you can return them as well as the
##         log likelihood values.
simulator$add$matrices(
    observed_I = observed_I
  , simulated_I = empty_matrix
  , log_lik = empty_matrix
  , .mats_to_save = "simulated_I"
  , .mats_to_return = c("log_lik", "simulated_I")
)
simulator$print$matrix_dims()

## Step 2: collect simulated values into matrices to be
##         compared with data. note here that `1` indicates
##         the position of the I state. the `.at = Inf` and
##         `.phase = "during"` indicates that this expression
##         should come at the end of the expressions evaluated
##         during each iteration of the simulation loop.
simulator$insert$expressions(
    simulated_I ~ I
  , .at = Inf  ## place the inserted expressions at the end of the expression list
  , .phase = "during"
)
simulator$print$expressions()

## Step 3: compute any values that will be part of the
##         objective function to be optimized. here we
##         have the log of the Poisson density of the
##         observed `I` values with mean (i.e. predicted)
##         value at the simulated `I` values. the
##         `rbind_time` function gathers together the
##         full simulation history of the `simulated_I`
##         matrix by binding together the rows at each
##         iteration.
simulator$insert$expressions(
  log_lik ~ dpois(
    observed_I,  ## observed values
    clamp(rbind_time(simulated_I))  ## simulated values
  )
 , .at = Inf
 , .phase = "after"
)
simulator$print$expressions()

## Step 4: specify the objective function (very often
##         this will be minus the sum of the log likelihoods).
simulator$replace$obj_fn(~ -sum(log_lik))

## Step 5: declare (and maybe transform) parameters to be optimized
simulator$add$matrices(
  log_beta = log(0.6),
  logit_gamma = qlogis(0.4)
)

simulator$insert$expressions(
    beta ~ exp(log_beta)
  , gamma ~ 1 / (1 + exp(-logit_gamma))
  , .phase = "before"
)

simulator$replace$params(
  default = c(log(0.6), qlogis(0.4)),
  mat = c("log_beta", "logit_gamma")
)
# simulator$replace$random(
#   default = qlogis(0.2),
#   mat = "logit_gamma"
# )

simulator$print$expressions()

## Step 6: use the engine object
simulator$optimize$optim()

simulator$optimize$optim(method = "Brent", lower = -10, upper = 0)
simulator$optimize$nlminb()

xx = simulator$optimization_history$get()
simulator$current$params_frame()
simulator$cache$invalidate()

refreshed_objective = function(...) {
  simulator$cache$invalidate()  ## start every evaluation from the default
  simulator$objective(...)
}

log_beta_values = seq(from = -3, to = -0.001, len = 100)
neg_log_lik = vapply(log_beta_values, refreshed_objective, numeric(1L))
plot(log_beta_values, neg_log_lik, type = 'l', las = 1)
plot(exp(log_beta_values), neg_log_lik, type = "l", las = 1)
refreshed_objective(log_beta_values[which(is.nan(neg_log_lik))] + 1e-3)
