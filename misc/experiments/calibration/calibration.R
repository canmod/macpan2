library(macpan2)
library(dplyr)
macpan2:::dev_compile()
dyn.load("/usr/local/lib/libprofiler.0.dylib")
sir = Compartmental(file.path("inst", "starter_models", "sir"))
N = 100
sir$labels$state()
sir$labels$flow()
sir$labels$other()
sir$flows()
# --------
simulator = sir$simulators$tmb(time_steps = 10
  , state = c(S = N - 1, I = 1, R = 0)
  , flow = c(foi = 0, gamma = 0.2)
  , N = N
  , beta = 0.4
  # , .mats_to_save = c(
  #     "state", "flow", "N", "beta", "state_length", "per_capita_from",
  #     "per_capita_to", "per_capita_flow", "absolute_from", "absolute_to",
  #     "absolute_flow", "per_capita_inflow_from", "per_capita_inflow_to",
  #     "per_capita_inflow_flow", "per_capita_outflow_from",
  #     "per_capita_outflow_flow", "absolute_inflow_to", "absolute_inflow_flow",
  #     "absolute_outflow_from", "absolute_outflow_flow", "per_capita",
  #     "absolute", "per_capita_inflow", "per_capita_outflow", "absolute_inflow",
  #     "absolute_outflow", "total_inflow", "total_outflow", "dummy"
  # )
  , .tmb_cpp = "dev"
)

simulator$print$matrix_dims()
simulator$print$expressions()

## generate simulated data so that i can fit the generating
## model to these data for a sanity check -- can we recover
## the parameters from the simulating model?
sims = simulator$report(.phases = "during")
if (FALSE) {
obs_time_steps = unique(sort(sample(1:100, 30)))
deterministic_prevalence = (sims
  %>% filter(row == "I")
  %>% pull(value)
)[obs_time_steps]




set.seed(1L)
observed_I = rpois(30, deterministic_prevalence)


## Step 1: add observed data and declare matrices storing
##         the simulation history of variables to compare
##         with observed data, as well as a matrix for
##         storing log-likelihood values. need to make sure
##         that the simulation histories are saved, and
##         if you want you can return them as well as the
##         log likelihood values.
simulator$add$matrices(
    observed_I = observed_I
  , obs_time_steps = obs_time_steps
  , simulated_I = empty_matrix
  , log_lik = empty_matrix
  , .mats_to_save = c("simulated_I", "total_inflow")
  , .mats_to_return = c("log_lik", "simulated_I", "total_inflow")
  , .dimnames = list(total_inflow = list(c("S", "I", "R"), ""))
)
simulator$print$matrix_dims()

## Step 2: collect simulated values into matrices to be
##         compared with data. note here that `1` indicates
##         the position of the I state. the `.at = Inf` and
##         `.phase = "during"` indicates that this expression
##         should come at the end of the expressions evaluated
##         during each iteration of the simulation loop.
simulator$insert$expressions(
    #trajectory = simulated_I ~ 0.1 * sum(total_inflow[c(20, 21)])
    trajectory = simulated_I ~ I
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
  likelihood = log_lik ~
    (
      dpois(
        observed_I,  ## observed values
        clamp(rbind_time(simulated_I, obs_time_steps))  ## simulated values
      )
      # + dpois(
      #   observed_var_a,
      #   clamp(rbind_time(sim_a, ts_a))
      # )
    )
 , .at = Inf
 , .phase = "after"
)
simulator$print$expressions()

## Step 4: specify the objective function (very often
##         this will be minus the sum of the log likelihoods).
simulator$replace$obj_fn(~ -sum(log_lik))

## Step 5: declare (and maybe transform) parameters to be optimized,
##         as well as starting values for the parameters to be optimized
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
plot(obs_time_steps, observed_I)
simulator$optimize$optim()
lines(1:100, filter(simulator$report(.phases = "during"), matrix == "state", row == "I")$value)
simulator$cache$invalidate()
lines(1:100, filter(simulator$report(.phases = "during"), matrix == "state", row == "I")$value, col = "red")

simulator$optimization_history$get()
}
