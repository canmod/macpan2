library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

mats = MatsList(

  # These are vectors that are composed of several variables in the
  # variables.csv, but appear in the state or rate component of the
  # settings.json file
  state = c(99, 1),
  rate = c(0, 0),
  flow = empty_matrix,

  # these are indices into the rate and flow vectors that come out of the
  # flows.csv file
  from = c(0, 1),
  to = c(1, 0),

  # dummy matrix as return value for functions that are called
  # for their side effects, as opposed to their return value
  dummy = empty_matrix,

  # variables that appear in the state component of the settings.json file
  # are passed as indices into a state vector
  S = 0, I = 1,

  # variables that appear in the rate component of the settings.json file
  # are passed as indices into a rate vector
  foi = 0, gamma = 1,

  # variables that do not appear in the settings.json file are passed as
  # numeric matrices
  beta = 0.2,

  .mats_to_save = c("state", "rate"),
  .mats_to_return = c("state", "rate", "beta"),
  .dimnames = list(
    state = list(c("S", "I"), ""),
    rate = list(c("foi", "gamma"), "")
  )
)
exprs = ExprList(
  during = list(
    dummy ~ assign(rate, foi, 0, state[I, 0] * beta / sum(state)),
    flow ~ rate * state[from, 0],
    state ~ state + group_sums(flow, to, 2) - group_sums(flow, from, 2)
  )
)

m = TMBModel(
  init_mats = mats,
  expr_list = exprs,
  params = OptParamsList(0.2
    , par_id = 0L
    , mat = "beta"
    , col_id = 0L
    , row_id = 0L
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(30L)
)

s = TMBSimulator(m)
s$report(0.2)
ggplot(dplyr::filter(s$report(0.3), row == "I")) + geom_line(aes(time, value))





