library(macpan2)
library(TMB)

compile('macpan2.cpp')
dyn.load(dynlib("macpan2"))


correct_answer = function(beta = 0.3) {

  ## matrices
  state = c(1 - 1e-2, 1e-2, 0)
  gamma = 0.2
  N = 1
  foi = 0
  ratemat = matrix(0, 3, 3)
  flowmat = matrix(0, 3, 3)

  state_hist = list(as.matrix(state))
  N_hist = list(as.matrix(N))
  foi_hist = list(as.matrix(foi))
  for (i in 1:2) {
    N = sum(state)
    foi = beta * state[2] / N
    ratemat = matrix(
      c(
          0, foi, 0,
          0, 0,   gamma,
          0, 0,   0
      ), 3, 3, byrow = TRUE
    )
    flowmat = sweep(ratemat, 1, state, "*")
    state = state - rowSums(flowmat) + colSums(flowmat)
    state_hist = c(state_hist, list(as.matrix(state)))
    N_hist = c(N_hist, list(as.matrix(N)))
    foi_hist = c(foi_hist, list(as.matrix(foi)))
  }
  state = state_hist
  N = N_hist
  foi = foi_hist
  return(
    nlist(state, N, foi)
  )
}

sir = TMBModel(
  init_mats = MatsList(
    state = c(1 - 1e-2, 1e-2, 0),
    beta = 0.3,
    gamma = 0.2,
    N = 1,
    foi = 0,
    ratemat = matrix(0, 3, 3),
    flowmat = matrix(0, 3, 3),
    .mats_to_save = c("state", "N", "foi"),
    .mats_to_return = c("state", "N", "foi")
  ),
  expr_list = ExprList(
    before = list(
      N ~ sum(state)
    ),
    during = list(
      foi ~ beta * state[1, 0] / N,
      ratemat ~ matrix(c(
        0,   0,     0,
        foi, 0,     0,
        0,   gamma, 0), 3, 3),
      flowmat ~ ratemat * state,
      state ~ state - rowSums(flowmat) + t(colSums(flowmat))
    )
  ),
  params = OptParamsList(0.3
    , par_id = 0L
    , mat = "beta"
    , row_id = 0L
    , col_id = 0L
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~ foi + 1),
  time_steps = Time(time_steps = 2L)
)
sir$data_arg()
sir$param_arg()

data_args = sir$data_arg()
parameter_args = sir$param_arg()
random_args = sir$random_arg()
print(random_args)

print("data args ...")
print(data_args)

print("parameter args ...")
print(parameter_args)

tmb_function = try(TMB::MakeADFun(
  data = data_args,
  parameters = parameter_args,
  random = random_args,
  DLL = 'macpan2'
))

# print("correct answer ...")
# correct_answer()  ## expected result
#
# print("actual answer ...")
# tmb_output = try(tmb_function$report())  ## actual result
#
#
correct_answer(0.1)
tmb_function$report(0.1)
