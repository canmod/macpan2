library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(
    S = 99, 
    E=1, 
    I=0,
    R=0,
    alpha = 0.05,
    beta = 0.07,
    gamma = 0.12, 
    N = empty_matrix,
    foi = empty_matrix,
    from = c(0, 1, 2),
    to = c(1, 2, 3), 
    state = empty_matrix,
    rate = empty_matrix,
    flow = empty_matrix,
    .mats_to_save = list("S", "E", "I", "R"),
    .mats_to_return = list("S", "E", "I", "R")
  ),
  expr_list = ExprList(
    during = list(
      state ~ c(S, E, I, R),
      N ~ sum(state),
      foi ~ beta*I/N,
      rate ~ c(foi, alpha, gamma),
      flow ~ state[from, 0]*rate,
      state ~ state - groupSums(flow, from, 4) + groupSums(flow, to, 4),
      S~state[0],
      E~state[1],
      I~state[2],
      R~state[3]
    )
  ),
  params = OptParamsList(0.05, 0.07, 0.12,
                         par_id = 0:2,
                         mat = c("alpha", "beta", "gamma"),
                         row_id = rep(0L, 3L),
                         col_id = rep(0L, 3L)
                         ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(50L)
)

s = TMBSimulator(m, "dev")
