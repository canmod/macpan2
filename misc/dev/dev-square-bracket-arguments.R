library(macpan2)
library(TMB)


compile('dev.cpp')
dyn.load(dynlib("dev"))

m_two_arg_sqr_brckt = TMBModel(
  init_mats = MatsList(
    state = c(99, 1, 0),
    from = c(0, 1),
    to = c(1, 2), 
    beta = 0.07,
    gamma = 0.12, 
    N = empty_matrix,
    foi = empty_matrix,
    rate = empty_matrix,
    flow = empty_matrix,
    .mats_to_save = list("state", "rate", "flow"),
    .mats_to_return = list("state", "rate", "flow")
  ),
  expr_list = ExprList(
    during = list(
      N ~ sum(state),
      foi ~ beta*state[1,0]/N,
      rate ~ c(foi, gamma),
      flow ~ state[from, 0]*rate,
      state ~ state - groupSums(flow, from, 3) + groupSums(flow, to, 3)
    )
  ),
  params = OptParamsList(0.05, 0.12,
                         par_id = 0:1,
                         mat = c("beta", "gamma"),
                         row_id = rep(0L, 2L),
                         col_id = rep(0L, 2L)
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(5L)
)

m_one_arg_sqr_brckt = TMBModel(
  init_mats = MatsList(
    state = c(99, 1, 0),
    beta = 0.07,
    gamma = 0.12, 
    from = c(0, 1),
    to = c(1, 2), 
    N = empty_matrix,
    foi = empty_matrix,
    rate = empty_matrix,
    flow = empty_matrix,
    .mats_to_save = list("state", "rate", "flow"),
    .mats_to_return = list("state", "rate", "flow")
  ),
  expr_list = ExprList(
    during = list(
      N ~ sum(state),
      foi ~ beta*state[1]/N,
      rate ~ c(foi, gamma),
      flow ~ state[from]*rate,
      state ~ state - groupSums(flow, from, 3) + groupSums(flow, to, 3)
    )
  ),
  params = OptParamsList(0.05, 0.12,
                         par_id = 0:1,
                         mat = c("beta", "gamma"),
                         row_id = rep(0L, 2L),
                         col_id = rep(0L, 2L)
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(5L)
)

m_three_arg_sqr_brckt = TMBModel(
  init_mats = MatsList(
    state = c(99, 1, 0),
    beta = 0.07,
    gamma = 0.12, 
    from = c(0, 1),
    to = c(1, 2), 
    N = empty_matrix,
    foi = empty_matrix,
    rate = empty_matrix,
    flow = empty_matrix,
    .mats_to_save = list("state", "rate", "flow"),
    .mats_to_return = list("state", "rate", "flow")
  ),
  expr_list = ExprList(
    during = list(
      N ~ sum(state),
      foi ~ beta*state[1, 0, 0]/N,
      rate ~ c(foi, gamma),
      flow ~ state[from, 0, 0]*rate,
      state ~ state - groupSums(flow, from, 3) + groupSums(flow, to, 3)
    )
  ),
  params = OptParamsList(0.05, 0.12,
                         par_id = 0:1,
                         mat = c("beta", "gamma"),
                         row_id = rep(0L, 2L),
                         col_id = rep(0L, 2L)
  ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(5L)
)

s_one_arg_sqr_brckt = TMBSimulator(m_one_arg_sqr_brckt, "dev")
s_two_arg_sqr_brckt = TMBSimulator(m_two_arg_sqr_brckt, "dev")
s_three_arg_sqr_brckt = TMBSimulator(m_three_arg_sqr_brckt, "dev")

s_one_arg_sqr_brckt$report(0.05, 0.12)
s_two_arg_sqr_brckt$report(0.05, 0.12)
s_three_arg_sqr_brckt$report(0.05, 0.12)
