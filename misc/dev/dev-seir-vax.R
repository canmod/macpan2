library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(
  init_mats = MatsList(
      beta.vax = 0.15
    , beta.unvax = 0.30
    , alpha. = 0.25
    , gamma. = 0.07
    , .dose_rate = 0.05
    , N.unvax = empty_matrix
    , N.vax = empty_matrix
    , foi.unvax = empty_matrix
    , foi.vax = empty_matrix
    , dummy = empty_matrix
    , state = c(89, 1, 0, 0, 9, 1, 0, 0)
    , from = c(0, 0, 1, 1, 2, 2, 3, 4, 5, 6)
    , to = c(1, 4, 2, 5, 3, 6, 7, 5, 6, 7)
    , rate = rep(0, 10)
    , flow = empty_matrix
    , .mats_to_save = c("state",  "rate", "flow")
    , .mats_to_return = c("state", "rate", "flow")
    , .dimnames = list(
      state = list(c("S.unvax", "E.unvax", "I.unvax", "R.unvax", "S.vax", "E.vax", "I.vax", "R.vax"), "")
    )
  ),
  expr_list = ExprList(
    before = list(
        dummy ~ assign(rate, 1, 0, .dose_rate)
      , dummy ~ assign(rate, 2, 0, alpha.)
      , dummy ~ assign(rate, 3, 0, .dose_rate)
      , dummy ~ assign(rate, 4, 0, gamma.)
      , dummy ~ assign(rate, 5, 0, .dose_rate)
      , dummy ~ assign(rate, 6, 0, .dose_rate)
      , dummy ~ assign(rate, 8, 0, alpha.)
      , dummy ~ assign(rate, 9, 0, gamma.)
    ),
    during = list(
        N.unvax ~ sum(state[0:3])
      , N.vax ~ sum(state[4:7])
      , foi.unvax ~ beta.unvax*state[2]/N.unvax
      , foi.vax ~ beta.vax*state[6]/N.vax
      , dummy ~ assign(rate, 0, 0, foi.unvax)
      , dummy ~ assign(rate, 7, 0, foi.vax)
      , flow ~ rate*state[from]
      , state ~ state +group_sums(flow, to, 8) - group_sums(flow, from, 8)
    ),
  ),
  params = OptParamsList(0.25, 0.07,
                            par_id = 0:1
                          , mat = c("alpha.", "gamma.")
                          , row_id = rep(0L, 2L)
                          , col_id = rep(0L, 2L)
                         ),
  random = OptParamsList(),
  obj_fn = ObjectiveFunction(~0),
  time_steps = Time(10L)
)

f = m$make_ad_fun("dev")
v = f$report()$values
