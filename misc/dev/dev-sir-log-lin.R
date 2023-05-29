library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(init_mats = MatsList(
      state = c(99, 1, 0)
    , I = 1  # index into state for retrieving the I box
    , N = empty_matrix
    , beta = empty_matrix
    , beta_curr = empty_matrix
    , foi = empty_matrix
    , t = 0
    , X = cbind(seq_len(110), 1)[,2:1]
    , b = c(-1.3, 0.1)
    , gamma = 0.1
    , from = c(0, 1)
    , to = c(1, 2)
    , rate = empty_matrix
    , flow = empty_matrix
    , .mats_to_return = c("state", "beta_curr", "foi", "N")
    , .mats_to_save = c("state", "beta_curr", "foi", "N")
    , .dimnames = list(
      state = list(c("S", "I", "R"), "")
    )
  )
  , expr_list = ExprList(
    before = list(
        beta ~ exp(X %*% b)
      , beta_curr ~ beta[0,0]
      , N ~ sum(state)
    ),
    during = list(
        t ~ t + 1
      , beta_curr ~ beta[t,0]
      , foi ~ beta_curr * state[I,0] / N
      , rate ~ c(foi, gamma)
      , flow ~ state[from, 0] * rate
      , state ~ state - groupSums(flow, from, 3) + groupSums(flow, to, 3)
    )
  )
  , params = OptParamsList(-1.6, -0.04
    , par_id = 0:1
    , mat = rep("b", 2L)
    , row_id = 0:1
    , col_id = rep(0L, 2L)
  )
  , random = OptParamsList()
  , obj_fn = ObjectiveFunction(~0)
  , time_steps = Time(100L)
)
#m$data_arg()
#f = m$make_ad_fun("dev")
#f$report(m$params$vector())$values

s = TMBSimulator(m, "dev")
r = s$report(-1.6, 0.03, .phases = c("before", "during"))
r
# library(tidyr)
# pivot_wider(filter(r, matrix != "beta"), id_cols = time, names_from = c(matrix, row, col), values_from = value) %>%
#   mutate(foi = beta_curr_0_0 * lag(state_I_, 1) / N_0_0) %>%
#   View
# aa = s$report(c(-1.6, 0.03), .phases = "after")

# filter(aa, matrix %in% c("state", "xx"))
#
# kk = s$report(c(-1.6, 0.03), .phases = c("before", "during"))
# bb = s$report(c(-1.6, 0.03), .phases = "after")
# head(kk)
# library(ggplot2)
# library(dplyr)
# ggplot(filter(kk, matrix == "state")) + geom_line(aes(x = time, y = value, colour = row))# + ylim(0, 100)
# ggplot(filter(bb, matrix == "beta")) + geom_line(aes(x = as.integer(row) + 1, y = value))
