library(macpan2)
library(TMB)

compile('dev.cpp')
dyn.load(dynlib("dev"))

m = TMBModel(init_mats = MatsList(
      state = c(99, 1, 0)
    , I = 1  # index into state for retrieving the I box
    , N = empty_matrix
    , beta = empty_matrix
    , foi = empty_matrix
    , xx = empty_matrix
    , ratemat = empty_matrix
    , flowmat = empty_matrix
    , t = 0
    , X = cbind(seq_len(100), 1)[,2:1]
    , b = c(-1.3, 0.1)
    , gamma = 0.1
    , e = exp(1)
    , .mats_to_return = c("state", "beta", "xx")
    , .mats_to_save = "state"
    , .dimnames = list(
      state = list(c("S", "I", "R"), "")
    )
  )
  , expr_list = ExprList(
    before = list(
        beta ~ e ^ (X %*% b)
      , N ~ sum(state)
    ),
    during = list(
        t ~ t + 1
      , foi ~ beta[t,0] * state[I,0] / N
      , ratemat ~ matrix(
        c(
            0,   0,     0,
            foi, 0,     0,
            0,   gamma, 0
        ), 3, 3
      )
      , flowmat ~ ratemat * state
      , state ~ state - rowSums(flowmat) + t(colSums(flowmat))
    ),
    after = list(
      xx ~ state[c(0, 2), 0]
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

library(oor)
TMBSimulator = function(tmb_model, tmb_cpp) {
  self = Base()
  self$tmb_model = tmb_model
  self$tmb_cpp = tmb_cpp
  self$matrix_names = s$tmb_model$.init_mats$.names()
  self$ad_fun = self$tmb_model$make_ad_fun(self$tmb_cpp)
  self$report = function(..., .phases = c("before", "during", "after")) {
    fixed_params = as.numeric(unlist(list(...)))
    r = setNames(
      as.data.frame(self$ad_fun$report(fixed_params)$table_to_return),
      c("matrix", "time", "row", "col", "value")
    )
    r$matrix = self$matrix_names[r$matrix + 1L]
    dn = self$tmb_model$.init_mats$.dimnames
    for (mat in names(dn)) {
      i = r$matrix == mat
      r[i,"row"] = dn[[mat]][[1L]][r[i,"row"] + 1L]
      r[i,"col"] = dn[[mat]][[2L]][r[i,"col"] + 1L]
    }
    r$time = as.integer(r$time)
    # r$row = as.integer(r$row)
    # r$col = as.integer(r$col)
    num_t = self$tmb_model$.time_steps$.time_steps
    if (!"before" %in% .phases) {
      r = r[r$time != 0L,,drop = FALSE]
    }
    if (!"during" %in% .phases) {
      r = r[(r$time < 1L) | (r$time > num_t),,drop=FALSE]
    }
    if (!"after" %in% .phases) {
      r = r[r$time < num_t + 1,,drop=FALSE]
    }
    r
  }
  return_object(self, "TMBSimulator")
}
s = TMBSimulator(m, "dev")
aa = s$report(c(-1.6, 0.03), .phases = "after")
filter(aa, matrix %in% c("state", "xx"))

kk = s$report(c(-1.6, 0.03), .phases = c("before", "during"))
bb = s$report(c(-1.6, 0.03), .phases = "after")
head(kk)
library(ggplot2)
library(dplyr)
ggplot(filter(kk, matrix == "state")) + geom_line(aes(x = time, y = value, colour = row))
ggplot(filter(bb, matrix == "beta")) + geom_line(aes(x = as.integer(row) + 1, y = value))
