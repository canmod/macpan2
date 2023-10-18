library(macpan2)
library(macpan2helpers)
library(oor)
library(ggplot2)
library(dplyr)

debug(macpan2:::get_indices)


dir = file.path("inst", "model_library", "sir_age")
f = macpan2:::ModelFiles(dir)
m = macpan2:::Model(f)

m$indices$flow$per_capita$flow()
m$infection_info$connections[[1]]$frame()
m$infection_info$connections[[1]]$from_vars()

sir = read_partition("inst", "model_library", "sir", "variables.csv")
loc = partition(Loc = c("a", "b", "c"))
cartesian_self(
    cartesian(sir, loc)
  , "infection"
  , "infectious"
  , "Type"
  , "Type.Vec"
)



cartesian(sir, loc)

v = read_partition("inst", "model_library", "sir_age", "variables.csv")
cartesian_self(v, "infection", "infectious", "Type")

if (FALSE) {
engine_eval(~1)
engine_eval(~ groupSums(x, i, 2), x = 1:6, i = c(0,0,0,1,1,1))
oor_debug$flag(".simulation_formatter", "TMBSimulator")

#options(macpan2_dll = "dev")
#TMB::compile("misc/dev/dev.cpp")
#dyn.load(TMB::dynlib('misc/dev/dev'))
options(oor_catch_dollar = FALSE)
epi_dir = system.file("model_library", "sir", package = "macpan2")
epi_dir = file.path("inst", "model_library", "sir")
epi_dir = system.file("starter_models", "product_example", "Epi_model", package = "macpan2")
# oor_debug$flag(".fetch")
#oor_debug$flag(".parse_table", "ObjectiveFunction")
#oor_debug$unflag("data_arg")
#oor_debug$flag(".assign_num_a_table_rows")
# oor_debug$flag(".type", "Variables")
# oor_debug$print_flagged()
# oor_debug$.objects$.objects
#oor_debug$flag(".assign_table")
#trace(macpan2:::connection_merge, quote(.counter <<- .counter + 1), print = FALSE)
#.counter = 0
model = Compartmental(epi_dir)
#print(.counter)
indices = model$indices
model$labels$state()
#print(.counter)
model$labels$flow()
#print(.counter)

#undebug(oor::clean_method_environment)

iv = IntVecs(
    state_length = as.integer(length(model$labels$state()))

  , per_capita_from = indices$flow$per_capita$from()
  , per_capita_to = indices$flow$per_capita$to()
  , per_capita_flow = indices$flow$per_capita$flow()

  , absolute_from = indices$flow$absolute$from()
  , absolute_to = indices$flow$absolute$to()
  , absolute_flow = indices$flow$absolute$flow()

  , per_capita_inflow_from = indices$flow$per_capita_inflow$from()
  , per_capita_inflow_to = indices$flow$per_capita_inflow$to()
  , per_capita_inflow_flow = indices$flow$per_capita_inflow$flow()

  , per_capita_outflow_from = indices$flow$per_capita_outflow$from()
  , per_capita_outflow_flow = indices$flow$per_capita_outflow$flow()

  , absolute_inflow_to = indices$flow$absolute_inflow$to()
  , absolute_inflow_flow = indices$flow$absolute_inflow$flow()

  , absolute_outflow_from = indices$flow$absolute_outflow$from()
  , absolute_outflow_flow = indices$flow$absolute_outflow$flow()

  , infection = model$indices$transmission$infection_flow()
  , infectious = model$indices$transmission$infectious_state()

)
m = TMBModel(
  init_mats = MatsList(
      state = c(S = 1 - 1e-5, E = 1e-5, I = 0, R = 0)
    , flow = c(total_foi = NA_real_, progression = 0.2, recovery = 0.01)
    , trans = 0.25
    , per_capita = empty_matrix
    , per_capita_inflow = empty_matrix
    , per_capita_outflow = empty_matrix
    , absolute = empty_matrix
    , absolute_inflow = empty_matrix
    , absolute_outflow = empty_matrix
    , total_inflow = empty_matrix
    , total_outflow = empty_matrix
    , .mats_to_save = "state"
    , .mats_to_return = "state"
  ),
  expr_list = ExprList(
    during = list(
        flow[infection] ~ trans * state[infectious]
      , per_capita ~ state[per_capita_from] * flow[per_capita_flow]
      , per_capita_inflow ~ state[per_capita_inflow_from] * flow[per_capita_inflow_flow]
      , per_capita_outflow ~ state[per_capita_outflow_from] * flow[per_capita_outflow_flow]
      , absolute ~ flow[absolute_flow]
      , absolute_inflow ~ flow[absolute_inflow_flow]
      , absolute_outflow ~ flow[absolute_outflow_flow]
      , total_inflow ~
          groupSums(per_capita, per_capita_to, state_length) +
          groupSums(absolute, absolute_to, state_length) +
          groupSums(per_capita_inflow, per_capita_inflow_to, state_length) +
          groupSums(absolute_inflow, absolute_inflow_to, state_length)
      , total_outflow ~
          groupSums(per_capita, per_capita_from, state_length) +
          groupSums(absolute, absolute_from, state_length) +
          groupSums(per_capita_inflow, per_capita_outflow_from, state_length) +
          groupSums(absolute_inflow, absolute_outflow_from, state_length)
      , state ~ state + total_inflow - total_outflow
    )
  ),
  engine_methods = EngineMethods(int_vecs = iv),
  time_steps = Time(100L),
  params = OptParamsList(0.5, 0.2, 0.1
    , par_id = 0:2
    , mat = c("trans", "flow", "flow")
    , row_id = 0:2
    , col_id = rep(0L, 3L)
  ),
  obj_fn = ObjectiveFunction(~ sum(state^2))
)
s = m$simulator()
s$optimize$optim()$value
s$optimize$optim()$par
s$objective(s$optimize$optim()$par)
r = s$report()

if (interactive()) {
  (r
    |> mutate(state = factor(row, levels = topological_sort(model)))
    |> ggplot()
    + geom_line(aes(time, value, colour = state))
  )
} else {
  print(head(r, 100))
  print(tail(r, 100))
}

#if (FALSE) {
valid_int_vecs = m$engine_methods$int_vecs
valid_vars = m$init_mats
valid_funcs = macpan2:::valid_funcs
pp = macpan2:::make_expr_parser("pp", finalizer_char)
qq = macpan2:::make_expr_parser("qq", finalizer_index)
pp(~ state[infectious])
qq(~ flow[infection, infectious])$parse_table
ff = function(x) qq(x)$parse_table
xx = m$expr_list$expr_list()
xx = append(xx, flow[infection] ~ trans %*% state[infectious])
yy = (xx
  |> lapply(macpan2:::lhs)
  |> lapply(ff)
)
(Reduce(rbind, yy)[1:3]
  |> setNames(sprintf("a_table_%s", c("x", "n", "i")))
  |> as.list()
)
vapply(yy, nrow, integer(1L))

sir_vax = Model(ModelFiles(system.file("starter_models", "sir_vax", package = "macpan2")))
#sir = Model(ModelFiles(system.file("starter_models", "sir", package = "macpan2")))
#oor_debug$flag("arguments", class = "Derivation")
# oor_debug$unflag("which_in")
# oor_debug$flag("which_not_in", class = "String")
# oor_debug$flag("order_by", class = "String")
dd = Derivations(sir_vax)
Rprof()
dd$during()
summaryRprof()



f1 = function(x) x$x
f2 = function(x) x[["x"]]
f3 = function(x) x[[1L]]
x = list(x = 1:1000)
microbenchmark::microbenchmark(
  f1(x), f2(x), f3(x)
)
debug(`%in%`)
Rprof()
summaryRprof()

dd$before()
dd$after()

dd = Derivation(sir_vax, 1L)
dd$expr_list()
do.call(mm, as.list(dd$all_arguments()[[1L]]))
(dd$all_arguments()[[1]])
dd$get()
dd$var_list()
dd$output_labels()
dd$get()$expression
dd$arguments()
dd$argument_dots()
dd$all_arguments()


dg_sir = DerivationGrouping(sir)
#debug(dg_sir$unique_var_lists)
dg_sir$unique_var_lists()
dg_sir$var_list(1L)
dg_sir$spec_map()

dg = DerivationGrouping(mf)
dg$n_derivations()
dg$spec_map()
dg$unique_var_lists()
dg$specs()[[4L]]
dg$var_list(1L)$vax

Derivation = function(derivation, model) {
  self = Base()
  self$derivation = derivation
  self$model = model
  self$groups = function() {
    grps = list()
    for (g in self$derivation$group_names) {
      grps[[g]] = self$model$variables$all()$filter(g
        , .wrt = self$derivation$group_partition
      )
    }
    grps
  }
  return_object(self, "Derivation")
}
lapply(mf$derivations(), getElement, "group_names")
lapply(mf$derivations(), getElement, "group_partition")
dd = Derivation(mf$derivations()[[3L]], m)
dd$groups()
# debug(FlowExpander)
# oor_debug$flag("filter_flow")
# oor_debug$flag(".filter_blanks_not_special")


mf$derivations()[[1L]]

m = Model(mf)
f = m$flows()
v = macpan2:::Variables(m)
debug(macpan2:::Variables)

xx = macpan2:::FilterTraits()

xx$.filter_blanks_not_special(m$variables$all()$frame(), "Epi", c("S", "I"))

m$variables()
m$flows()[1L, "from"]
ss = m$variables$state()

ss$filter(m$flows()[1L, "from"], .wrt = m$flows()[1L, "from_partition"])
ss$filter(m$flows()[1L, "to"], .wrt = m$flows()[1L, "to_partition"])
m$flows()[1L, "from_to_partition"]


xx = macpan2:::CompartmentalAlt(system.file("starter_models", "sir", package = "macpan2"))
y = xx$simulators$tmb(time_steps = 25L
  , state = c(S = 99, I = 1, R = 0)
  , flow = c(foi = NA, gamma = 0.1)
  , beta = 0.2
  , N = empty_matrix
)# |> Euler()
y$print$expressions()

y$report()

SimulatorConstructor(epi_dir,
   time_steps = 25L,
   state = c(S = 999, E = 1, I = 0, R = 0),
   flow = c(total_foi = NA, progression = 0.1, recovery = 0.05),
   N = empty_matrix,
   transmissability = 0.75,
   per_capita_transmission = empty_matrix,
   .mats_to_return = c("state")
)



ff = Formula(flow[infection] ~ per_capita_transmission %*% state[infectious])
ff$meth_list()
ff$expr_list()
ff$mat_args()

ff = Formula(foi[e] ~ beta * I / N)
ff$could_be_getter()
ff$could_be_setter()


ff$could_be_getter()
ff$setter_name()
ff$getter_name()

Formula(absolute ~ flow[absolute_flow], "xx")$getter_name()

epi_dir = system.file("starter_models", "product_example", "Epi_model", package = "macpan2")
model = Compartmental(epi_dir)
model$indices$transmission$infectious_state()

for (type in model$indices$flow$flow_types) {
  model$indices$flow[[type]]
}
indices = model$indices
model$expr_list()

m = TMBModel(time_steps = Time(100))
m$insert$expressions()
f = flow[infection] ~ per_capita_transmission %*% state[infectious]
xx = macpan2:::MethodTypes()
y = try(xx$make_method(f, "f"), silent = TRUE)
y
meth_types = macpan2:::MethodTypes()
meth = meth_types$make_method(f, "set_infection_flows")
macpan2:::mat_vec_nms(f)
meth$int_vec_args
meth$mat_args

xx$meth_mat_mult_to_rows$int_vec_args(f)
xx$meth_mat_mult_to_rows$int_vec_arg_nms
xx$meth_mat_mult_to_rows$mat_args(f)



MatsExprMeth = function(init_mats = list(), int_vecs = list(), before = list(), during = list(), after = list()) {
  meth_types = macpan2:::MethodTypes()
  mat_nms = names(init_mats)
  vec_nms = names(int_vecs)
  sort_expr_list = function(expr_list) {
    expr_args = list()
    meth_args = list()
    mat_args = character(0L)
    vec_args = character(0L)
    expr_nms = names(expr_list)
    for (i in seq_along(expr_list)) {
      meth_i = try(meth_types$make_method(expr_list[[i]], expr_nms[i]), silent = TRUE)
      if (inherits(meth_i, "try-error")) {
        expr_args = append(expr_args, expr_list[[i]])
        mat_args = append(mat_args, macpan2:::mat_vec_nms(expr_list[[i]]))
      } else {
        meth_args = c(meth_args, setNames(list(expr_list[[i]]), expr_nms[[i]]))
        new_expr = macpan2:::two_sided()
        # if (is_setter(meth_i$formula)) {
        #
        # } else if (is_getter(meth_i$formula)) {
        #
        # }
      }
    }
  }
}

TMBModel(
  init_mats = MatsList(
      state = c(S = 1 - 1e-5, E = 1e-5, I = 0, R = 0)
    , flow = c(total_foi = NA_real_, progression = 0.1, recovery = 0.1)
    , per_capita_transmission = 0.2
    , per_capita = empty_matrix
    , per_capita_inflow = empty_matrix
    , per_capita_outflow = empty_matrix
    , absolute = empty_matrix
    , absolute_inflow = empty_matrix
    , absolute_outflow = empty_matrix
    , total_inflow = empty_matrix
    , total_outflow = empty_matrix
    , . = empty_matrix
    , .mats_to_save = c("state", "absolute")
    , .mats_to_return = c("state", "absolute")
  ),
  expr_list = ExprList(
    during = list(
        . ~ set_infection_flow
      , per_capita ~ get_per_capita
      , per_capita_inflow ~ get_per_capita_inflow
      , per_capita_outflow ~ get_per_capita_outflow
      , absolute ~ get_absolute
      , absolute_inflow ~ get_absolute_inflow
      , absolute_outflow ~ get_absolute_outflow
      , total_inflow ~ get_per_capita_state_in + get_per_capita_state_in + get_per_capita_inflow_state_in + get_absolute_inflow_state_in
      , total_outflow ~ get_per_capita_state_out + get_absolute_state_out + get_per_capita_outflow_state_out + get_absolute_outflow_state_out
      , state ~ state + total_inflow - total_outflow
    )
  ),
  engine_methods = EngineMethods(
    exprs = list(
        set_infection_flow = flow[infection] ~ per_capita_transmission %*% state[infectious]
      , get_per_capita = ~ state[per_capita_from] * flow[per_capita_flow]
      , get_absolute = ~ flow[absolute_flow]
      , get_per_capita_inflow = ~ state[per_capita_inflow_from] * flow[per_capita_inflow_flow]
      , get_per_capita_outflow = ~ state[per_capita_outflow_from] * flow[per_capita_outflow_flow]
      , get_absolute_inflow = ~ flow[absolute_inflow_flow]
      , get_absolute_outflow = ~ flow[absolute_outflow_flow]
      , get_per_capita_state_in = ~ groupSums(per_capita, per_capita_to, state_length)
      , get_absolute_state_in = ~ groupSums(absolute, absolute_to, state_length)
      , get_per_capita_inflow_state_in = ~ groupSums(per_capita_inflow, per_capita_inflow_to, state_length)
      , get_absolute_inflow_state_in = ~ groupSums(absolute_inflow, absolute_inflow_to, state_length)
      , get_per_capita_state_out = ~ groupSums(per_capita, per_capita_from, state_length)
      , get_absolute_state_out = ~ groupSums(absolute, absolute_from, state_length)
      , get_per_capita_outflow_state_out = ~ groupSums(per_capita_outflow, per_capita_outflow_from, state_length)
      , get_absolute_outflow_state_out = ~ groupSums(absolute_outflow, absolute_outflow_from, state_length)
    ),
    int_vecs = IntVecs(
        state_length = length(model$labels$state())

      , per_capita_from = indices$flow$per_capita$from()
      , per_capita_to = indices$flow$per_capita$to()
      , per_capita_flow = indices$flow$per_capita$flow()

      , absolute_from = indices$flow$absolute$from()
      , absolute_to = indices$flow$absolute$to()
      , absolute_flow = indices$flow$absolute$flow()

      , per_capita_inflow_from = indices$flow$per_capita_inflow$from()
      , per_capita_inflow_to = indices$flow$per_capita_inflow$to()
      , per_capita_inflow_flow = indices$flow$per_capita_inflow$flow()

      , per_capita_outflow_from = indices$flow$per_capita_outflow$from()
      , per_capita_outflow_flow = indices$flow$per_capita_outflow$flow()

      , absolute_inflow_to = indices$flow$absolute_inflow$to()
      , absolute_inflow_flow = indices$flow$absolute_inflow$flow()

      , absolute_outflow_from = indices$flow$absolute_outflow$from()
      , absolute_outflow_flow = indices$flow$absolute_outflow$flow()

      , infection = model$indices$transmission$infection_flow()
      , infectious = model$indices$transmission$infectious_state()

    )
  ),
  time_steps = Time(100L)
)
s = m$simulator()
r = s$report(.phases = "during")
if (interactive()) {
  (r
    |> filter(row == "I")
    |> ggplot()
    + geom_line(aes(time, value))
  )
} else {
  print(r)
}

filter(r, matrix == "absolute")


if (FALSE) {
m = TMBModel(
  init_mats = MatsList(
      state = c(S = 1 - 1e-2, I = 1e-2, R = 0)
    , flow = c(infection = 0, recovery = 0.1)
    , per_capita_transmission_matrix = 0.15
    , per_capita = empty_matrix
    , beta_ts = c(0.11, 0.2, 0.4)
    , dummy = empty_matrix
    , .mats_to_save = "state"
    , .mats_to_return = "state"
  ),
  expr_list = ExprList(
    during = list(
        per_capita_transmission_matrix ~ beta
      , dummy ~ update_infection_flows
      , per_capita ~ from_states * flow
      , state ~ state - outflow + inflow
    )
  ),
  time = Time(150),
  engine_methods = EngineMethods(
    exprs = list(
        from_states = ~ state[from_indices]
      , infectious_states = ~ state[infectious_indices]
      , update_infection_flows = flow[infection_indices] ~
          per_capita_transmission_matrix %*% state[infectious_indices]
      , inflow = ~ groupSums(per_capita, to_indices, state_length)
      , outflow = ~ groupSums(per_capita, from_indices, state_length)
      , beta = ~ time_var(beta_ts, beta_cp, beta_n, beta_group)
    ),
    int_vecs = IntVecs(
        from_indices = 0:1
      , to_indices = 1:2
      , state_length = 3L
      , infectious_indices = 1L
      , infection_indices = 0L
      , beta_cp = c(0L, 50L, 100L)
      , beta_n = 1L
      , beta_group = 0L
    )
  )
)
m$data_arg()
s = m$simulator()
r = s$report(.phases = "during")
if (interactive()) {
  (r
    |> filter(row == "I")
    |> ggplot()
    + geom_line(aes(time, value))
  )
} else {
  print(r)
}
}
}
