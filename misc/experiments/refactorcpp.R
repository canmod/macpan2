library(macpan2)
library(macpan2helpers)
library(oor)
library(ggplot2)
library(dplyr)
# m = Compartmental(file.path("inst", "starter_models", "sir"))
# m$simulators$tmb(100
#   , state = c(S = 99, I = 1)
# )

## factor model bases ---------------

state_sir = basis(
  Epi = c("S", "I", "R", "D"),
  Vital = c("alive", "alive", "alive", "dead"),
  labelling_names = "Epi"
)
flow_rates_sir = basis(Epi = c("lambda", "gamma", "mu"))
trans_rates_sir = basis(Epi = "beta")
cities = basis(Loc = c("cal", "ham", "que"))
movement = mp_linear(cities, "Move")
age = basis(Age = c("young", "old"))
contact = mp_square(age, "Contact")

## product model bases -------------

state = (state_sir
  |> mp_cartesian(cities)
  |> mp_cartesian(age)
)
flow_rates = mp_union(
  (flow_rates_sir
    |> mp_subset(Epi = "lambda")
    |> mp_cartesian(age)
    |> mp_cartesian(cities)
  ),
  (flow_rates_sir
    |> mp_subset(Epi = "mu")
    |> mp_cartesian(age)
  ),
  mp_subset(flow_rates_sir, Epi = "gamma"),
  movement
)
trans_rates = (trans_rates_sir
  |> mp_subset(Epi = "beta")
  |> mp_cartesian(cities)
  |> mp_cartesian(contact)
)

strata = mp_select(state, "Loc.Age")
alive = mp_subset(state, Vital = "alive")
mp_indices(alive$partial_labels("Loc.Age"), strata$labels())
mp_labels(alive, "Loc.Age")
mp_labels(strata)
merge(alive$partition$frame(), strata$partition$frame(), c("Loc", "Age"))


movement_flows = mp_join(
  mp_filter(state, "from", Vital = "alive"),
  mp_filter(state, "to", Vital = "alive"),
  mp_filter(flow_rates, "rate", Epi = ""),
  from.to = "Epi.Age",
  from.rate = "Loc",
  to.rate = "Loc" ~ "Move"
)
infection_flows = mp_join(
  mp_filter(state, "from", Epi = "S"),
  mp_filter(state, "to", Epi = "I"),
  mp_filter(flow_rates, "rate", Epi = "lambda"),
  from.to = "Loc.Age",
  from.rate = "Loc.Age"
)
recovery_flows = mp_join(
  mp_filter(state, "from", Epi = "I"),
  mp_filter(state, "to", Epi = "R"),
  mp_filter(flow_rates, "rate", Epi = "gamma"),
  from.to = "Loc.Age"
)
death_flows = mp_join(
  mp_filter(state, "from", Vital = "alive"),
  mp_filter(state, "to", Vital = "dead"),
  mp_filter(flow_rates, "rate", Epi = "mu"),
  from.to = "Loc.Age",
  from.rate = "Age"
)


flows = rbind(
    movement_flows$labels_frame()
  , infection_flows$labels_frame()
  , recovery_flows$labels_frame()
  , death_flows$labels_frame()
)
influences = mp_join(
  mp_filter(state, "infectious", Epi = "I"),
  mp_filter(flow_rates, "infection", Epi = "lambda"),
  mp_filter(trans_rates, "rate", Epi = "beta"),
  infectious.infection = "Loc",
  infectious.rate = "Age.Loc",
  infection.rate = "Loc.Age" ~ "Loc.Contact"
)$labels_frame()

trans_rate_vector = Vector(trans_rates)
trans_rate_vector$set_numbers(
    Age.Contact = c(young.young = 0.5, old = 0.25)
  , Loc = "ham"
)
trans_rate_vector$frame()




mp_flow_join = function(state, flow_rates, from = list(), to = "", rate = "", from.to = "", from.rate = "", to.rate = "") {
  macpan2:::mp("filter")(state, "from", from)
}
mp_flow_join(state, flow_rates)


vec = list()
vec$state = Vector(state)
vec$state$set_numbers(
  Epi = c(I = 1),
  Loc = c("que", "cal", "ham")
)
vec$state$set_numbers(
  Loc = c(que = 549459, cal = 1306784, ham = 569353),
  Epi = "S"
)

N = mp_select(state, "N", "Loc")
alive = mp_filter(state, "alive", Epi = c("S", "I", "R"))

VectorMerged = function(labeller) {
  self = Base()
  self$labeller = labeller

  self$.numbers = zero_vector(self$labeller$labels())
  self$numbers = function(...) {

  }
  return_object(self, "VectorMerged")
}

vec$N = Vector(N)

N_groups = mp_join(alive, N, alive.N = "Loc")

mp_indices(N_groups$labels_for$N(), N$labels_for$N())
mp_indices(alive$labels_for$alive(), state$labels())



I = mp_filter(state, "I", Epi = "I")



N_groups$labels_for$alive()
state$labels()



#sub_pop$labels_for$alive(), state$labels()


flow_file = CSVReader("inst/model_library/sir_age/flows.csv")$read()
vars_file = CSVReader("inst/model_library/sir_age/variables.csv")$read()
settings_file = JSONReader("inst/model_library/sir_age/settings.json")$read()

make_basis = function(vec_name, vars_table, settings_list, vec_part_field, lab_part_field) {
  vec_labs = to_labels(vars_table[settings_list[[vec_part_field]]])
  basis(
    vars_table[vec_labs == vec_name, , drop = FALSE],
    labelling_names = to_name(settings_list[[lab_part_field]])
  )
}
state = make_basis("state", vars_file, settings_file, "vec_partition", "labelling_partition")
flow_rates = make_basis("flow_rates", vars_file, settings_file, "vec_partition", "labelling_partition")



state$labels()
flow_rates$labels()
tag = function(key, val) list(val) |> setNames(key)
flow_file
i = 1L
flow = flow_file[i,,drop=FALSE]
mp_join(
  mp_filter(state, "from", flow$from_partition),
  mp_filter(state, "to", Epi = flow$to),
  mp_filter(flow_rates, "flow", Epi = flow$flow),
  from.to = "Epi"
)


N = mp_select(state, "N", "Loc")
active = mp_filter(state, "active", Epi = c("S", "I"))
infectious = mp_filter(state, "infectious", Epi = "I")
mp_join(N, infectious, N.infectious = "Loc")

mp_join(xx, yy, N.norm_inf = "Loc")$labels_frame()
mp_join(N, mp_filter(state, "infectious", Epi = "I"))


match(N$labels_frame()$active, state$labels()) - 1L
match(N$labels_frame()$N, unique(N$labels_frame()$N)) - 1L
groupSums(state[active], strata, length(active))

xx = mp_join(
  mp_filter(state, "state"),
  mp_select(state, "N", "Loc"),
  state.N = "Loc"
)
xx$labels_for$state()
xx$labels_for$N()

mp_init_vec = function(components, ...) {
  values = unlist(list(...))
  labs = components$labels()
  vec = setNames(numeric(length(labs)), labs)
  vec[names(values)] = values
  vec
}
mp_init_vec(state, S.tor = 99)
mp_join(
    mp_filter(state, "infectious", Epi = "I"),
    mp_filter(flow_rates, "infection", Epi = "lambda"),
    infectious.infection = "Loc"
)$labels_frame()

mp_union(
  mp_join(
      mp_filter(state, "from")
    , mp_filter(state, "to")
    , mp_filter(flow_rates, "flow", Epi = "")
    , from.to = "Epi"
    , from.flow = "Loc"
    , to.flow = "Loc" ~ "Move"
  ),
  mp_join(
      mp_filter(state, "from", Epi = "S")
    , mp_filter(state, "to", Epi = "I")
    , mp_filter(flow_rates, "flow", Epi = "lambda")
    , from.to = "Loc"
    , from.flow = "Loc"
  )
)

match(ff$from, state$labels()) - 1L
match(ff$to, state$labels()) - 1L
match(ff$flow, flow_rates$labels()) - 1L

merge_generic_by_util(
  from,
  to,
  from.to = "Epi"
) |> merge_generic_by_util(
  flow,
  from.flow = "Loc",
  to.flow = "Loc" ~ "Move"
)

#mp_join = function(table_list, by_list) {
  z = table_list[[1L]]
  for (i in 2:length(table_list)) {
    args = c(
      list(x = z, y = table_list[[i]]),
      by_list
    )
    z = do.call(merge_generic_by_util, args)
  }
  z
}

is_orig_col_nm = function(by) {
  nn = (by
   |> strsplit(":")
   |> vapply(length, integer(1L))
  )
  nn == 1L
}

z = merge_util(
  from,
  to,
  by.x = "Epi",
  by.y = "Epi"
)

fix_column_provenence = function(nms, table_nms) {
  split_nms = nms |> strsplit(":")
  orig_nms = split_nms |> vapply(getElement, character(1L), 1L)
  inv_map = lapply(split_nms, setdiff, orig_nms)
  for (nm in unique(orig_nms)) {
    v = unique(unlist(inv_map[orig_nms == nm], use.names = FALSE))
  }
}

column_map_from_names = function(nms) {
  split_nms = nms |> strsplit(":")
  orig_nms = split_nms |> vapply(getElement, character(1L), 1L)
  inv_map = lapply(split_nms, setdiff, orig_nms)
  table_nms = inv_map |> unlist(use.names = FALSE) |> unique()
  l = list()
  for (nm in table_nms) {
    l[[nm]] = list()
    for (i in seq_along(inv_map)) {
      if (nm %in% inv_map[[i]]) l[[nm]][[orig_nms[[i]]]] = nms[[i]]
    }
  }
  l
}
split_nms |> vapply(function(x) "from" %in% x, logical(1L))
split_nms |> vapply(function(x) "to" %in% x, logical(1L))
names(z) |> strsplit(":") |> lapply(setdiff, original_colnames)

hh = merge_util(
  from,
  to,
  by.x = "Epi",
  by.y = "Epi"
)

ii = merge_util(
  hh,
  flow,
  by.x = c("Loc:from", "Loc:to"),
  by.y = c("Loc", "Move")
)

ii$column_map


prod_state = cartesian(
    sir_state$partition_by_dim$component()
  , loc_state$partition_by_dim$component()
)
prod_flow_rates = union_vars(
  cartesian(sir$flow_rates, loc$state_variables),
  loc_flow_rates
)



state_components = sir_state$partition_by_dim$component()$filter("S", .wrt = "Epi")
state_components$filter("S", .wrt = "Epi")
state_components$.filter
filter_lp = function(partition, x) {
  eval(macpan2:::rhs(x)[[2L]], envir = partition$column_by_dim)
}
filter_lp(loc_flow_rates, ~ flow_rates$Move() == "tor")


x = prod$join3(
  prod$from(),
  prod$to(),
  prod$flow(),
  "Loc",
  Loc:to ~ Move:flow
)
x = x[setdiff(names(x), c("from", "to"))]

l = list(
  from = list(
    Loc = "Loc",
    Epi = "Epi:from"
  ),
  to = list(
    Loc = "Loc",
    Epi = "Epi:to"
  )
)

tt = LabelledPartition(x, l, "Epi.Loc")
tt$labels()
tt$labels_for$from()
tt$labels_for$from()
tt$frame_by_dim$to()
tt$column_by_dim$from$Epi()


sir = Quantities(sir_state, sir_flow_rates, "Epi")
loc = Quantities(loc_state, loc_flow_rates, "Loc.Move")


method_parser = macpan2:::method_parser
method_parser(Move[flow]~Loc)

loc$join(loc$from(), loc$from()) |> loc$join(loc$from()) |> loc$join(loc$from())
loc$join3(loc$flow(), loc$from(), loc$to(), Loc~Loc, Move[flow]~Loc)

sir$join3(
  sir$from("S", filter_by = "Epi"),
  sir$to("I", filter_by = "Epi"),
  sir$flow("lambda", filter_by = "Epi")
)




prod = Quantities(
  cartesian(sir$state_variables, loc$state_variables),
  union_vars(
    cartesian(sir$flow_rates, loc$state_variables),
    loc_flow_rates
  ),
  labelling_descriptors = "Epi.Loc.Move"
)

## learned lots about joining and a bit about filtering:
##
## 1. inner joins are commutative so we can just use from-to-flow order
##    (maybe later we can optimize)
## 2. need to be able to map across partitions when matching (for example:
##    the Move column in a flow rate to the Loc column in a to variable)
## 3. we can have partition matching for all 3 tables (the Loc model is
##    a good example from #2 above). this all just means that the first
##    criterion (e.g. from_to) is used in the first join and both of the
##    two remaining criteria (e.g. from_flow, to_flow) are used in the
##    second join.
## 4. multiple ways to represent partition matching:
##    a) in flows.csv we have a formula for each from_to, from_flow, to_flow.
##       these formulas are things like Epi.Loc ~ Epi.Move.
##    b) in the ... arguments of a 'mechanism' function we need things like
##       Loc[to] ~ Move[flow], Epi[from] ~ Epi[to], ...
##    c) in the by arguments of a non-user-facing wrapper for merge, we need
##       things like Loc:from.Loc:to ~ Loc.Move.  this version is not
##       readible but connects with merge well.
##    d) however, maybe we do not need (c) and instead just need to supply
##       things like by.x = c("Loc:from", "Loc:to"), by.y = c("Loc", "Move").
##       yes i think this is better
##    so we will need to have a generic representation of partition matching
##    that can accept and convert among all of these forms
## 5. when filtering there are a few cases:
##    a) no-op : just return all of the, either, state variables or flow rates
##    b) ... but no filter_by :
##    b) only filter_by :
##    probably need to think more about filtering cases.

list(
    prod$from()
  , prod$to()
  , prod$flow()
  , from_to = Epi ~ Epi
  , from_flow = Loc ~ Loc
  , to_flow = Loc ~ Move
)
from_to = macpan2:::join_partitions(prod$from(), prod$to(), ~Epi)
macpan2:::join_partitions(from_to, prod$flow(), Loc:from.Loc:to ~ Loc.Move)[c("from", "to", "flow")]



prod$flow_mechanism(
  prod$from("S", filter_by = "Epi"),
  prod$to("I", filter_by = "Epi"),
  prod$flow("lambda", filter_by = "Epi"),
  Loc ~ Loc,
  Loc ~ Loc
)



prod$join(
  x = prod$state_variables$frame(),
  y = prod$flow_rates$frame(),
  by = "Loc",
  main = "y"
)


join_partitions(
  from("S", dimensions = prod_state, filter_by = "Epi", label_by = "Epi.Loc"),
  to("I", dimensions = prod_state, filter_by = "Epi", label_by = "Epi.Loc"),
  by = "Loc"
)


x = labelled_frame(prod_state$filter("S", .wrt = "Epi"), "from")
y = labelled_frame(prod_state$filter("I", .wrt = "Epi"), "to")
z = merge(x, y, by = "Loc")
setdiff(intersect(names(x), names(y)), "Loc")


macpan2:::labelled_frame(sir_state, "from")

merge(
  macpan2:::labelled_frame(sir_state$filter("I"), "from"),
  macpan2:::labelled_frame(sir_flow_rates$filter("lambda"), "to"),
  by = character(0L)
)[,c("from", "to"),drop = FALSE]

macpan2:::Connection(
  data.frame(
    from = "S", to = "I", flow = "lambda", type = "per_capita",
    from_filter = "Epi", to_filter = "Epi", flow_filter = "Epi",
    from_to_join = "", from_flow_join = "", to_flow_join = "Null"
  ),
  union_vars(sir_state, sir_flow_rates),
  conn_ref = "flow"
)


map_partitions = function(x, y, x_name, y_name)

cartesian(sir_state$filter("I"), sir_flow$filter("lambda"))
join_partitions(
  sir_state$filter("I"),
  sir_flow_rates$filter("lambda"),
  by = NULL,
  x_suffix = "State",
  y_suffix = "Flow",
  include = "EpiState.EpiFlow"
)


sir_loc_state = cartesian(sir_state, loc_state)
sir_loc_flow = union_vars(
  cartesian(sir_state, loc_flow),
  cartesian(sir_flow, loc_state)
)



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
