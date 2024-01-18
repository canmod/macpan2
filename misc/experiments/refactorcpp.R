library(macpan2)
library(macpan2helpers)
library(oor)
library(ggplot2)
library(dplyr)

sir = mp_index(Epi = c("S", "I", "R"))
sir_two_strains = (mp_rename(sir, A = "Epi")
  |> mp_cartesian(mp_rename(sir, B = "Epi"))
  |> mp_cartesian(mp_rename(sir, C = "Epi"))
  |> mp_setdiff(A.B = "I.I", A.C = "I.I", B.C = "I.I")
)


gg = mp_aggregate(sir_two_strains, by = "A", ledger_column = "a_strain")
gg$positions_for$a_strain()


## replacement model

n_strains = 2L
strain_nms = letters[seq_len(n_strains + 1L)]

strains = mp_index(
  Strain = strain_nms[-(n_strains + 1L)],
  Replace = strain_nms[-1L],
  labelling_column_names = "Strain"
)

replacement_state = mp_union(

  ## naive part
  mp_index(Epi = "S", Strain = "", Replace = "a"
    , labelling_column_names = "Epi.Strain"
  ),

  ## non-naive part
  mp_cartesian(
    mp_index(Epi = c("I", "E", "R")),
    strains
  )
)
infection = mp_join(
  from = mp_subset(replacement_state, Epi = c("S", "R")),
  to = mp_subset(replacement_state, Epi = "E"),
  by = list(from.to = "Replace" ~ "Strain")
)
progression = mp_join(
  from = mp_subset(replacement_state, Epi = "E"),
  to = mp_subset(replacement_state, Epi = "I"),
  by = list(from.to = "Strain")
)


xx = macpan2:::LinkData(
  infection = mp_join(
    from = mp_subset(replacement_state, Epi = c("S", "R")),
    to = mp_subset(replacement_state, Epi = "E"),
    by = list(from.to = "Replace" ~ "Strain")
  ),
  progression = mp_join(
    from = mp_subset(replacement_state, Epi = "E"),
    to = mp_subset(replacement_state, Epi = "I"),
    by = list(from.to = "Strain")
  )
)


replacement_infection$labelling_column_names_list
replacement_progression$labelling_column_names_list

replacement_infection$column_map
replacement_progression$column_map

macpan2:::bind_rows(
  replacement_infection$frame,
  replacement_progression$frame
)


replacement_progression$frame


mp_union(replacement_infection, replacement_progression)

replacement_progression$reference_positions_for$from()

mp_vector(c(S. = 1000, I.a = 1), replacement_state)


replacement_state$labels()[replacement_progression$positions_for$from()]
initial_replacement_state = data.frame(
  Epi    = c("S",  "I"),
  Strain = c("",  "a"),
  values = c(1000,  1 )
)
state_vector = mp_vector(initial_replacement_state, replacement_state)
state_vector
state_vector$frame()
state_vector$csv("state-test-write.csv")


xx = mp_vector(replacement_state)
xx$set_numbers(Strain = "a", Epi = c(I = 1, E = 1))


mp_subset(replacement_state, Epi = "E")$position()
mp_subset(replacement_state, Epi = "E")$reference_position()

xx = mp_group(replacement_state, "Strain")
xx$reference_labels()
xx$labels()

## atomic model indices ---------------

state = mp_index(
  Epi = c("S", "I", "R")
  #Vital = c("alive", "alive", "alive", "dead"),
  #labelling_column_names = "Epi"
)
flow_rates_sir = mp_index(Epi = c("lambda", "gamma", "mu"))
trans_rates_sir = mp_index(Epi = "beta")
cities = mp_index(Loc = c("cal", "ham", "que"))

movement = mp_linear(cities, "Move")
mp_square(cities, c("Loc", "Move"))

age = mp_index(Age = c("young", "old"))
contact = mp_square(age, c("Infectious", "Infection"))


library(macpan2)

sir = mp_index(Epi = c("S", "I", "R", "lambda", "gamma"))
strain = mp_index(Strain = c("a", "b"))
state = mp_union(
  mp_cartesian(mp_subset(sir, Epi = "I"), strain),
  mp_subset(sir, Epi = c("S", "R"))
)

flow_rates = mp_union(
  mp_cartesian(mp_subset(sir, Epi = "lambda"), strain),
  mp_subset(sir, Epi = "gamma")
)
trans_rates = mp_cartesian(
  mp_index(Epi = "beta"),
  strain
)

state
strain
flow_rates
trans_rates

ll = mp_join(
  state = mp_subset(state, Epi = "I"),
  flow = mp_subset(flow_rates, Epi = "lambda"),
  trans = trans_rates,
  by = list(
    state.flow = "Strain",
    flow.trans = "Strain"
  )
)
list(group = list())
hh = function(index, by = "Group", ledger_column = "group") {
  index_columns = to_names(by)
  if (length(index_columns) == 1L & !index_columns %in% names(index)) {
    partition = index$partition$constant(by, "a")
    index = Index(partition)
  } else {
    partition = index$partition
  }
  Link(
    partition$frame(),
    macpan2:::initial_column_map(names(partition), ledger_column),
    macpan2:::initial_reference_index_list(index, ledger_column),
    setNames(list(index_columns), ledger_column)
  )
}
hh(movement, by = "Move", ledger_column = "move")
hh(movement)
mp_ledgers(hh(state))$positions_frame(TRUE)
hh = function(len) {
  data.frame(group = rep("group", len))

}
Link(
  ,
  column_map = macpan2:::initial_column_map()
)

mp_join(
  state = mp_subset(state, Epi = "I"),
  rate = trans_rates,
  by = list(
    state.rate = "Strain"
  )
)

mp_join(
  flow = mp_subset(flow_rates, Epi = "lambda"),
  rate = trans_rates,
  by = list(
    flow.rate = "Strain"
  )
)




## structured model indices -------------
state_sir = mp_index(Epi = c("S", "I", "R"))
state = (state_sir
  |> mp_cartesian(cities)
  |> mp_cartesian(age)
)
flow_rates = mp_union(
  (flow_rates_sir
    |> mp_subset(Epi = "lambda")
    |> mp_cartesian(cities)
    |> mp_cartesian(age)
  ),
  (flow_rates_sir
    |> mp_subset(Epi = "mu")
    |> mp_cartesian(age)
  ),
  mp_subset(flow_rates_sir, Epi = "gamma"),
  movement
) |> mp_group("Epi.Age.Loc.Move")
trans_rates = (trans_rates_sir
  |> mp_cartesian(cities)
  |> mp_cartesian(contact)
)

## index groups and subsets of indexes ------------

strata = mp_group(state, by = "Loc.Age")
alive = mp_subset(state, Epi = c("S", "I", "R"))
infectious = mp_subset(state, Epi = "I")
beta = mp_subset(trans_rates, Epi = "beta")
lambda = mp_subset(flow_rates, Epi = "lambda")

## create vectors for particular indexes -----------

state_vector = mp_vector(state)
state_vector$
  set_numbers(Epi = c(I = 1))$
  set_numbers(Epi.Loc.Age = c(S.cal.young = 1000))
N = mp_vector(strata)
flow_rate_vector = mp_vector(flow_rates)
flow_rate_vector$set_numbers(Epi = c(lambda = 0.15, gamma = 0.1, mu = 0.01))

## links between entries in the indexes -----------

movement_flows = mp_join(
  from = mp_subset(state, Epi = c("S", "I", "R")),
  to = mp_subset(state, Epi = c("S", "I","R")),
  link = mp_subset(flow_rates, Epi = ""),
  by = list(
    from.to = "Epi.Age",
    from.link = "Loc",
    to.link = "Loc" ~ "Move"
  )
)
infection_flows = mp_join(
  from = mp_subset(state, Epi = "S"),
  to = mp_subset(state, Epi = "I"),
  link = mp_subset(flow_rates, Epi = "lambda"),
  by = list(
    from.to = "Loc.Age",
    from.link = "Loc.Age"
  )
)
recovery_flows = mp_join(
  from = mp_subset(state, Epi = "I"),
  to = mp_subset(state, Epi = "R"),
  link = mp_subset(flow_rates, Epi = "gamma"),
  by = list(from.to = "Loc.Age")
)
death_flows = mp_join(
  from = mp_subset(state, Vital = "alive"),
  to = mp_subset(state, Vital = "dead"),
  link = mp_subset(flow_rates, Epi = "mu"),
  by = list(
    from.to = "Loc.Age",
    from.link = "Age"
  )
)

flows = mp_ledgers(
    movement_flows
  , infection_flows
  , recovery_flows
  , death_flows
)

vv = macpan2:::IndexedExpressions(
    flows_per_time ~ state[from] * flow_rates[link]
  , inflow ~ group_sums(flows_per_time, to, state)
  , outflow ~ group_sums(flows_per_time, from, state)
  , state ~ state + inflow - outflow
  , index_data = flows
  , vector_list = list(
      state = state_vector
    , flow_rates = flow_rate_vector
  )
)
vv$mats_list()
vv$int_vecs(TRUE)
filter(vv$simulate(20), matrix == "state", row == "S.cal.young")


state_aggregation = mp_join(
  alive = alive,
  group_by = strata,
  by = list(alive.group_by = "Loc.Age")
) |> mp_ledgers()
## N ~ group_sums(state[alive], group_by, N)
state_vector = Vector(state)
state_vector$set_numbers(Epi = c(I = 1))$set_numbers(Epi.Loc.Age = c(S.cal.young = 1000))
hh = macpan2:::IndexedExpressions(
    N ~ group_sums(state[alive], group_by, N)
  , index_data = state_aggregation
  , vector_list = list(
      N = Vector(strata)
    , state = state_vector
  )
)
filter(hh$simulate(10), matrix == "N")
state_normalization = mp_join(
  infectious = infectious,
  strata = strata,
  by = list(infectious.strata = "Loc.Age")
)
## Inorm ~ state[infectious] / N[strata]

transmission = mp_join(
  infectious = infectious,
  infection = lambda,
  transmission = beta,

)





## performance sanity check ---------
if (FALSE) {
  cities = mp_index(Loc = letters)
  state_sir = mp_cartesian(
    mp_index(EpiA = rep(LETTERS)),
    mp_index(EpiB = rep(LETTERS))
  ) |> mp_cartesian(cities)
  flow_rates = mp_cartesian(
    mp_index(EpiA = paste(letters[1:25], letters[2:26], sep = "")),
    mp_index(EpiB = paste(letters[1:25], letters[2:26], sep = ""))
  ) |> mp_cartesian(cities)
  flow = mp_join(
    from = mp_subset(state_sir, EpiA = "A"),
    to = mp_subset(state_sir, EpiA = "B"),
    rate = mp_subset(flow_rates, EpiA = "gh"),
    by = list(
      from.to = "Loc",
      from.rate = "Loc",
      to.rate = "Loc"
    )
  )
  flow
}



self$reference_index_list$N
self$frame
self$reference_index_list$N$labels()
group_sums()
mp_labels
xx$reference_index_list$N$labelling_column_names
yy = mp_ledgers(xx)
yy$reference_index_list

yy = Vector(state)
yy$numbers()
yy$set_numbers(Loc = c(cal = 1000, ham = 2000, que = 3000), Epi = "S")
yy$set_numbers(Epi = c(I = 1), Loc.Age = "ham.young")

infection_flows = mp_join(
    to = mp_subset(state, Epi = "S")
  , from = mp_subset(state, Epi = "I")
  , rate = mp_subset(flow_rates, Epi = "lambda")
  , to.rate = "Age.Loc"
  , rate.from = "Age.Loc"
  , from.to = "Age.Loc"
)

influence = mp_join(
    state = mp_subset(state, Epi = "I")
  , flow = mp_subset(flow_rates, Epi = "lambda")
  , trans = mp_subset(trans_rates, Epi = "beta")

    ## can't infect someone at a different location
  , state.flow = "Loc"

    ## location and age specific transmission rates
    ## for each infectious state
  , state.trans = "Loc.Age" ~ "Loc.AgeInfectious"

    ## location and age specific transmission rates
    ## for each infection flow
  , flow.trans = "Loc.Age" ~ "Loc.AgeInfection"
)

# mp_aggregate = function(
#     formula, index
#   )

# macpan2:::mp_expr_group_sum(state
#   , stratify_by = "Loc.Age"
#   , output_name = "N"
#   , vector_name = "state"
#   , subset_name = "alive"
#   , grouping_name = sprintf("%s_groups", output_name)
#   , length_name = sprintf("%s_length", output_name)
#   , Vital = "alive"
# )

mp_aggregate(N ~ group_sums(state)
  , group_by = "Loc.Age"
  , index = state
  , Vital = "alive"
)




trans_decomp = macpan2:::mp_decompose(
    beta ~ contact * infectivity * susceptibility
  , index = mp_subset(trans_rates, Epi = "beta")
  , decomp_name = "trans"

  ## define dimensions over-which each component of the
  ## decomposition will vary. these all must be dimensions
  ## in the index.
  , contact = "Loc.AgeInfectious.AgeInfection"
  , infectivity = "Loc"
  , susceptibility = "AgeInfection"
)

trans_decomp
trans_decomp$formula
trans_decomp$input_formula
macpan2:::update_formula(g ~ y * x * z, g ~ friend)



hh = function(e) {
  e = substitute(e)
  eval(e, list(x = as.symbol("lkjg"), y = as.symbol("LKJDSF")))
}
hh(x + y)

x = function(e, ...) {
  self = new.env(parent = emptyenv())
  self$`*` = function(...) paste(..., sep = " * ")
  self$e
}
y = x(A * B * C, A[friend], "A")
y$e

trans_decomp$formula
trans_decomp$int_vecs
trans_decomp$labels_for$beta()
trans_decomp$labels_for$infectivity()

mp_positions(
  trans_decomp$labels_for$susceptibility(),
  trans_decomp$partition_for$susceptibility()$labels()
)

N_expr = macpan2:::mp_expr_group_sum(state
  , "Loc.Age"
  , "N", "state", "alive", "alive_groups", "length_N"
  , Vital = "alive"
)


xx = mp_join(
  mp_subset(N_expr$strata, "N"),
  mp_subset(state, "infectious", Epi = "I"),
  N.infectious = N_expr$strata$labelling_column_names
)
xx$labels_for$infectious()
xx$frame

state_strata = mp_group(state, "Loc.Age")
alive_states = mp_subset(state, Vital = "alive")
alive_groups = mp_positions(
  mp_labels(alive_states, "Loc.Age"),
  mp_labels(state_strata)
)
alive = mp_positions(
  mp_labels(alive_states),
  mp_labels(state)
)
N_vector = Vector(state_strata)
state_vector = Vector(state)
state_vector$set_numbers(Epi = c(I = 1), Loc.Age = "ham.young")
S_numbers = mp_zero_vector(state, "Loc.Age", Epi = "S")
S_numbers[] = round(abs(rnorm(length(S_numbers), 1000, 1000)))
state_vector$set_numbers(Loc.Age = S_numbers, Epi = "S")

alive_groups
alive
engine_eval(
  ~ group_sums(state[alive], alive_groups, n_strata),
  state = state_vector$numbers(),
  alive = alive,
  alive_groups = alive_groups,
  n_strata = length(N_vector)
)




flows = rbind(
    movement_flows$labels_frame()
  , infection_flows$labels_frame()
  , recovery_flows$labels_frame()
  , death_flows$labels_frame()
)
influences = mp_join(
  mp_subset(state, "infectious", Epi = "I"),
  mp_subset(flow_rates, "infection", Epi = "lambda"),
  mp_subset(trans_rates, "rate", Epi = "beta"),
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

N = mp_group(state, "N", "Loc")
alive = mp_subset(state, "alive", Epi = c("S", "I", "R"))

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

mp_positions(N_groups$labels_for$N(), N$labels_for$N())
mp_positions(alive$labels_for$alive(), state$labels())



I = mp_subset(state, "I", Epi = "I")



N_groups$labels_for$alive()
state$labels()



#sub_pop$labels_for$alive(), state$labels()


flow_file = CSVReader("inst/model_library/sir_age/flows.csv")$read()
vars_file = CSVReader("inst/model_library/sir_age/variables.csv")$read()
settings_file = JSONReader("inst/model_library/sir_age/settings.json")$read()

make_mp_index = function(vec_name, vars_table, settings_list, vec_part_field, lab_part_field) {
  vec_labs = to_labels(vars_table[settings_list[[vec_part_field]]])
  mp_index(
    vars_table[vec_labs == vec_name, , drop = FALSE],
    labelling_column_names = to_name(settings_list[[lab_part_field]])
  )
}
state = make_mp_index("state", vars_file, settings_file, "vec_partition", "labelling_partition")
flow_rates = make_mp_index("flow_rates", vars_file, settings_file, "vec_partition", "labelling_partition")



state$labels()
flow_rates$labels()
tag = function(key, val) list(val) |> setNames(key)
flow_file
i = 1L
flow = flow_file[i,,drop=FALSE]
mp_join(
  mp_subset(state, "from", flow$from_partition),
  mp_subset(state, "to", Epi = flow$to),
  mp_subset(flow_rates, "flow", Epi = flow$flow),
  from.to = "Epi"
)


N = mp_group(state, "N", "Loc")
active = mp_subset(state, "active", Epi = c("S", "I"))
infectious = mp_subset(state, "infectious", Epi = "I")
mp_join(N, infectious, N.infectious = "Loc")

mp_join(xx, yy, N.norm_inf = "Loc")$labels_frame()
mp_join(N, mp_subset(state, "infectious", Epi = "I"))


match(N$labels_frame()$active, state$labels()) - 1L
match(N$labels_frame()$N, unique(N$labels_frame()$N)) - 1L
group_sums(state[active], strata, length(active))

xx = mp_join(
  mp_subset(state, "state"),
  mp_group(state, "N", "Loc"),
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
    mp_subset(state, "infectious", Epi = "I"),
    mp_subset(flow_rates, "infection", Epi = "lambda"),
    infectious.infection = "Loc"
)$labels_frame()

mp_union(
  mp_join(
      mp_subset(state, "from")
    , mp_subset(state, "to")
    , mp_subset(flow_rates, "flow", Epi = "")
    , from.to = "Epi"
    , from.flow = "Loc"
    , to.flow = "Loc" ~ "Move"
  ),
  mp_join(
      mp_subset(state, "from", Epi = "S")
    , mp_subset(state, "to", Epi = "I")
    , mp_subset(flow_rates, "flow", Epi = "lambda")
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
  labelling_index = "Epi.Loc.Move"
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
engine_eval(~ group_sums(x, i, 2), x = 1:6, i = c(0,0,0,1,1,1))
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

iv = macpan2:::IntVecs(
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
          group_sums(per_capita, per_capita_to, state_length) +
          group_sums(absolute, absolute_to, state_length) +
          group_sums(per_capita_inflow, per_capita_inflow_to, state_length) +
          group_sums(absolute_inflow, absolute_inflow_to, state_length)
      , total_outflow ~
          group_sums(per_capita, per_capita_from, state_length) +
          group_sums(absolute, absolute_from, state_length) +
          group_sums(per_capita_inflow, per_capita_outflow_from, state_length) +
          group_sums(absolute_inflow, absolute_outflow_from, state_length)
      , state ~ state + total_inflow - total_outflow
    )
  ),
  engine_methods = macpan2:::EngineMethods(int_vecs = iv),
  time_steps = macpan2:::Time(100L),
  params = macpan2:::OptParamsList(0.5, 0.2, 0.1
    , par_id = 0:2
    , mat = c("trans", "flow", "flow")
    , row_id = 0:2
    , col_id = rep(0L, 3L)
  ),
  obj_fn = macpan2:::ObjectiveFunction(~ sum(state^2))
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

m = TMBModel(time_steps = macpan2:::Time(100))
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
  engine_methods = macpan2:::EngineMethods(
    exprs = list(
        set_infection_flow = flow[infection] ~ per_capita_transmission %*% state[infectious]
      , get_per_capita = ~ state[per_capita_from] * flow[per_capita_flow]
      , get_absolute = ~ flow[absolute_flow]
      , get_per_capita_inflow = ~ state[per_capita_inflow_from] * flow[per_capita_inflow_flow]
      , get_per_capita_outflow = ~ state[per_capita_outflow_from] * flow[per_capita_outflow_flow]
      , get_absolute_inflow = ~ flow[absolute_inflow_flow]
      , get_absolute_outflow = ~ flow[absolute_outflow_flow]
      , get_per_capita_state_in = ~ group_sums(per_capita, per_capita_to, state_length)
      , get_absolute_state_in = ~ group_sums(absolute, absolute_to, state_length)
      , get_per_capita_inflow_state_in = ~ group_sums(per_capita_inflow, per_capita_inflow_to, state_length)
      , get_absolute_inflow_state_in = ~ group_sums(absolute_inflow, absolute_inflow_to, state_length)
      , get_per_capita_state_out = ~ group_sums(per_capita, per_capita_from, state_length)
      , get_absolute_state_out = ~ group_sums(absolute, absolute_from, state_length)
      , get_per_capita_outflow_state_out = ~ group_sums(per_capita_outflow, per_capita_outflow_from, state_length)
      , get_absolute_outflow_state_out = ~ group_sums(absolute_outflow, absolute_outflow_from, state_length)
    ),
    int_vecs = macpan2:::IntVecs(
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
  time_steps = macpan2:::Time(100L)
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
  time = macpan2:::Time(150),
  engine_methods = macpan2:::EngineMethods(
    exprs = list(
        from_states = ~ state[from_indices]
      , infectious_states = ~ state[infectious_indices]
      , update_infection_flows = flow[infection_indices] ~
          per_capita_transmission_matrix %*% state[infectious_indices]
      , inflow = ~ group_sums(per_capita, to_indices, state_length)
      , outflow = ~ group_sums(per_capita, from_indices, state_length)
      , beta = ~ time_var(beta_ts, beta_cp, beta_n, beta_group)
    ),
    int_vecs = macpan2:::IntVecs(
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
