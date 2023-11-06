library(macpan2)

## dynamics ---------------------------------------------

expr_list = mp_expr_list(
  before = list(

      sub_population_sizes =
        N ~ groupSums(state[alive], groups, N)

    , decompose_transmission_rate =
        trans_rates[decomp] ~ infectivity[i] * contact[si] * susceptibility[s]
  ),
  during = list(

      normalize_infectious_states =
        norm_state ~ state[norm] / N[denominator]

    , update_force_of_infection =
        flow_rates[infection_flow] ~
          norm_state[infectious_state] * trans_rates[transmission]

    , update_flows_per_time =
        flows_per_time ~ state[from] * flow_rates[edge]

    , update_inflows_per_time =
        total_inflow ~ groupSums(flows_per_time, to, state)

    , update_outflows_per_time =
        total_outflow ~ groupSums(flows_per_time, from, state)

    , update_state =
        state ~ state + total_inflow - total_outflow
  )
)

## going to add all required vectors to this list as we go through
## each set of expressions in the dynamics above
#vecs = VectorList()
#joins = LinkList()

## basic model indexes -------------------------

state_sir = mp_index(Epi = c("S", "I", "R"))
flow_rates_sir = mp_index(Epi = c("lambda", "gamma"))
trans_rates_sir = mp_index(Epi = "beta")

## model strata indexes ------------------------

age = mp_index(Age = c("young", "old"))
age_contact = mp_cartesian(
  mp_rename(age, AgeInfectious  = "Age"),
  mp_rename(age, AgeSusceptible = "Age")
)

## structured model --------

state = (state_sir
  |> mp_cartesian(age)
)
flow_rates = mp_union(
  mp_cartesian(
    mp_subset(flow_rates_sir, Epi = "lambda"),
    age
  ),
  mp_subset(flow_rates_sir, Epi = "gamma")
)
trans_rates = (trans_rates_sir
  |> mp_cartesian(age_contact)
)


## Vectorizing sub-population size computations -------

#expr_list$before$sub_population_sizes
## N ~ groupSums(state[alive], groups, N)

#N = mp_group(state, "Age")
#state_alive = mp_subset(state, Epi = c("S", "I", "R"))
#joins$add(sub_population_sizes =
#  mp_join(groups = N, alive = state_alive, by = "Age")
#)
#vecs$add(N)
#vecs$add(state)

#oor::method_apply(joins$list, "labels_frame") |> lapply(as.list) |> unlist(use.names = FALSE, recursive = FALSE)
#joins$list$sub_population_sizes

## grouped indices ----------

### stratify by age for normalizing I
N = mp_group(state, "Age")

### decomposing transmission into three components:
#### 1. component that characterizes the susceptible categories
susceptibility = mp_group(trans_rates, "AgeSusceptible")
#### 2. component that characterizes the infectious-susceptible pairs
contact        = mp_group(trans_rates, "AgeInfectious.AgeSusceptible")
#### 3. component that characterizes the infectious categories
infectivity    = mp_group(trans_rates, "AgeInfectious")


## initialize vectors

## subset and grouping indexes -----------------

S      = mp_subset(state, Epi = "S")
I      = mp_subset(state, Epi = "I")
R      = mp_subset(state, Epi = "R")

lambda = mp_subset(flow_rates, Epi = "lambda")
gamma  = mp_subset(flow_rates, Epi = "gamma")


## aggregations, normalizations, summarizations ---------------

### N ~ groupSums(state[alive], groups, N)
### norm_state ~ state[norm] / N[denominator]

alive  = mp_subset(state, Epi = c("S", "I", "R")) ## all states are alive in this model

### lining up vectors that are involved
aggregation   = mp_join(alive = alive, groups      = N, by = "Age")
normalization = mp_join(norm  = I    , denominator = N, by = "Age")

### creating new indexes for some of these vectors
#N = mp_index(Age = c("old", "young"))
#N          = mp_extract(aggregation  , "groups") ## necessary?
norm_state = mp_extract(normalization, "norm")

## decompositions -----------------------------

trans_decomposition = mp_join(
  decomp = trans_rates,
  s  = susceptibility,
  si = contact,
  i  = infectivity,
  by = list(
      decomp.s  = "AgeSusceptible"
    , decomp.si = "AgeInfectious.AgeSusceptible"
    , decomp.i  = "AgeInfectious"
  )
)


## linking states and rates -----------

transmission = mp_join(
  infectious_state = norm_state,
  infection_flow = lambda,
  transmission = trans_rates,
  by = list(
    infectious_state.transmission = "Age" ~ "AgeInfectious",
    infection_flow.transmission  = "Age" ~ "AgeSusceptible"
  )
)
infection = mp_join(
  from = S, to = I, edge = lambda,
  by = list(from.to = "Age", from.edge = "Age")
)
recovery  = mp_join(
  from = I, to = R, edge = gamma,
  by = list(from.to = "Age")
)

## wrapping the results of joins ------------

## wrap up decomposition, aggregation, normalization,
## flows, and influences into a data structure that
## is used by the dynamic model expressions that appear below.

link_data = list(
    decomposition = mp_link_data(trans_decomposition)
  , aggregation = mp_link_data(aggregation)
  , normalization = mp_link_data(normalization)
  , influences = mp_link_data(transmission)
  , flows = mp_link_data(infection, recovery)
)

## expressions that define model dynamics ---------

## these expressions are very general and should remain robust
## as model structure changes

## instantiate numeric vectors labelled with particular indexes -----------

## TODO: give automatic advice or just automatically initialize
##       vectors required given an analysis of the expression graph
init_vecs = list(
  state = mp_vector(state),
  flow_rates = mp_vector(flow_rates),
  trans_rates = mp_vector(trans_rates),
  N = mp_vector(N),
  #N = mp_vector(mp_extract(aggregation, "groups")),
  norm_state = mp_vector(mp_extract(normalization, "norm")),
  susceptibility = mp_vector(susceptibility),
  contact = mp_vector(contact),
  infectivity = mp_vector(infectivity)
)


dynamic_model = DynamicModel(
  expr_list = expr_list,
  link_data = link_data,
  init_vecs = init_vecs,
  unstruc_mats = list()
)
