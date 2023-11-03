library(macpan2)

## basic model indexes -------------------------

state_sir = mp_index(Epi = c("S", "I", "R"))
flow_rates_sir = mp_index(Epi = c("lambda", "gamma"))
trans_rates_sir = mp_index(Epi = "beta")

## model strata indexes ------------------------

age = mp_index(Age = sprintf("lb%s", seq(from = 0, to = 90, by = 10)))
age_contact = mp_cartesian(
  mp_rename(age, AgeInfectious  = "Age"),
  mp_rename(age, AgeSusceptible = "Age")
)

## structured model --------

state = mp_cartesian(state_sir, age)
flow_rates = mp_union(
  mp_cartesian(
    mp_subset(flow_rates_sir, Epi = "lambda"),
    age
  ),
  mp_subset(flow_rates_sir, Epi = "gamma")
)
trans_rates = mp_cartesian(trans_rates_sir, age_contact)

## subset and grouping indexes -----------------

S      = mp_subset(state, Epi = "S")
I      = mp_subset(state, Epi = "I")
R      = mp_subset(state, Epi = "R")

lambda = mp_subset(flow_rates, Epi = "lambda")
gamma  = mp_subset(flow_rates, Epi = "gamma")

alive  = mp_subset(state, Epi = c("S", "I", "R"))
strata = mp_group(state, "Age")

susceptibility = mp_group(trans_rates, "AgeSusceptible")
contact        = mp_group(trans_rates, "AgeInfectious.AgeSusceptible")
infectivity    = mp_group(trans_rates, "AgeInfectious")

## aggregations, normalizations, summarizations ---------------

### lining up vectors that are involved
aggregation   = mp_join(alive = alive, groups      = strata, by = "Age")
normalization = mp_join(norm  = I    , denominator = strata, by = "Age")

### creating new indexes for some of these vectors
N          = mp_extract(aggregation  , "groups")
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
  infectious = norm_state,
  infection = lambda,
  transmission = trans_rates,
  by = list(
    infectious.transmission = "Age" ~ "AgeInfectious",
    infection.transmission  = "Age" ~ "AgeSusceptible"
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

index_data = list(
    mp_index_data(trans_decomposition)
  , mp_index_data(aggregation)
  , mp_index_data(normalization)
  , mp_index_data(infection, recovery)  ## flows
  , mp_index_data(transmission)
)

## expressions that define model dynamics ---------

## these expressions are very general and should remain robust
## as model structure changes
expr_list = mp_expr_list(
  before = list(
    trans_rates ~ susceptibility[s] * contact[si] * infectivity[i]
  ),
  during = list(
      N ~ groupSums(state[alive], groups, N)
    , norm_state ~ state[norm] / N[denominator]
    , flow_rates[infection] ~ norm_state[infectious] * trans_rates[transmission]
    , flows_per_time ~ state[from] * flow_rates[edge]
    , inflow ~ groupSums(flows_per_time, to, state)
    , outflow ~ groupSums(flows_per_time, from, state)
    , state ~ state + inflow - outflow
  )
)

## instantiate numeric vectors labelled with particular indexes -----------

## TODO: give automatic advice or just automatically initialize
##       vectors required given an analysis of the expression graph
indexed_vecs = list(
  state = mp_vector(state),
  flow_rates = mp_vector(flow_rates),
  trans_rates = mp_vector(trans_rates),
  N = mp_vector(mp_extract(aggregation, "groups")),
  norm_state = mp_vector(mp_extract(normalization, "norm")),
  susceptibility = mp_vector(susceptibility),
  contact = mp_vector(contact),
  infectivity = mp_vector(infectivity)
)