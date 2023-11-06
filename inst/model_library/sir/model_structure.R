library(macpan2)

## dynamics ---------------------------------------------

expr_list = mp_expr_list(
  before = list(
      N ~ sum(state)
  ),
  during = list(
      flow_rates[infection_flow] ~ state[infectious_state] * trans_rates[transmission] / N
    , flows_per_time ~ state[from] * flow_rates[edge]
    , total_inflow ~ groupSums(flows_per_time, to, state)
    , total_outflow ~ groupSums(flows_per_time, from, state)
    , state ~ state + total_inflow - total_outflow
  )
)

## basic model indexes -------------------------

state = mp_index(Epi = c("S", "I", "R"))
flow_rates = mp_index(Epi = c("lambda", "gamma"))
trans_rates = mp_index(Epi = "beta")

## model products (none in this model) --------

## subset and grouping indexes -----------------

### note: this section is more interesting in structured models.
###       in particular these are all single row indexes. also
###       there are no grouping indexes, only subsets.

S      = mp_subset(state, Epi = "S")
I      = mp_subset(state, Epi = "I")
R      = mp_subset(state, Epi = "R")
lambda = mp_subset(flow_rates, Epi = "lambda")
gamma  = mp_subset(flow_rates, Epi = "gamma")
beta   = mp_subset(trans_rates, Epi = "beta")

## links between entries in the indexes -----------

infection = mp_join(from = S, to = I, edge = lambda)
recovery  = mp_join(from = I, to = R, edge = gamma)

transmission =  mp_join(
  infectious_state = I,
  infection_flow = lambda,
  transmission = beta
)

index_data = list(
    transmission = mp_index_data(transmission)  ## influences
  , update = mp_index_data(infection, recovery)  ## flows
)

vecs = list(
  state = mp_vector(state),
  flow_rates = mp_vector(flow_rates),
  trans_rates = mp_vector(trans_rates)
)
