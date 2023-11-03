library(macpan2)

## basic model indexes -------------------------

state = mp_index(Epi = c("S", "I", "R"))
rates = mp_index(Epi = c("lambda", "gamma", "beta"))

## model products (none in this model) --------

## subset and grouping indexes -----------------

### note: this section is more interesting in structured models.
###       in particular these are all single row indexes. also
###       there are no grouping indexes, only subsets.

S      = mp_subset(state, Epi = "S")
I      = mp_subset(state, Epi = "I")
R      = mp_subset(state, Epi = "R")
lambda = mp_subset(rates, Epi = "lambda")
gamma  = mp_subset(rates, Epi = "gamma")
beta   = mp_subset(rates, Epi = "beta")

## links between entries in the indexes -----------

index_data = list(
  flow = mp_index_data(
    infection = mp_join(from = S, to = I, flow = lambda),
    recovery  = mp_join(from = I, to = R, flow = gamma)
  ),
  influences = mp_index_data(
    transmission = mp_join(
      infectious = I,
      infection = lambda,
      transmission = beta
    )
  )
)

## expressions that define model dynamics ---------

expr_list = mp_expr_list(
  during = list(
      rates[infection] ~ state[infectious] * rates[transmission] / N
    , flows_per_time ~ state[from] * rates[flow]
    , inflow ~ groupSums(flows_per_time, to, state)
    , outflow ~ groupSums(flows_per_time, from, state)
    , state ~ state + inflow - outflow
  )
)
