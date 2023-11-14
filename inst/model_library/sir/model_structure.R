library(macpan2)

## dynamics ---------------------------------------------

expr_list = mp_expr_list(
  before = list(
    ## aggregations
      N ~ sum(state)
  ),
  during = list( ## this is a for loop over time steps
    ## influences
      flow_rates[infection_flow] ~
        state[infectious_state] * trans_rates[transmission] / N

    ## flows
    , flows_per_time ~ state[from] * flow_rates[edge]

    ## state update
    , total_inflow ~ groupSums(flows_per_time, to, state)
    , total_outflow ~ groupSums(flows_per_time, from, state)
    , state ~ state + total_inflow - total_outflow
  )
)

data.frame(
  flow = runif(10),
  from = rep(1:4, times = 1:4)
)

data.frame(
  flow = runif(10),
  from = rep(1:2, 5)
)

## basic model indexes -------------------------

state = mp_index(Epi = c("S", "I", "R"))
flow_rates = mp_index(Epi = c("lambda", "gamma"))
trans_rates = mp_index(Epi = "beta")

## subset indexes -----------------

S      = mp_subset(state, Epi = "S")
I      = mp_subset(state, Epi = "I")
R      = mp_subset(state, Epi = "R")
lambda = mp_subset(flow_rates, Epi = "lambda")
gamma  = mp_subset(flow_rates, Epi = "gamma")
beta   = mp_subset(trans_rates, Epi = "beta")

## links between entries in the indexes -----------

# flows_per_time ~ state[from] * flow_rates[edge]
infection = mp_join(from = S, to = I, edge = lambda)
recovery  = mp_join(from = I, to = R, edge = gamma)


# flow_rates[infection_flow] ~
#         state[infectious_state] * trans_rates[transmission] / N
transmission =  mp_join(
  infectious_state = I,
  infection_flow = lambda,
  transmission = beta
)

ledgers = list(
    influences = mp_ledgers(transmission)
  , flows = mp_ledgers(infection, recovery)
)

## Initialize indexed vectors (to all zeros) --------------

init_vecs = list(
  state = mp_vector(state),
  flow_rates = mp_vector(flow_rates),
  trans_rates = mp_vector(trans_rates)
)

## Final output -----------------

dynamic_model = mp_dynamic_model(
  expr_list = expr_list,
  ledgers = ledgers,
  init_vecs = init_vecs,
  unstruc_mats = list()
)
