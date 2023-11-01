library(macpan2)

state = mp_index(Epi = c("S", "I", "R"))
rates = mp_index(Epi = c("lambda", "gamma", "beta"))

infection = mp_join(
  from = mp_subset(state, Epi = "S"),
  to   = mp_subset(state, Epi = "I"),
  link = mp_subset(rates, Epi = "lambda")
)

recovery = mp_join(
  from = mp_subset(state, Epi = "I"),
  to   = mp_subset(state, Epi = "S"),
  link = mp_subset(rates, Epi = "gamma")
)

flow = mp_formula_data(infection, recovery)
flow$reference_index_list$from$partial_labels()
infection$labelling_names_list()

mp_flow(
    flow ~ rates[link] * state[from]
  , inflow ~ groupSums(flow, to, state)
  , outflow ~ groupSums(flow, from, state)
  , state ~ state + inflow - outflow
  , formula_data = flow
)

flows = mp_union(infection, recovery)
flows$labels_frame()
flows$labelling_names_list()

l = list(infection, recovery)
lapply(l, getElement, "column_map") |> unique()
lapply(l, getElement, "reference_index_list") |> unique()
lapply(l, getElement, "frame") |> do.call(what = rbind)

mp_flow(
    flow ~ rate * from
  , inflow ~ groupSums(flow, )
  , links =
)


mp_subset(state, Epi = "I")

transmission = mp_join(
  state = mp_subset(state, Epi = "I"),
  flow  = mp_subset(rates, Epi = "lambda"),
  rate  = mp_subset(rates, Epi = "beta")
)

mp_union(infection, recovery, transmission)
