library(macpan2)

state = mp_index(Epi = c("S", "I", "R"))
rate = mp_index(Epi = c("beta", "gamma", "lambda"))

# infection ledger
infection = mp_join(
  from_states = mp_subset(state, Epi = "S"),
  to_states = mp_subset(state, Epi = "I"),
  flow_rates = mp_subset(rate, Epi = "lambda")
)

# recovery ledger
recovery  = mp_join(
  from_states = mp_subset(state, Epi = "I"),
  to_states = mp_subset(state, Epi = "R"),
  flow_rates = mp_subset(rate, Epi = "gamma")
)

# unite into a single flows ledger
mp_union(infection, recovery)
