## numeric vectors labelled with particular indexes -----------

vecs$state = mp_set_numbers(vecs$state
  , Epi = c(S = 999, I = 1, R = 0)
)
vecs$flow_rates = mp_set_numbers(vecs$flow_rates
  , Epi = c(lambda = NA, gamma = 0.1)
)
vecs$trans_rates = mp_set_numbers(vecs$trans_rates
  , Epi = c(beta = 0.25)
)

## unstructured matrices --------------------------

unstruc_mats = list()

## number of time steps to run in simulations ------

time_steps = 100L
