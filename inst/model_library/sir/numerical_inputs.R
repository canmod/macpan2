source("model_structure.R")

## numeric vectors labelled with particular indexes -----------

indexed_vecs = list(
  state = mp_vector(
    c(S = 999, I = 1, R = 0),
    state
  ),
  rates = mp_vector(
    c(lambda = NA, gamma = 0.1, beta = 0.25),
    rates
  )
)

## unstructured matrices --------------------------

unstruc_mats = list(N = 1000)

## number of time steps to run in simulations ------

time_steps = 100L
