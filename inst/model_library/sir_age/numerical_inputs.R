library(dplyr)
library(tidyr)

## insert non-zero values

indexed_vecs$state = (indexed_vecs$state
  |> mp_set_numbers(Epi = c(S = 1000)) ## ridiculous even age distribution
  |> mp_set_numbers(Epi = c(I = 1), Age = "lb90") ## initial infectious 90-year-old
)
indexed_vecs$flow_rates = (indexed_vecs$flow_rates
  |> mp_set_numbers(Epi = c(lambda = NA, gamma = 0.1))
)
indexed_vecs$susceptibility = (susceptibility
  |> as.data.frame()
  |> mutate(values = seq(from = 0.01, by = 0.1, length.out = n()))
  |> mp_vector(susceptibility)
)
indexed_vecs$contact = (contact
  |> as.data.frame()
  |> mutate(
    i = as.numeric(sub("^lb", "", AgeInfectious)),
    s = as.numeric(sub("^lb", "", AgeSusceptible)),
    values = exp(-((i - s)/20)^2)  ## ridiculous kernel
  )
  |> group_by(AgeInfectious)
  |> mutate(values = values / sum(values))
  |> ungroup()
  |> select(-i, -s)
  |> as.data.frame()
  |> mp_vector(contact)
)
indexed_vecs$infectivity = (infectivity
  |> as.data.frame()
  |> mutate(values = seq(from = 0.01, by = 0.1, length.out = n()))
  |> mp_vector(infectivity)
)

## unstructured matrices --------------------------

unstruc_mats = list()

## number of time steps to run in simulations ------

time_steps = 100L
