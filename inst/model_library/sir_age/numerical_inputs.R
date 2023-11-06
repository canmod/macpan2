library(dplyr)
library(tidyr)

## insert non-zero values

init_vecs$state = (init_vecs$state
  |> mp_set_numbers(Epi.Age = c(S.young = 1000, S.old = 3000))
  |> mp_set_numbers(Epi = c(I = 1), Age = "old") ## initial infectious
)
init_vecs$flow_rates = (init_vecs$flow_rates
  |> mp_set_numbers(Epi = c(lambda = NA, gamma = 0.1))
)

## young people are less susceptible
init_vecs$susceptibility = (init_vecs$susceptibility
  |> mp_set_numbers(AgeSusceptible = c(young = 0.1, old = 1))
)

## symmetric contact matrix with 0.6 on the diagonal and 0.4 off
## of the diagonal.
init_vecs$contact = (contact
  |> as.data.frame()
  |> mutate(
    values = if_else(AgeInfectious == AgeSusceptible, 0.6, 0.4)
  )
  |> group_by(AgeInfectious)
  |> mutate(values = values / sum(values))
  |> ungroup()
  |> as.data.frame()
  |> mp_vector(contact)
)

## set infectivity = 1 for both age groups
init_vecs$infectivity = (init_vecs$infectivity
  |> mp_set_numbers(AgeInfectious = c(young = 1, old = 1))
)



## unstructured matrices --------------------------

unstruc_mats = list()  ## none in this model

## number of time steps to run in simulations ------

time_steps = 100L
