library(macpan2)
library(ggplot2)
library(dplyr)
library(tidyr)
## NOT DONE

state_index = mp_index(
    Epi  = c("S", "I"   , "I"     , "R")
  , Symp = c("" , "mild", "severe", "" )
)
flow_rate_index = mp_cartesian(
    mp_index(Epi = c("infection", "recovery"))
  , mp_index(Symp = c("mild", "severe"))
)
state_labels     = state_index                               |> mp_labels()
flow_rate_labels = flow_rate_index                           |> mp_labels()
I_labels         = state_index     |> mp_lookup("I")         |> mp_labels()
infection_labels = flow_rate_index |> mp_lookup("infection") |> mp_labels()

flow = data.frame(
    rate = c("infection.mild", "infection.severe", "recovery.mild", "recovery.severe")
  , from = c("S."            , "S."              , "I.mild"       , "I.severe")
  , to   = c("I.mild"        , "I.severe"        , "R."           , "R.")
)

initialize_state = list(
    state[S.    ] ~ N - 1
  , state[I.mild] ~ 1
)

pre_computations = list(
    I_effective ~ sum(infectivity * state[I]) / N
)

force_of_infection = list(
    per_capita[infection] ~ beta * I_effective
)

## euler
update_state = list(
    flow_rates[rate] ~ per_capita[rate] * state[from]
  , outflow ~ group_sums(flow_rates[rate] , from , state) 
  , inflow  ~ group_sums(flow_rates[rate] , to   , state)
  , state ~ state - outflow + inflow
)

## hazard
update_state = list(
    outflow_per_capita ~ group_sums(per_capita[rate], from, state)
  , outflow ~ state * (1 - exp(-outflow_per_capita))
  , flow_rates ~ outflow[from] * per_capita[rate] / outflow_per_capita[from]
  , inflow ~ group_sums(flow_rates[rate], to, state)
  , state ~ state - outflow + inflow
)


spec = mp_tmb_model_spec(
    before = initialize_state
  , during = c(pre_computations, force_of_infection, update_state)
  , default = list(
        N = 100
      , beta = c(mild = 0.5, severe = 0.2)
      , infectivity = c(mild = 1, severe = 1.2)
      , state = mp_zero_vector(state_index)
      , flow_rates = mp_zero_vector(flow_rate_labels)
      , per_capita = c(
            infection.mild   = NA, recovery.mild   = 0.3
          , infection.severe = NA, recovery.severe = 0.05
        )
    )
  , integers = list(
        from      = mp_indices(flow$from        , state_labels)
      , to        = mp_indices(flow$to          , state_labels)
      , rate      = mp_indices(flow$rate        , flow_rate_labels)
      , I         = mp_indices(I_labels         , state_labels)
      , infection = mp_indices(infection_labels , flow_rate_labels)
  )
)

(spec
  |> mp_simulator(50L, I_labels)
  |> mp_trajectory()
  |> rename(Prevalence = value)
  |> separate_wider_delim(row, '.', names = c("Epi", "Symptom Status"))
  |> ggplot()
  + geom_line(aes(time, Prevalence, colour = `Symptom Status`))
)
