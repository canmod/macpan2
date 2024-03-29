library(macpan2)

## dynamics ---------------------------------------------

expr_list = mp_tmb_expr_list(
  before = list(

      sub_population_sizes =
        N ~ group_sums(state[alive], groups, N)

    , decompose_transmission_rate =
        trans_rates[decomp] ~ infectivity[i] * contact[si] * susceptibility[s]
  ),
  during = list(

    normalize_infectious_states =
        norm_state ~ state[norm] / N[denominator]

    , update_force_of_infection =
        flow_rates[infection_flow] ~
          norm_state[infectious_state] * trans_rates[transmission]

    , update_flows_per_time =
        flows_per_time ~ state[from] * flow_rates[edge]

    , update_inflows_per_time =
        total_inflow ~ group_sums(flows_per_time, to, state)

    , update_outflows_per_time =
        total_outflow ~ group_sums(flows_per_time, from, state)

    , update_state =
        state ~ state + total_inflow - total_outflow
  )
)

## basic model indexes -------------------------

state_sir = mp_index(Epi = c("S", "I", "R"))
flow_rates_sir = mp_index(Epi = c("lambda", "gamma"))
trans_rates_sir = mp_index(Epi = "beta")

## model strata indexes ------------------------

age = mp_index(Age = c("young", "old"))
age_contact = mp_cartesian(
  mp_rename(age, AgeInfectious  = "Age"),
  mp_rename(age, AgeSusceptible = "Age")
)



trans_rates_sir = mp_index(Epi = "beta")
symptoms = mp_index(Symp = c("mild", "severe"))
trans_rates = mp_cartesian(
  trans_rates_sir,
  mp_cartesian(
    mp_rename(symptoms, SympInfectious  = "Symp"),
    mp_rename(symptoms, SympSusceptible = "Symp")
  )
)

## structured model --------

state = (state_sir
  |> mp_cartesian(age)
)
flow_rates = mp_union(
  mp_cartesian(
    mp_subset(flow_rates_sir, Epi = "lambda"),
    age
  ),
  mp_subset(flow_rates_sir, Epi = "gamma")
)
trans_rates = (trans_rates_sir
  |> mp_cartesian(age_contact)
)

## decomposing transmission ----------

### decomposing transmission into three components:
#### 1. component that characterizes the susceptible categories
susceptibility = mp_group(trans_rates, "AgeSusceptible")
#### 2. component that characterizes the infectious-susceptible pairs
contact        = mp_group(trans_rates, "AgeInfectious.AgeSusceptible")
#### 3. component that characterizes the infectious categories
infectivity    = mp_group(trans_rates, "AgeInfectious")

trans_decomposition = mp_join(
  decomp = trans_rates,
  s  = susceptibility,
  si = contact,
  i  = infectivity,
  by = list(
      decomp.s  = "AgeSusceptible"
    , decomp.si = "AgeInfectious.AgeSusceptible"
    , decomp.i  = "AgeInfectious"
  )
)


## subset indexes -----------------

S = mp_subset(state, Epi = "S")
I = mp_subset(state, Epi = "I")
R = mp_subset(state, Epi = "R")

lambda = mp_subset(flow_rates, Epi = "lambda")
gamma  = mp_subset(flow_rates, Epi = "gamma")

## aggregating states ---------------

### N ~ group_sums(state[alive], groups, N)
aggregation = mp_join(
    alive = mp_subset(state, Epi = c("S", "I", "R")) ## all states are alive in this model
  , groups = mp_group(state, "Age")
  , by = "Age"
)
N = mp_extract(aggregation, "groups")

## normalizing states -----------

### norm_state ~ state[norm] / N[denominator]
normalization = mp_join(norm = I, denominator = N, by = "Age")


## influences (i.e. states directly influencing flow rates) -----------

### update_force_of_infection =
###         flow_rates[infection_flow] ~
###           norm_state[infectious_state] * trans_rates[transmission]
norm_state = mp_extract(normalization, "norm")
transmission = mp_join(
  infectious_state = norm_state,
  infection_flow = lambda,
  transmission = trans_rates,
  by = list(
    infectious_state.transmission = "Age" ~ "AgeInfectious",
    infection_flow.transmission  = "Age" ~ "AgeSusceptible"
  )
)


## flows (i.e. movement from one state to another) ---------

### update_flows_per_time =
###     flows_per_time ~ state[from] * flow_rates[edge]
### update_inflows_per_time =
###     total_inflow ~ group_sums(flows_per_time, to, state)
### update_outflows_per_time =
###     total_outflow ~ group_sums(flows_per_time, from, state)
### update_state =
###     state ~ state + total_inflow - total_outflow
infection = mp_join(
  from = S, to = I, edge = lambda,
  by = list(from.to = "Age", from.edge = "Age")
)
recovery  = mp_join(
  from = I, to = R, edge = gamma,
  by = list(from.to = "Age")
)

## wrapping up the links defined by the joins above ------------

ledgers = list(
    decompositions = mp_ledgers(trans_decomposition)
  , aggregations = mp_ledgers(aggregation)
  , normalizations = mp_ledgers(normalization)
  , influences = mp_ledgers(transmission)
  , flows = mp_ledgers(infection, recovery)
)

## instantiate numeric vectors labelled with particular indexes -----------

init_vecs = list(
  state = mp_vector(state),
  flow_rates = mp_vector(flow_rates),
  trans_rates = mp_vector(trans_rates),
  N = mp_vector(mp_extract(aggregation, "groups")),
  norm_state = mp_vector(norm_state),
  susceptibility = mp_vector(susceptibility),
  contact = mp_vector(contact),
  infectivity = mp_vector(infectivity)
)

## wrap up the above definition -----------------

dynamic_model = mp_dynamic_model(
  expr_list = expr_list,
  ledgers = ledgers,
  init_vecs = init_vecs,
  unstruc_mats = list()
)
