## FIXME: this is just a hack to get the coverage up so that 
## i can better understand what needs to be retained and what
## is out-of-date. but for now we have just copied the code
## from the engine_agnostic_grammar vignette.

## ----setup, echo = FALSE, message = FALSE, warning = FALSE--------------------
library(macpan2)
library(ggplot2)
library(dplyr)


## ----SIR-starter, echo = FALSE------------------------------------------------
## helper function to simplify the exposition in this vigette -----------
SIR_starter <- function(
  # index tables for model quantities
  state,
  rate,
  # ledgers tabulating the use of different functional forms
  flow, # list of individual ledgers
  force_of_infection
){
  
  ## Set up expressions list for each functional form --------------
  ## names refer to when the calculation gets performed relative to 
  ## the simulation time-step loop (before, during, ...)
  expr_list <- mp_tmb_expr_list(
    before = list(
      ## aggregations
        N ~ sum(state)
    ),
    during = list(
      ## force of infections
        rate[infection_flow_rates] ~
          state[infectious_states] * rate[transmission_rates] / N
  
      ## unsigned individual flows
      , flow_per_time ~ state[from_states] * rate[flow_rates]
  
      ## state update
      , total_inflow ~ group_sums(flow_per_time, to_states, state)
      , total_outflow ~ group_sums(flow_per_time, from_states, state)
      , state ~ state + total_inflow - total_outflow
    )
  )
  
  ## Ledgers for each specific calculation --------------
  ledgers <- list(
    flow = mp_ledgers(flow),
    force_of_infection = mp_ledgers(force_of_infection)
  )
  
  ## Initialize vectors from index tables (with all zeros for values) --------------
  # used as placeholders for user input
  init_vecs <- list(
    state = mp_vector(state),
    rate = mp_vector(rate)
  )
  
  ## Initialize model object -----------------
  mp_dynamic_model(
    expr_list = expr_list,
    ledgers = ledgers,
    init_vecs = init_vecs
  )
}


## ----sir-index-tables---------------------------------------------------------
## index tables to label model quantities -------------------------
state <- mp_index(Epi = c("S", "I", "R"))
rate <- mp_index(Epi = c("beta", "gamma", "lambda"))


## ----sir-state-and-rate-------------------------------------------------------
state
rate


## ----sir-infection-ledger-----------------------------------------------------
## infection ledger -------------------------
infection <- mp_join(
  from_states = mp_subset(state, Epi = "S"),
  to_states = mp_subset(state, Epi = "I"), 
  flow_rates = mp_subset(rate, Epi = "lambda")
)


## ----sir-infection-ledger-inputs----------------------------------------------
mp_subset(state, Epi = "S")
mp_subset(state, Epi = "I")
mp_subset(rate, Epi = "lambda")


## ----sir-infection-ledger-2---------------------------------------------------
infection


## ----sir-recovery-ledger------------------------------------------------------
## recovery ledger -------------------------
recovery  <- mp_join(
  from_states = mp_subset(state, Epi = "I"),
  to_states = mp_subset(state, Epi = "R"),
  flow_rates = mp_subset(rate, Epi = "gamma")
)

recovery


## ----sir-foi-ledger-----------------------------------------------------------
## force of infection ledger -------------------------
# infection additionally involves the calculation of a force of infection
force_of_infection <- mp_join(
  infectious_states = mp_subset(state, Epi = "I"),
  transmission_rates = mp_subset(rate, Epi = "beta"),
  infection_flow_rates = mp_subset(rate, Epi = "lambda")
)


## ----sir----------------------------------------------------------------------
## SIR model object -------------------------
sir <- SIR_starter(
  # index tables
  state = state,
  rate = rate,
  # ledgers
  flow = list(
    infection,
    recovery
  ),
  force_of_infection = force_of_infection
)


## ----sir-simulator------------------------------------------------------------
## SIR model simulator -------------------------
sir_simulator <- mp_tmb_simulator(
  dynamic_model = sir,
  vectors = list(
    state = c(S = 999, I = 1, R = 0),
    rate = c(beta = 0.25, gamma = 0.1)
  ),
  time_steps = 100
)


## ----sir-results--------------------------------------------------------------
## SIR model simulation results -------------------------
sir_results <- mp_report(sir_simulator)


## ----sir-results-head---------------------------------------------------------
head(sir_results)


## ----sir-ggplot-example, fig.width = 6, fig.height = 4------------------------
(sir_results
  |> filter(matrix == "state") # keep just the state variables at each point in time
  |> mutate(state = factor(row, levels = c("S", "I", "R"))) # to enforce logical state ordering in legend
  |> ggplot(aes(time, value, colour = state))
  +  geom_line()
)


## ----pivot_wider--------------------------------------------------------------
sir_results_wide <- (sir_results
    |> dplyr::filter(matrix == "state") # keep state variables at each point in time
    ## drop unneeded columns before pivoting
    |> dplyr::select(-c(matrix, col))
    |> tidyr::pivot_wider(id_cols = time, names_from = row)
)

head(sir_results_wide, n = 3)


## ----sir-base-plot-ex, fig.width = 6------------------------------------------
with(sir_results_wide,
     plot(x = time,
          y = I,
          type = "l")
)


## ----sir-base-matplot-ex, fig.width = 6---------------------------------------
par(las = 1) ## horizontal y-axis ticks
matplot(sir_results_wide[, 1],
        sir_results_wide[,-1],
        type = "l",
        xlab = "time", ylab = "")
legend("left", col = 1:3, lty = 1:3, legend = state$labels())


## ----strain-indices-----------------------------------------------------------
Strain_indices <- c("A", "B")


## ----strain-expand-I----------------------------------------------------------
I_indices <- mp_cartesian(
  mp_subset(state, Epi = "I"),
  mp_index(Strain = Strain_indices)
)

I_indices


## ----two-strain-state---------------------------------------------------------
state <- mp_union(
  mp_subset(state, Epi = "S"),
  I_indices, 
  mp_subset(state, Epi = "R")
)

state


## ----two-strain-rate----------------------------------------------------------
rate <- 
  mp_union(
  # stratify rates involved in the infection process by strain
  mp_cartesian(
    mp_subset(rate, Epi = c("beta", "lambda")),
    mp_index(Strain = Strain_indices)
  ),
  # recovery rate will be the same across strains
  mp_subset(rate, Epi = "gamma")
)

rate


## ----two-strain-infection-default---------------------------------------------
# infection ledger from before
mp_join(
  from_states = mp_subset(state, Epi = "S"),
  to_states = mp_subset(state, Epi = "I"), 
  flow_rates = mp_subset(rate, Epi = "lambda")
)


## ----two-strain-infection-ledger----------------------------------------------
## new infection ledger -------------------------
infection <- mp_join(
  from_states = mp_subset(state, Epi = "S"),
  to_states = mp_subset(state, Epi = "I"), 
  flow_rates = mp_subset(rate, Epi = "lambda"),
  by = list(
    to_states.flow_rates = "Strain"
  )
)

infection


## ----two-strain-recovery-ledger-----------------------------------------------
recovery <- mp_join(
    from_states = mp_subset(state, Epi = "I"),
    to_states = mp_subset(state, Epi = "R"),
    flow_rates = mp_subset(rate, Epi = "gamma")
)
recovery


## ----two-strain-foi-default---------------------------------------------------
mp_join(
  infection_flow_rates = mp_subset(rate, Epi = "lambda"),
  infectious_states = mp_subset(state, Epi = "I"),
  transmission_rates = mp_subset(rate, Epi = "beta")
)


## ----two-strain-foi-ledger----------------------------------------------------
## new force of infection ledger -------------------------
force_of_infection <- mp_join(
  infection_flow_rates = mp_subset(rate, Epi = "lambda"),
  infectious_states = mp_subset(state, Epi = "I"),
  transmission_rates = mp_subset(rate, Epi = "beta"),
  by = list(
    infection_flow_rates.infectious_states = "Strain", # first pairwise join
    infectious_states.transmission_rates = "Strain" # second pairwise join
  )
)

force_of_infection


## ----two-strain-results, fig.width = 6, fig.height = 4------------------------
two_strain_model <- SIR_starter(
  # index tables
  state = state,
  rate = rate,
  # ledgers
  flow = list(
    infection,
    recovery
  ),
  force_of_infection = force_of_infection
)

two_strain_simulator <- mp_tmb_simulator(
  dynamic_model = two_strain_model,
  vectors = list(
    state = c(S = 998, I.A = 1, I.B = 1, R = 0),
    rate = c(beta.A = 0.25, gamma = 0.1, beta.B = 0.2)
  ),
  time_steps = 100
)

two_strain_results <- (mp_report(two_strain_simulator)
  |> filter(matrix == "state")                    
)

levels <- unique(two_strain_results$row) # get state variables in the desired order

(two_strain_results # keep state variables at each point in time
  |> mutate(state = factor(row, levels = levels)) # to enforce logical state ordering in plot
  |> ggplot(aes(time, value, colour = state))
  +  geom_line()
)


## ----sir-starter-print--------------------------------------------------------
## helper function to simplify the exposition in this vigette -----------
SIR_starter <- function(
  # index tables for model quantities
  state,
  rate,
  # ledgers tabulating the use of different functional forms
  flow, # list of individual ledgers
  force_of_infection
){
  
  ## Set up expressions list for each functional form --------------
  ## names refer to when the calculation gets performed relative to 
  ## the simulation time-step loop (before, during, ...)
  expr_list <- mp_tmb_expr_list(
    before = list(
      ## aggregations
        N ~ sum(state)
    ),
    during = list(
      ## force of infections
        rate[infection_flow_rates] ~
          state[infectious_states] * rate[transmission_rates] / N
  
      ## unsigned individual flows
      , flow_per_time ~ state[from_states] * rate[flow_rates]
  
      ## state update
      , total_inflow ~ group_sums(flow_per_time, to_states, state)
      , total_outflow ~ group_sums(flow_per_time, from_states, state)
      , state ~ state + total_inflow - total_outflow
    )
  )
  
  ## Ledgers for each specific calculation --------------
  ledgers <- list(
    flow = mp_ledgers(flow),
    force_of_infection = mp_ledgers(force_of_infection)
  )
  
  ## Initialize vectors from index tables (with all zeros for values) --------------
  # used as placeholders for user input
  init_vecs <- list(
    state = mp_vector(state),
    rate = mp_vector(rate)
  )
  
  ## Initialize model object -----------------
  mp_dynamic_model(
    expr_list = expr_list,
    ledgers = ledgers,
    init_vecs = init_vecs
  )
}
