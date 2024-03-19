library(tidyr)
library(macpan2)
library(dplyr)

## SIR with state vector ----
## Taken from here https://canmod.github.io/macpan2/articles/design_concepts.html#vectors-in-the-tmb-engine
state_labels = c("S", "I", "R")

flow = data.frame(
  rate = c("infection", "recovery")
  , from = c("S"        , "I"       )
  , to   = c("I"        , "R"       )
)

sir = mp_tmb_model_spec(
  before = list(
    state[S] ~ N - 1
    , state[I] ~ 1
    , state[R] ~ 0
  )
  , during = list(
    flow_rate[infection] ~ beta * state[S] * state[I] / N
    , flow_rate[recovery] ~ gamma * state[I]
    , state ~ state + group_sums(flow_rate, to, state) - group_sums(flow_rate, from, state)
  )
  , default = list(
      state     = mp_zero_vector(state_labels)
    , flow_rate = mp_zero_vector(flow$rate)
    , N = 100
    , beta = 0.25
    , gamma = 0.1
  )
  , integers = list(
      from = mp_positions(flow$from, state_labels)
    , to   = mp_positions(flow$to  , state_labels)
  )
)
## --------------------------
# function to generate simulated data given a state
# vector of initial conditions to be passed to defaults
sim_data <- function(state){
  (mp_tmb_update(sir,default=list(state=state))
   |> mp_simulator(time_steps = 10, outputs = c("state"))
   |> mp_trajectory()
  )
}

# read in defaults
# For the use case when state vectors are needed
# and are read in from a file (ex. NFDS model),
# or in general when any default is not a single number
# and is you don't want to initialize the vector to 
# zeroes.
sir_defaults = (read.csv(file.path("misc","experiments","defaults","sir","simulator_defaults.csv"))
                %>% select(matrix,value)
                %>% pivot_wider(names_from=matrix,values_from=value)
                
                
)

## state vector objects -----

#1. mp_zero_vector - this works, as expected
state_mp_zero = mp_zero_vector(state_labels)
names(state_mp_zero)
dimnames(state_mp_zero)
sim_data(state_mp_zero)

#2. tibble/data.frame - this errors
state_tibble = sir_defaults
# same names attributes as 1.
names(state_tibble)
# diff dimnames attributes from 1.
dimnames(state_tibble)
# errors
sim_data(state_tibble)

#3a. as.matrix - this errors
state_matrix = as.matrix(sir_defaults,ncol=3)
# no names, doesn't save names attribute?
names(state_matrix)
# has names in dimnames[[2]]
dimnames(state_matrix)
# errors
sim_data(state_matrix) 

#3b. as.matrix, and manually setting names and dimnames - this errors
state_matrix_2 = as.matrix(sir_defaults,ncol=3)
names(state_matrix_2)<-names(state_mp_zero)
names(state_matrix_2)
dimnames(state_matrix_2)<-dimnames(state_mp_zero)
dimnames(state_matrix_2)
sim_data(state_matrix_2) 

#4. convert to vector with c(), and manually setting names and dimnames - this works
state_vector = c(state_matrix)
names(state_vector)<-names(state_mp_zero)
dimnames(state_vector)<-dimnames(state_mp_zero)
sim_data(state_vector)

#5. 
# we want to be able to pass matrices without names as well
# NFDS initial state vector with initial conditions - not named