library(macpan2)
library(ggplot2)
library(dplyr)
library(tidyr)
N = 1e6
state = c(S = N - 1, I = 1, R = 0)
sir_for_fake_data = TMBModel(
  init_mats = MatsList(

    ## state vectors
      state_vector = state # exact
    , noisy_state = empty_matrix # with observation error
    , incidence = empty_matrix # number of new individuals flowing in
    , outflow = empty_matrix # number of individuals flowing out

    ## flow vectors
    , flow_vector = c(foi = 0, gamma = 0.15) # per-capita flow rate
                                             # one element for each flow
    , flow_pars = c(beta = 0.25) # parameters for modifying flow vector
    , flows = empty_matrix #

    ## state summaries
    , N = N
    , n_states = length(state)

    ## state indices
    , from_indices = c(0, 1)
    , to_indices = c(1, 2)
    , I_index = 1

    ## flow indices
    , foi_index = 0
    , beta_index = 0

    , dummy = empty_matrix
    , .mats_to_save = "noisy_state"
    , .mats_to_return = "noisy_state"
    , .dimnames = list(noisy_state = list(names(state), ""))
  ),
  expr_list = ExprList(
    before = list(
      N ~ sum(state_vector)
    ),
    during = list(
        dummy ~ assign(flow_vector
          , foi_index, 0
          , flow_pars[beta_index] * state_vector[I_index] / N
        )
      , flows ~ flow_vector * state_vector[from_indices]
      , incidence ~ groupSums(flows, to_indices, n_states)
      , outflow ~ groupSums(flows, from_indices, n_states)
      , state_vector ~ state_vector + incidence - outflow
      , noisy_state ~ rnbinom(state_vector, 10)
    )
  ),
  time_steps = Time(300L)
)
fake_data_simulator = sir_for_fake_data$simulator()
fake_data = fake_data_simulator$report()

(fake_data
  %>% filter(matrix == "noisy_state")
  %>% mutate(state = factor(row, levels = names(state)))
  %>% ggplot()
  + facet_wrap(~state, ncol = 1, scale = 'free_y')
  + geom_line(aes(time, value, colour = state))
)








sir_for_fake_data$print_exprs()





simulated_reports = filter(fake_data, matrix == "simulated_reports")$value
expected_reports = filter(fake_data, matrix == "expected_reports")$value

plot(simulated_reports)
lines(expected_reports)

length(simulated_reports)
length(expected_reports)

sir_for_fitting = sir_for_fake_data$insert_exprs(
  condensed_vector ~ c(state_vector[1], incidence[1]),
  .at = 1, .phase = "after"
)$add_mats(
  observed_reports = simulated_reports,
  neg_log_lik = empty_matrix
)
# $insert_exprs(
#   neg_log_lik ~ dnbinom(observed_reports, simulated_reports, 10),
#   .at = 3, .phase = "after"
# )
sir_for_fitting$simulator()
xx$print_exprs("testingtesting.txt")
xx$simulator()$report() %>% filter(matrix == "condensed_vector")
