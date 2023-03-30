simple_sims = function(iteration_exprs, time_steps, ...) {
  TMBModel(
    init_mats = MatsList(...
      , .mats_to_return = "x"
      , .mats_to_save = "x"
    ),
    expr_list = ExprList(during = iteration_exprs),
    time_steps = Time(time_steps)
  )$simulator()$report(.phases = c("before", "during"))
}

if (FALSE) {

  simulator = TMBModel(
    init_mats = MatsList(
      A = matrix(c(1, 1, 1, 0), 2, 2),
      x = c(1, 0),
      .mats_to_return = "x", .mats_to_save = "x"
    ),
    expr_list = ExprList(during = list(x ~ A %*% x)),
    time_steps = Time(10)
  )$simulator()
  filter(simulator$report(), row == 0)

  init = list(x = 1)
  simple_sims = function(iteration_exprs, time_steps, ...) {
    TMBModel(
      init_mats = MatsList(..., .mats_to_return = "x", .mats_to_save = "x"),
      expr_list = ExprList(during = iteration),
      time_steps = Time(time_steps)
    )$simulator()$report(.phases = c("before", "during"))
  }
  iteration = list(x ~ x + 0.01 * x)
  simple_sims(iteration, time_steps = 10, x = 1)


  simulator = TMBModel(
    init_mats = MatsList(
      state_vector = c(S = 100, E = 1, I = 0, R = 0),
      flow_vector = c(foi = 0, alpha = 0.1, gamma = 0.06),
      beta = 0.24,
      foi_index = 0, I_index = 2,
      from_index = c(0, 1, 2),
      to_index = c(1, 2, 3),
      n_states = 4,
      N = empty_matrix,
      incidence_vector = empty_matrix,
      outflow_vector = empty_matrix,
      flows = empty_matrix,
      dummy = empty_matrix,
      .mats_to_save = "state_vector", .mats_to_return = "state_vector"
    ),
    expr_list = ExprList(
      before = list(N ~ sum(state_vector)),
      during = list(
        #flow_vector[foi_index] ~ beta * state_vector[I_index] / N,
        dummy ~ assign(flow_vector, foi_index, 0, beta * state_vector[I_index] / N),
        flows ~ flow_vector * state_vector[from_index],
        incidence_vector ~ groupSums(flows, to_index, n_states),
        outflow_vector ~ groupSums(flows, from_index, n_states),
        state_vector ~ state_vector + incidence_vector - outflow_vector
      )
    ),
    time_steps = Time(100)
  )$simulator()

  simulations = simulator$report()
  simulations
}

