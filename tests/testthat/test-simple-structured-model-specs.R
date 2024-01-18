test_that("tmb model specs can vectorize state updates with indices", {
  state_labels = c("S", "I", "R")
  flow = data.frame(
      rate = c("infection", "recovery")
    , from = c("S"        , "I"       )
    , to   = c("I"        , "R"       )
  )
  sir = mp_tmb_model_spec(
      before = list(
          init = state[S] ~ N - 1
        , state[I] ~ 1
        , state[R] ~ 0
      )
    , during = list(
          flow_rates[infection] ~ beta * state[S] * state[I] / N
        , flow_rates[recovery] ~ gamma * state[I]
        , state ~ state + group_sums(flow_rates, to, state) - group_sums(flow_rates, from, state)
    )
    , default = list(
        state     = mp_zero_vector(state_labels)
      , flow_rates = mp_zero_vector(flow$rate)
      , N = 100
      , beta = 0.25
      , gamma = 0.1
    )
    , integers = list(
         from = mp_positions(flow$from, state_labels)
       , to   = mp_positions(flow$to  , state_labels)
    )
  )
  print(sir)
  # sir$all_default_vars()
  # sir$all_derived_vars()
  # sir$all_formula_vars()

  #mp_extract_exprs(sir, "init")
  
  correct_trajectory = structure(list(matrix = c("state", "state", "state", "state", 
"state", "state", "state", "state", "state", "state"), time = 1:10, 
    row = c("I", "I", "I", "I", "I", "I", "I", "I", "I", "I"), 
    col = c("", "", "", "", "", "", "", "", "", ""), value = c(1.1475, 
    1.316046234375, 1.50841667298164, 1.72768475405867, 1.97722773451576, 
    2.26072663861586, 2.58215441123499, 2.94574812507613, 3.35596031449616, 
    3.81738379261492)), row.names = c(NA, -10L), class = "data.frame")
  actual_trajectory = (sir
    |> mp_simulator(time_steps = 10, outputs = "I")
    |> mp_trajectory()
  )
  expect_equal(actual_trajectory, correct_trajectory)
})
