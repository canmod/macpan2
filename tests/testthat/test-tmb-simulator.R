test_that("informative messages are given when non-existant functions or variables are used", {
  expect_error(
    engine_eval(~ f(1)),
    regexp = "that were not found in the list of available functions"
  )
  expect_error(
    engine_eval(~ x),
    regexp = "but no variables were declared in the model"
  )
})

test_that("empty simulators generate empty trajectories", {
  empty_sim = mp_simulator(
      model = mp_tmb_model_spec()
    , time_steps = 0
    , outputs = character()
  )
  expect_equal(
      mp_trajectory(empty_sim)
    , empty_trajectory
  )
})
