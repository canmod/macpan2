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
