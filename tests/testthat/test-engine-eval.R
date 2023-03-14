test_that("an error is thrown if engine_eval doesn't get a one-sided formula", {
  expect_error(engine_eval(0), "right-hand-side of a one-sided formula")
})
