test_that("an informative error msg results when there is no column indicating the matrix", {
  sir = "SPEC-sir.rds" |> test_cache_read()
  sir_sim = "SIM-sir_5_I.rds" |> test_cache_read()
  
  set.seed(101)
  dd = sir_sim$report() |>
     ## leaving out 'matrix' here on purpose
      select(time, value)
  
  expect_error(
    mp_tmb_calibrator(spec = sir, data = dd),
    "Supplied data did not contain a column called"
  )
})
