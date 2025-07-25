test_that("report variable names are unique", {
  spec = test_cache_read("SPEC-si.rds")
  expect_error(
    mp_tmb_insert_reports(spec
      , report_prob = 0.1
      , mean_delay = 10
      , cv_delay = 0.25
      , incidence_name = "infection"
      , reports_name = "infection"
      , mean_delay_name = "delay"
      , cv_delay_name = "delay"
    )
    , regexp = "inconsistently used for multiple purposes"
  )
})
