test_that("dates can be turned into time-steps", {
  library(lubridate)
  base_date = as.Date("2024-01-25")
  expect_error(
      macpan2:::Weekly(start = base_date - days(30), end = base_date)
    , regexp = "The following fractional time steps were found"
  )
  expect_error(
      macpan2:::Daily(start = base_date + days(2), end = base_date)
    , regexp = "The following times fell before the start time"
  )
  expect_error(
      macpan2:::Daily(start = base_date - hours(26), end = base_date)
    , regexp = "none of these are in the following list of valid classes"
  )
  
  good_daily = macpan2:::Daily(start = base_date - days(17), end = base_date)
  good_weekly = macpan2:::Weekly(start = base_date - weeks(10), end = base_date)
  
  expect_error(
      good_weekly$frame(base_date - weeks(c(c(10, 5, 8), 10:0)))
    , regexp = "The following times with duplicated time steps were found"
  )
  expect_equal(
      good_weekly$frame(base_date - weeks(10:0))
    , data.frame(time_step = 1:11, ending_time = as.character(base_date - weeks(10:0)))
  )
  expect_equal(
      good_daily$time_ids(base_date - weeks(2:0))
    , seq(from = 4L, to = 18L, by = 7)
  )
})
