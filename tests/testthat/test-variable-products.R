test_that("state and flow variables products are correct", {
  sir = Compartmental(system.file("starter_models", "sir", package = "macpan2"))
  vax = Compartmental(system.file("starter_models", "vax", package = "macpan2"))

  expect_identical(
    union_vars(
      cartesian(
        sir$variables$state(),
        vax$variables$state()
      ),
      cartesian(
        sir$variables$flow(),
        vax$variables$state()
      ),
      cartesian(
        sir$variables$state(),
        vax$variables$flow()
      )
    )$labels(),
    c("S.unvax", "I.unvax", "R.unvax", "S.vax", "I.vax", "R.vax",
    "foi.unvax", "gamma.unvax", "foi.vax", "gamma.vax", "S.dose_rate",
    "I.dose_rate", "R.dose_rate")
  )
})