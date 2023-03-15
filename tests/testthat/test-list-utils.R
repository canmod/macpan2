test_that("nlist and list are equivalent when explicit names are used", {
  expect_identical(nlist(a = 1), list(a = 1))
})
