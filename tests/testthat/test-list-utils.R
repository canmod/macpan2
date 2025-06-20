library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("nlist and list are equivalent when explicit names are used", {
  expect_identical(nlist(a = 1), list(a = 1))
})

test_that("named row and column vectors are melted correctly", {
  rv = macpan2:::melt_matrix(t(c(a = 1, b = 2)))
  cv = macpan2:::melt_matrix(  c(a = 1, b = 2) )
  expect_identical(rv$col, cv$row)
  expect_identical(rv$row, rep("", 2))
  expect_identical(cv$col, rep("", 2))
})
