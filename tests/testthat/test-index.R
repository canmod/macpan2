library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2)
test_that("functions that act on index objects behave", {
  # SEIR epi compartments with vaccination status
  epi_states = c("S","E","I","R")
  epi = mp_index(epi = epi_states)
  vax_status = mp_index(vax_status = c("vax", "unvax"))
  state = mp_cartesian(epi, vax_status)
  
  # mp_index can take input data frame
  epi_df = data.frame(epi = epi_states)
  expect_identical(mp_index(epi_df)$partition,epi$partition)
  
  # look-up symbol that doesn't exist in index
  expect_error(mp_lookup(state, "H"), regexp = "failed to find symbol")
  
  # partition state index by "vax" in two different ways
  expect_identical(mp_lookup(state,"vax"), mp_slices(state)$vax)
  
  # factor state index to recover original indices
  expect_identical(mp_factors(state)$epi |> data.frame(), data.frame(epi))
  expect_identical(mp_factors(state)$vax_status |> data.frame(), data.frame(vax_status))
  
  # test that we can recover labels from index
  expect_identical(mp_labels(epi),epi_states)
})
