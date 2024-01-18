macpan2:::to_special_vecs(formula = y ~ y + b + x
  , component_list = list(state = "y")
  , matrix_list =  c("state", "x", "b")
  , component_vec_by = c(state = "...RAW...INDICES...")
) |> macpan2:::to_assign()


test_that("out-of-context formula parsing works", {
  expect_equal(
    macpan2:::formula_components(this[is] ~ a(test, 3) / of_the_number + 2.),
    list(
      variables = c("this", "is", "of_the_number", "test"),
      functions = c("~", "[", "+", "/", "a"),
      literals = c(2, 3)
    )
  )
  expect_equal(
    macpan2:::formula_components(~ 1.35 / a(b)),
    list(
      variables = "b",
      functions = c("~", "/", "a"),
      literals = 1.35
    )
  )
})
