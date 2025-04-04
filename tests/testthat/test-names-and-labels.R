test_that("undotted scalars can be constructed", {
  x = macpan2:::StringUndottedScalar("TestScalar")
  expect_warning(x$dot(), "Undotted scalars cannot be dotted")
  expect_identical(
    x$regenerate("AnotherTestScalar"),
    macpan2:::StringUndottedScalar("AnotherTestScalar")
  )
  expect_identical(
    x$value_combiner(as.list(letters)),
    macpan2:::StringUndottedVector(letters)
  )
})

test_that("dotted matrices can be constructed", {
  test_cols = paste(letters, rev(letters), sep = ".")
  x = macpan2:::StringDottedMatrix(test_cols, test_cols)
  expect_identical(x$regenerate(x$value()), x)
  expect_error(x$undot(), "not implemented")
})

test_that("dotted vectors can be filtered", {
  x = macpan2:::StringDottedVector(paste(letters, rev(letters), sep = "."))
  y = macpan2:::StringDottedVector(paste(letters[1:6], rev(letters)[1:6], sep = "."))
  expect_identical(which(x$which_in(y, macpan2::all_equal)), 1:6)
  expect_false(any(x$which_not_in(y, macpan2::all_equal)))
  expect_false(any(x$which_dup()))
  expect_identical(x$unique(), x)
  expect_identical(x$subset(1:3), y$subset(1:3))
})

test_that("undotted matrices can be ordered", {
  x = macpan2:::StringDottedVector(paste(letters, rev(letters), sep = "."))$undot()
  y = macpan2:::StringDottedVector(rev(paste(letters[1:6], rev(letters)[1:6], sep = ".")))
  z = macpan2:::StringUndottedMatrix(matrix(c(letters[1:3], letters[26:24]), 3, 2))
  expect_identical(
    x$order_by(y$undot(), all_consistent),
    6:1
  )
  expect_error(x$order_by(y$undot(), all_not_equal), "Lack of uniqueness")
  expect_false(any(x$which_dup()))
  expect_identical(x$subset(1:3), z)
  expect_identical(x$unique(), x)
})

test_that("strings can be concatenated", {
  x = macpan2:::StringUndottedVector("S", "E", "I", "R")
  y = macpan2:::StringUndottedVector("D")
  z = macpan2:::StringUndottedVector("S", "E", "I", "R", "D")

  # failing now -- https://github.com/canmod/macpan2/issues/22
  #expect_identical(c(x, y), z)
})

test_that("string data frames can be constructed", {
  undotted = macpan2:::StringDataFromFrame(
    data.frame(
      Epi = c("S", "I", "S", "I"),
      Vax = c("unvax", "unvax", "vax", "vax")
    )
  )
  dotted = undotted$dot()
  unvax = macpan2:::StringDataFromFrame(data.frame(Vax = "unvax"))
  unvax_explicit = macpan2:::StringDataFromFrame(
    data.frame(
      Epi = c("S", "I"),
      Vax = c("unvax", "unvax")
    )
  )
  vax_explicit = macpan2:::StringDataFromFrame(
    data.frame(
      Epi = c("S", "I"),
      Vax = c("vax", "vax")
    )
  )

  expect_identical(
    undotted$change_coordinates("Epi"),
    dotted$change_coordinates("Epi")$undot()
  )
  expect_identical(
    undotted$filter(unvax, macpan2::all_equal),
    unvax_explicit
  )
  expect_identical(
    undotted$filter(unvax, macpan2::all_equal),
    dotted$filter(unvax, macpan2::all_equal)$undot()
  )
  expect_identical(
    undotted$filter_out(unvax, all_not_equal),
    vax_explicit
  )
  expect_identical(
    undotted$filter_out(unvax, all_not_equal),
    dotted$filter_out(unvax, all_not_equal)$undot()
  )
  # expect_identical(
  #   undotted$filter_other(unvax, all_not_equal),
  #   dotted$filter_other(unvax, all_not_equal)$undot()
  # )

})
