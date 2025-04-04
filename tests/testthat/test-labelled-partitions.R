test_that("model files can be read in and used", {
  skip("Skipping because we are still reworking this test for new approach")
  f = mp_model_starter("seir_symp_vax"
    , file.path(tempdir(TRUE), paste0(sample(LETTERS, 50, TRUE), collapse = ""))
  )
  f$freeze()
  m = Model(f)
  expect_identical(
    m$variables$flow()$labels(),
    c(
      "alpha..unstructured.",
      "gamma.mild.component.",
      "gamma.severe.component.",
      "foi..unstructured.unvax",
      "foi..unstructured.vax",
      "..unstructured.dose_rate"
    )
  )
  expect_identical(
    m$variables$flow()$names(),
    c("Epi", "Symp", "SympStruc", "Vax")
  )
  expect_identical(
    m$variables$flow()$name(),
    "Epi.Symp.SympStruc.Vax"
  )
  expect_identical(
    m$variables$state()$labels(),
    c("S..unstructured.unvax", "E..unstructured.unvax", "I.mild.component.unvax",
      "I.severe.component.unvax", "R..unstructured.unvax", "S..unstructured.vax",
      "E..unstructured.vax", "I.mild.component.vax", "I.severe.component.vax",
      "R..unstructured.vax")
  )
  expect_identical(
    m$variables$state()$names(),
    c("Epi", "Symp", "SympStruc", "Vax")
  )
  expect_identical(nrow(m$flows()), 4L)
  expect_identical(nrow(m$flows_expanded()), 17L)
  m$variables$all()$labels()
  m$def$derivations()
})

test_that("labels, name, and names conversion is correct", {

  p = macpan2:::Partition(data.frame(A = letters[1:4], B = letters[26:23]))
  dotted_scalar = macpan2:::StringDottedScalar("a.z")
  dotted_vector = macpan2:::StringDottedVector("a.z", "b.y")
  undotted_vector = dotted_scalar$undot()
  undotted_matrix = dotted_vector$undot()

  expect_identical(to_labels(p), c("a.z", "b.y", "c.x", "d.w"))
  expect_identical(to_labels(p$.partition), c("a.z", "b.y", "c.x", "d.w"))
  expect_identical(to_labels(dotted_scalar), "a.z")
  expect_identical(to_labels(dotted_vector), c("a.z", "b.y"))
  expect_identical(to_labels(undotted_matrix), c("a.z", "b.y"))

  expect_identical(to_names(p), c("A", "B"))
  expect_identical(to_names("A"), "A")
  expect_identical(to_names(c("A", "B")), c("A", "B"))
  expect_identical(to_names("A.B"), c("A", "B"))
  expect_error(to_names(c("A.B", "C.D")), "vector contained invalid strings")
  expect_identical(to_names(p$.partition), c("A", "B"))
  expect_identical(to_names(dotted_scalar), c("a", "z"))
  expect_identical(to_names(undotted_vector), c("a", "z"))
  expect_error(
    to_names(character(0L)),
    "an empty character vector cannot be turned into names"
  )


  expect_identical(to_name(p), "A.B")
  expect_error(to_name(c("a.z", "b.y")), "vector contained invalid strings")
  expect_identical(to_name("a.z"), "a.z")
  expect_identical(to_name(c("a", "z")), "a.z")
  expect_identical(to_name(p$.partition), "A.B")
  expect_identical(to_name(dotted_scalar), "a.z")
  expect_error(
    to_name(character(0L)),
    "an empty character vector cannot be turned into a name"
  )
})

test_that("labels are appropriately generated when null partitions are involved", {
  skip("Skipping because we are still reworking this test for new approach")
  m = Compartmental(system.file("model_library", "sir_vax", package = "macpan2"))
  v = m$variables$all()
  s = m$variables$state()

  expect_identical(
    v$filter("foi.", .wrt = "Epi.Vax")$labels(),
    "foi."
  )
  expect_identical(
    v$filter("foi", "I", .wrt = "Epi")$labels(),
    c("I.unvax", "foi.unvax", "I.vax", "foi.vax", "foi.")
  )
  expect_identical(
    v$filter("not.exist")$labels(),
    character(0L)
  )
  expect_identical(
    v$filter()$union(s)$labels(),
    s$union(v$filter())$labels()
  )
})
