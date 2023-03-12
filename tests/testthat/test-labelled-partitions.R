test_that("model files can be read in and used", {
  f = model_starter("seir_symp_vax"
    , file.path(tempdir(TRUE), paste0(sample(LETTERS, 50, TRUE), collapse = ""))
  )
  f$freeze()
  m = Model(f)
  expect_identical(
    m$flow_variables()$labels(),
    c(
      "foi..unstructured.unvax", "foi..unstructured.vax", "alpha..unstructured.",
      "gamma.mild.component.", "gamma.severe.component.", "..unstructured.dose_rate"
    )
  )
  expect_identical(
    m$flow_variables()$names(),
    c("Epi", "Symp", "SympStruc", "Vax")
  )
  expect_identical(
    m$flow_variables()$name(),
    "Epi.Symp.SympStruc.Vax"
  )
  expect_identical(
    m$state_variables()$labels(),
    c("S..unstructured.unvax", "E..unstructured.unvax", "I.mild.component.unvax",
      "I.severe.component.unvax", "R..unstructured.unvax", "S..unstructured.vax",
      "E..unstructured.vax", "I.mild.component.vax", "I.severe.component.vax",
      "R..unstructured.vax")
  )
  expect_identical(
    m$state_variables()$names(),
    c("Epi", "Symp", "SympStruc", "Vax")
  )
  expect_identical(nrow(m$flows()), 4L)
  expect_identical(nrow(m$flows_expanded()), 17L)
  m$variables()$labels()
  m$derivations()
})

p = Partition(data.frame(A = letters[1:4], b = letters[26:23]))
dotted_scalar = macpan2:::StringDottedScalar("a.z")
dotted_vector = macpan2:::StringDottedVector("a.z", "b.y")
undotted_matrix = dotted_vector$undot()
to_labels(p)
to_labels(p$.partition)
to_labels(dotted_scalar)
to_labels(dotted_vector)
to_labels(undotted_matrix)
to_names(p)
to_names(p$.partition)
to_names(dotted_scalar)
to_name(p)
to_name(p$.partition)
to_name(dotted_scalar)
