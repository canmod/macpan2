test_that("all models in the library can run", {
  model_names = setdiff(
    show_models()$dir,
    c("nfds")
  )
  for (m in model_names) {
    print("========")
    print(m)
    calibration_file = system.file("starter_models"
      , m
      , "calibration_example.R"
      , package = "macpan2"
    )
    if (file.exists(calibration_file)) source(calibration_file)
  }
})
