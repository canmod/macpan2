test_that("all models in the library can run", {
  model_names = setdiff(
    show_models()$dir,
    ## TODO: review this list of models
    ## to avoid running. maybe we do
    ## want to run some of them. the
    ## issue with macpan_base for now
    ## is that it runs too slowly.
    ## what we probably want is to have 
    ## a test.R in the model files that
    ## will run a small test of the
    ## model, rather than running
    ## calibration_example.R
    c("nfds", "shiver", "macpan_base", "awareness")
  )
  for (m in model_names) {
    print("====MODEL====")
    print(m)
    calibration_file = system.file("starter_models"
      , m
      , "calibration_example.R"
      , package = "macpan2"
    )
    if (file.exists(calibration_file)) source(calibration_file, verbose= TRUE)
  }
})

