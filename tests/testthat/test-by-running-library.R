library(macpan2)
model_names = show_models()$dir
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
