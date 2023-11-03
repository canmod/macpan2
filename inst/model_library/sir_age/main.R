proj_dir = "inst/model_library/sir_age"

proj_dir |> file.path("model_structure.R") |> source()
proj_dir |> file.path("numerical_inputs.R") |> source()
proj_dir |> file.path("simulator.R") |> source()
