TMBCalDataStruc = function(data, time) {
  self = Base()
  
  infer_time_step = function(x) {
    y = is.numeric(x)
    if (y) return(TRUE)
    if (inherits(x, "character")) {
      if (all(grepl("^[0-9]+$", x))) {
        return(TRUE)
      }
    }
    FALSE
  }
  if (is.null(time)) {
    if (infer_time_step(data$time)) {
      data$time = as.integer(data$time)
      time = Steps(max(data$time))
    } else {
      ## TODO: I'm guessing this could fail cryptically
      time = Daily(min(data$time), max(data$time), checker = NoError)
    }
  }
  self$time_steps = time$bound_steps()[2L]
  data$time_ids = time$time_ids(data$time)
  data = rename_synonyms(data
    , time = c(
        "time", "Time", "ID", "time_id", "id", "date", "Date"
      , "time_step", "timeStep", "TimeStep"
    )
    , matrix = c(
        "matrix", "Matrix", "mat", "Mat", "variable", "var", "Variable", "Var"
    )
    , row = c("row", "Row")
    , col = c("col", "Col", "column", "Column")
    , value = c("value", "Value", "val", "Val", "default", "Default")
  )
  self$matrix_list = split(data, data$matrix)
  
  self$check_matrices = function(matrices) {
    bad_traj = !matrices %in% names(self$matrix_list)
    if (any(bad_traj)) {
      sprintf("%s (including %s) %s:\n     %s"
        , "Requested trajectories"
        , paste0(matrices[bad_traj], collapse = ", ")
        , "are not available in the data, which includes the following"
        , paste(names(self$matrix_list), collapse = ", ")
      ) |> stop()
    }
    NULL
  }
  
  self$init_list = function(matrices) {
    self$check_matrices(matrices)
    self$matrix_list[matrices]
  }
  return_object(self, "TMBCalDataStruc")
}

## TODO: Still splitting on matrices, which doesn't allow flexibility
## in what counts as an 'output'. In general, an output could be
## a matrix, row, or column.
