#' Time
#'
#' Define the number of time steps in a compartmental model in TMB.
#'
#' @param time_steps Number of time steps in the simulation loop.
#'
#' @return Object of class \code{Time} with the following methods.
#'
#' ## Methods
#'
#' * `$data_arg()` -- Return the following components of the data structure
#' to pass to C++.
#'     * `time_steps` -- Number of time steps in the simulation loop.
#'
#' @noRd
Time = function(time_steps) {
  self = Base()
  self$time_steps = time_steps
  self$data_arg = function() list(time_steps = self$time_steps)
  return_object(self, "Time")
}

DiffTime = function(start_time, end_time) {
  self = Base()
  self$start_time = valid$scalar$assert(start_time)
  self$end_time = valid$scalar$assert(end_time)
  self$start = function() {}
}

Daily = function(start_date, end_date) {
  self = Base()
  self$start_date = as.Date(start_date)
  self$end_date = as.Date(end_date)
  self$time_steps = function() {
    (self$end_date
     |> difftime(self$start_date, units = "days")
     |> as.integer()
    )
  }
  self$data_arg = function() list(list_steps = self$time_steps())
}
