#' Simulation Bounds (Experimental)
#' 
#' Set the simulation bounds (start time and end time) for a 
#' calibration. This is used to override the default simulation 
#' bounds taken from the observed data passed to 
#' \code{\link{mp_tmb_calibrator}}. the first date is when the first 
#' simulated time step (chosen to be before the first data point
#' so that infectious individuals can be built up) and
#' the second date is the last simulated time step (chosen
#' to be after the last data point so that there can be
#' a forecast period). the last argument gives the 
#' scale of a single time step (in this case it should
#' always be daily).
#' 
#' @param sim_start Start time of each simulation.
#' @param sim_end End time of each simulation.
#' @param time_scale Qualitative description of the size of a time step.
#' currently only `"steps"`, `"daily"`, and `"weekly"` are allowed,
#' and but `"steps"` is the only recommended version as the other two
#' are poorly tested and will throw a warning. The recommended `"steps"`
#' option assumes that positive integers are used to indicate a particular
#' point in the simulation.
#' 
#' @export
mp_sim_bounds = function(sim_start, sim_end, time_scale, time_column = "time") {
  self = Base()
  self$sim_start = sim_start
  self$sim_end = sim_end
  self$time_scale = time_scale
  self$time_column = time_column
  self$cal_time_steps = function(data, original_coercer = force) {
    column = data[[self$time_column]]
    dat_start = min(column)
    dat_end = max(column)
    ## TODO: check type consistency
    constr = switch(self$time_scale
      , steps = CalTimeStepsInt
      , daily = CalTimeStepsDaily
    )
    constr(self$sim_start, self$sim_end, dat_start, dat_end, original_coercer)
  }
  return_object(self, "SimBounds")
}

mp_sim_offset = function(sim_start_offset, sim_end_offset, time_scale, time_column = "time") {
  self = Base()
  self$sim_start_offset = as.integer(sim_start_offset)
  self$sim_end_offset = as.integer(sim_end_offset)
  self$time_scale = time_scale
  self$time_column = time_column
  self$cal_time_steps = function(data, original_coercer = force) {
    column = data[[self$time_column]]
    if (is.character(column)) column = as.Date(column)
    if (!inherits(column, "Date")) column = as.integer(column)
    dat_start = min(column)
    dat_end = max(column)
    sim_start = dat_start - self$sim_start_offset
    sim_end = dat_end + self$sim_end_offset
    ## TODO: check type consistency
    constr = switch(self$time_scale
      , steps = CalTimeStepsInt
      , daily = CalTimeStepsDaily
    )
    constr(sim_start, sim_end, dat_start, dat_end, original_coercer)
  }
  return_object(self, "SimOffset")
}



CalTime = function(sim_start, sim_end, time_scale) {
  self = mp_time_scale(sim_start, sim_end, time_scale, checker = NoError)
  self$sim_start = sim_start
  self$sim_end = sim_end
  self$time_scale = time_scale
  
  self$data_start = sim_start
  self$data_end = sim_end
  self$update_data_bounds = function(data) {
    self$data_start = min(data$time)
    self$data_end = max(data$time)
    self$start = min(self$data_start, self$sim_start, na.rm = TRUE)
    self$end = max(self$data_end, self$sim_end, na.rm = TRUE)
  }
  
  self$data_time_steps = function() {
    self$time_ids(self$data_start):self$time_ids(self$data_end)
  }
  self$data_bound_steps = function() {
    c(self$time_ids(self$data_start), self$time_ids(self$data_end))
  }
  
  return_object(self, "CalTime")
}
