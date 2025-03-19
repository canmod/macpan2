#' Simulation Bounds
#' 
#' Set the simulation bounds (start time and end time) for a 
#' calibration. This is used to override the default simulation 
#' bounds taken from the observed data passed to 
#' \code{\link{mp_tmb_calibrator}}.
#' 
#' @param sim_start Start time of each simulation.
#' @param sim_end End time of each simulation.
#' @param time_scale Qualitative description of the size of a time step.
#' The only valid values are 'steps' and 'daily'. If you would like each time 
#' step in the model to represent one day, then you should consider using 
#' 'daily'. Note that using 'daily' will require that your data represent
#' time using a (1) \code{\link{Date}} vector, (2) \code{\link{character}}
#' vector in YYYY-MM-DD format, or (3) \code{\link{integer}} vector that 
#' counts the number of days since some reference. Otherwise please choose 
#' 'steps' and convert your time column into integer values that represent 
#' the time step that you would like in the model.
#' @param time_column Name of the column that will identify the time at which
#' particular values were observed.
#' 
#' @return An object to be passed to the `time` argument of
#' \code{\link{mp_tmb_calibrator}}.
#' @seealso [mp_sim_offset()]
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
    check_valid_time_scales(self$time_scale)
    constr = get_time_constructor(self$time_scale)
    if (length(column) == 0L) {
      dat_start = self$sim_start
      dat_end = self$sim_end
    } else {
      dat_start = min(column)
      dat_end = max(column)
    }
    constr(self$sim_start, self$sim_end, dat_start, dat_end, original_coercer)
  }
  return_object(self, "SimBounds")
}

#' Simulation Offsets
#' 
#' Offset the starting and ending times of the simulation, from the
#' start and end time of the data used in calibration. This is used to 
#' override the default offsets of zero taken from the observed data passed to 
#' \code{\link{mp_tmb_calibrator}}.
#' 
#' @param sim_start_offset Number of time steps before the first data point
#' to start each simulation.
#' @param sim_end_offset Number of time steps after the last data point to
#' end each simulation.
#' @inheritParams mp_sim_bounds
#' @inherit mp_sim_bounds return
#' 
#' @seealso [mp_sim_bounds()]
#' 
#' @export
mp_sim_offset = function(sim_start_offset, sim_end_offset, time_scale, time_column = "time") {
  self = Base()
  self$sim_start_offset = as.integer(sim_start_offset)
  self$sim_end_offset = as.integer(sim_end_offset)
  self$time_scale = time_scale
  self$time_column = time_column
  self$cal_time_steps = function(data, original_coercer = force) {
    column = data[[self$time_column]]
    check_valid_time_scales(self$time_scale)
    constr = get_time_constructor(self$time_scale)
    if (length(column) == 0L) {
      dat_start = 1L
      dat_end = 1L
    } else {
      if (is.character(column)) column = as.Date(column)
      if (!inherits(column, "Date")) column = as.integer(column)
      dat_start = min(column)
      dat_end = max(column)
    }
    sim_start = dat_start - self$sim_start_offset
    sim_end = dat_end + self$sim_end_offset
    constr(sim_start, sim_end, dat_start, dat_end, original_coercer)
  }
  return_object(self, "SimOffset")
}

check_valid_time_scales = function(scale) {
  valid_time_scales = c("steps", "daily")
  if (!scale %in% valid_time_scales) {
    msg_space(
        "The only valid time scales are 'steps' and 'daily'."
      , "If your data have (1) dates, (2) characters in YYYY-MM-DD format,"
      , "or (3) integers that represent dates,"
      , "and you would like each time step in the model to represent"
      , "one day, then you should choose 'daily'. Otherwise"
      , "please choose 'steps' and convert your time column"
      , "into integer values that represent the time step that you would"
      , "like in the model."
    ) |> mp_wrap() |> stop()
  }
}

get_time_constructor = function(scale) {
  constr = switch(scale
    , steps = CalTimeStepsInt
    , daily = CalTimeStepsDaily
  )
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
