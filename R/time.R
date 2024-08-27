# y ~ group_sums(x[i], j, y)
# y ~ group_sums(v * x[i], j, y)
# y ~ A %*% x

#' Time Scale
#' 
#' @param start First date or time in the first time step
#' @param end Last date or time in the last time step
#' @param time_step_scale TODO
#' @param ... TODO
#' @importFrom utils getFromNamespace
#' @export
mp_time_scale = function(start, end
    , time_step_scale = c("steps", "daily", "weekly")
    , ...
  ) {
  time_cls = (time_step_scale
    |> match.arg() 
    |> var_case_to_cls_case()
    |> getFromNamespace("macpan2")
  )
  time_cls(start, end, ...)
}
# mp_time_scale("2000-01-01", "2000-01-08", "weekly")

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

as_Time = function(x) UseMethod("as_Time")
as_Time.Time = function(x) x
as_Time.DateTimeSteps = function(x) x$time_obj()

## Abstract class for generating time steps from date/time objects
## 
## See below for concrete classes: e.g. Daily, Weekly
## 
## @param start First date or time in the first time step
## @param end Last date or time in the last time step
## @param checker Function generating objects of class "TimeCheck"
DateTimeSteps = function(start, end, checker) {
  self = Base()
  self$unit = "" ## abstract class so nothing meaningful like "Days"
  self$acceptable_time_classes = "" ## e.g. "Date"
  self$character_converter = function(x) stop("abstract class")
  self$start = start
  self$end = end
  self$checker = checker(self)
  self$bounds = function() c(self$start, self$end)
  self$bound_steps = function() c(1, self$final_step())
  self$final_step = function() self$time_id_engine(self$end)
  self$time_obj = function() Time(self$final_step())
  self$all_ending_times = function() {
    (self$final_step()
     |> seq_len()
     |> self$ending_time_engine()
    )
  }
  self$ending_times = function(time_steps) self$ending_time_engine(time_steps)
  self$time_ids = function(x, fixer = NULL) {
    time_steps = self$time_id_engine(x)
    if (is.function(fixer)) x = fixer(x)
    self$checker$assert_time_steps(x, time_steps)
  }
  self$frame_engine = function(x) {
    data.frame(
        time_step = self$time_id_engine(x)
      , ending_time = as.character(x)
    )
  }
  self$frame = function(x) {
    data.frame(
        time_step = self$time_ids(x)
      , ending_time = as.character(x)
    )
  }
  return_object(self, "DateTimeSteps")
}

Steps = function(start, end) {
  self = DateTimeSteps(start, end, checker = NoError)
  self$unit = 1L
  self$acceptable_time_classes = "integer"
  self$character_converter = as.integer
  self$time_id_engine = function(x) {
    as.integer(x) - self$start + 1
  }
  self$ending_time_engine = function(time_steps) time_steps
  return_object(self, "Steps")
}

Daily = function(start, end, checker = RangeError) {
  self = DateTimeSteps(start, end, checker)
  self$unit = "Day"
  self$acceptable_time_classes = "Date"
  self$character_converter = as.Date
  self$time_id_engine = function(x) {
    (x
     |> as.Date()
     |> difftime(as.Date(self$start), units = "days")
     |> round()
     |> as.numeric()
    ) + 1L
  }
  self$ending_time_engine = function(time_steps) {
    ## trying to avoid hard dependency on lubridate::days
    as.Date(as.integer(self$start) + time_steps - 1L)
  }
  self$checker$check_bounds()
  return_object(self, "Daily")
}

Weekly = function(start, end, checker = AllTimeErrors) {
  self = DateTimeSteps(start, end, checker)
  self$unit = "Week"
  self$acceptable_time_classes = "Date"
  self$character_converter = as.Date
  self$time_id_engine = function(x) {
    (x
     |> as.Date()
     |> difftime(as.Date(self$start), units = "weeks")
     |> as.numeric()
    ) + 1L
  }
  self$ending_time_engine = function(time_steps) {
    ## trying to avoid hard dependency on lubridate::weeks
    as.Date(as.integer(self$start) + (time_steps - 1L) * 7L)
  }
  self$checker$check_bounds()
  return_object(self, "Weekly")
}



TimeCheck = function(time) {
  self = Base()
  self$time = time
  self$too_early = function(time_steps) time_steps < 1L
  self$too_late = function(time_steps) time_steps > self$time$final_step()
  self$fractional = function(time_steps) {
    not = \(x) !x
    is_equal = Vectorize(all.equal, SIMPLIFY = FALSE)
    (time_steps
      |> round()
      |> is_equal(time_steps)
      |> vapply(isTRUE, logical(1L))
      |> not()
    )
  }
  self$wrong_time_class = function(x) {
    if (is.character(x)) x = self$time$character_converter(x)
    !(inherits(x, self$time$acceptable_time_classes))
  }
  self$clashing_steps = function(time_steps) {
    dups = time_steps[duplicated(time_steps)] |> unique()
    time_steps %in% dups
  }
  self$early_times = function(x, time_steps) x[self$too_early(time_steps)]
  self$late_times = function(x, time_steps) x[self$too_late(time_steps)]
  self$fractional_times = function(x, time_steps) x[self$fractional(time_steps)]
  self$clashing_times = function(x, time_steps) x[self$clashing_steps(time_steps)]
  #memoise_all(self)
  self$check_early = function(x, time_steps) {
    if (any(self$too_early(time_steps))) {
      msg_colon(
          msg(
              "The following times fell before the start time"
            , self$time$start
          )
        , msg_indent(self$early_times(x, time_steps))
      ) |> stop()
    }
  }
  self$check_late = function(x, time_steps) {
    if (any(self$too_late(time_steps))) {
      msg_colon(
          msg(
              "The following times fell after the end time"
            , self$time$end
          )
        , msg_indent(self$late_times(x, time_steps))
      ) |> stop()
    }
  }
  self$check_fractional = function(x, time_steps) {
    if (any(self$fractional(time_steps))) {
      msg_colon(
          msg("The following fractional time steps were found")
        , frame_formatter(
            self$time$frame_engine(self$fractional_times(x, time_steps))
          )
      ) |> stop()
    }
  }
  self$check_time_class = function(x) {
    if (self$wrong_time_class(x)) {
      msg_break(
          ""
        , msg_colon(
              msg("The classes of the time vector were")
            , msg_indent(class(x))
          )
        , msg_colon(
              msg("But none of these are in the following list of valid classes")
            , msg_indent(self$time$acceptable_time_classes)
          )
      ) |> stop()
    }
  }
  self$check_clashing_times = function(x, time_steps) {
    if (any(self$clashing_steps(time_steps))) {
      msg_colon(
          msg("The following times with duplicated time steps were found")
        , frame_formatter(
            self$time$frame_engine(self$clashing_times(x, time_steps))
          )
      ) |> stop()
    }
  }
  return_object(self, "TimeCheck")
}

NoError = function(time) {
  self = TimeCheck(time)
  self$assert_time_steps = function(x, time_steps) as.integer(time_steps)
  self$check_bounds = function() TRUE
  return_object(self, "NoError")
}
RangeError = function(time) {
  self = TimeCheck(time)
  self$assert_time_steps = function(x, time_steps) {
    self$check_early(x, time_steps)
    self$check_late(x, time_steps)
    self$check_time_class(x)
    self$check_clashing_times(x, time_steps)
    as.integer(time_steps)
  }
  self$check_bounds = function() {
    x = self$time$bounds()
    time_steps = self$time$bound_steps()
    self$check_early(x, time_steps)
    self$check_late(x, time_steps)
    self$check_time_class(x)
  }
  return_object(self, "RangeError")
}

AllTimeErrors = function(time) {
  self = TimeCheck(time)
  self$assert_time_steps = function(x, time_steps) {
    self$check_early(x, time_steps)
    self$check_late(x, time_steps)
    self$check_fractional(x, time_steps)
    self$check_time_class(x)
    self$check_clashing_times(x, time_steps)
    as.integer(time_steps)
  }
  self$check_bounds = function() {
    x = self$time$bounds()
    time_steps = self$time$bound_steps()
    self$check_early(x, time_steps)
    self$check_late(x, time_steps)
    self$check_fractional(x, time_steps)
    self$check_time_class(x)
  }
  return_object(self, "AllTimeErrors")
}

#' @export
print.DateTimeSteps = function(x, ...) {
  cls_nm = class(x) |> getElement(1L)
  msg_header(sprintf("%s Time Steps", cls_nm)) |> cat()
  msg_list(
      msg(x$unit, "1 ends on", x$start)
    , msg(x$unit, x$final_step(), "ends on", x$end)
  ) |> cat()
  msg_hline(end_char = "") |> cat()
}
