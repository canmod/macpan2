% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp_cal_time.R
\name{mp_sim_offset}
\alias{mp_sim_offset}
\title{Simulation Offsets}
\usage{
mp_sim_offset(
  sim_start_offset,
  sim_end_offset,
  time_scale = "steps",
  time_column = "time"
)
}
\arguments{
\item{sim_start_offset}{Number of time steps before the first data point
to start each simulation.}

\item{sim_end_offset}{Number of time steps after the last data point to
end each simulation.}

\item{time_scale}{Qualitative description of the size of a time step.
The only valid values are 'steps' and 'daily'. If you would like each time
step in the model to represent one day, then you should consider using
'daily'. Note that using 'daily' will require that your data represent
time using a (1) \code{\link{Date}} vector, (2) \code{\link{character}}
vector in YYYY-MM-DD format, or (3) \code{\link{integer}} vector that
counts the number of days since some reference. Otherwise please choose
'steps', the default, and convert your time column into integer values that
represent the time step that you would like in the model.}

\item{time_column}{Name of the column that will identify the time at which
particular values were observed.}
}
\value{
An object to be passed to the \code{time} argument of
\code{\link{mp_tmb_calibrator}}.
}
\description{
Offset the starting and ending times of the simulation, from the
start and end time of the data used in calibration. This is used to
override the default offsets of zero taken from the observed data passed to
\code{\link{mp_tmb_calibrator}}.
}
\seealso{
\code{\link[=mp_sim_bounds]{mp_sim_bounds()}}
}
