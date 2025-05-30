% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp_tmb_objective.R
\name{mp_tmb_objective}
\alias{mp_tmb_objective}
\title{Value of the Objective Function of a Model}
\usage{
mp_tmb_objective(
  model,
  parameter_updates = list(),
  baseline = c("recommended", "default", "optimized")
)
}
\arguments{
\item{model}{A model with an objective function, probably one produced using
\code{\link{mp_tmb_calibrator}}.}

\item{parameter_updates}{Named list of a subset of model variables with
the values to use when simulating the trajectory using the
\code{mp_trajectory_par} function. In the future we plan
to allow this variable to be a data frame with one row for each scalar value
(which would be useful if only certain elements of a vector
or matrix are parameters) and a string giving the name of a file containing
parameter information. But for now, only a list is allowed.}

\item{baseline}{Models can contain several alternative sets of
parameters, and this \code{baseline} argument is used to choose which of these
should be updated using the \code{parameter_updates} passed to
\code{mp_trajectory_par}. The current options are \code{"recommended"}, \code{"optimized"},
and \code{"default"}. The \code{"recommended"} option will be used if neither of the
other two options are selected. If \code{model} is capable of being optimized
(e.g., it was created using \code{\link{mp_tmb_calibrator}}) then
\code{"recommended"} is equivalent to \code{"optimized"}, which use the best set of
parameters found by \code{\link{mp_optimize}}. If \code{\link{mp_optimize}}
has not yet been called on \code{model} then a warning will be issued. If
\code{model} is not capable of being optimized then \code{"recommended"} is
equivalent to \code{"default"}, which uses the original set of parameters
available when \code{model} was created.}
}
\description{
Value of the Objective Function of a Model
}
