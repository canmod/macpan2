% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmb_model.R
\name{mp_default}
\alias{mp_default}
\alias{mp_default_list}
\title{Default Values}
\usage{
mp_default(model, include_all = FALSE)

mp_default_list(model, include_all = FALSE)
}
\arguments{
\item{model}{A model object from which to extract default values. If
\code{model} is a calibrator object (see \code{\link{mp_tmb_calibrator}})
that has been optimized (using \code{\link{mp_optimize}}), then the values
returned by \code{mp_default} and \code{mp_default_list} are updated to reflect this
calibration/optimization process.}

\item{include_all}{Include all default variables, even those that are not
used in the \code{before}, \code{during}, or \code{after} phase of the simulations.
When \code{include_all} is \code{FALSE}, examples of excluded variables are
those used by an objective function only or those intended to be used in an
extended model specification produced using functions like
\code{\link{mp_tmb_insert}} and \code{\link{mp_tmb_update}}.}
}
\value{
A long-format data frame with default values for matrices required
as input to model objects. The columns of this output are \code{matrix}, \code{row},
\code{col}, and \code{value}. Scalar matrices do not have any entries in the \code{row} or
\code{col} columns.
}
\description{
Default Values
}
\section{Functions}{
\itemize{
\item \code{mp_default_list()}: List of the default variables as matrices.

}}
