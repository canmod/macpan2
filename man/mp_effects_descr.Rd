% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_methods.R
\name{mp_effects_descr}
\alias{mp_effects_descr}
\alias{mp_add_effects_descr}
\title{Describe Statistical Effects}
\usage{
mp_effects_descr(model)

mp_add_effects_descr(coef_table, model)
}
\arguments{
\item{model}{A model in the TMB engine that can be used to compute
tables of statistical effects.}

\item{coef_table}{Coefficient table that was probably generated using
\code{\link{mp_tmb_coef}} or \code{\link{mp_tmbstan_coef}},
but also perhaps generated directly using the \code{tidy.TMB} or the
\code{tidy.stanfit} methods in the \code{broom.mixed} package.}
}
\description{
Additional information that can be joined to the output of
the \code{tidy.TMB} or \code{tidy.stanfit} functions in the \code{broom.mixed} package.
}
\section{Functions}{
\itemize{
\item \code{mp_add_effects_descr()}: Convenience function for adding coefficient
descriptions from a calibrated model to \code{coef_table}s generated by
\code{\link{mp_tmb_coef}} or \code{\link{mp_tmbstan_coef}}.

}}
