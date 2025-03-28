% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmb_model_editors.R
\name{mp_tmb_insert_trans}
\alias{mp_tmb_insert_trans}
\title{Insert Basic Transformations of Model Variables}
\usage{
mp_tmb_insert_trans(model, variables = character(), transformation = mp_log)
}
\arguments{
\item{model}{TMB model spec object produced using
\code{\link{mp_tmb_library}} or \code{\link{mp_tmb_model_spec}}.}

\item{variables}{Character vector of variables to transform.}

\item{transformation}{A transformation object such as \code{\link{mp_log}},
which is the default. See the help page for \code{\link{mp_log}} for
available options.}
}
\value{
A new model specification object with expressions for the transformed
variables at the end of the simulation loop. The transformed variables
are identified with a prefixed name (e.g., \code{log_incidence} if \code{incidence}
is log transformed).
}
\description{
Insert Basic Transformations of Model Variables
}
\seealso{
\code{\link[=mp_tmb_insert_backtrans]{mp_tmb_insert_backtrans()}}
}
