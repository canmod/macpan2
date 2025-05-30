% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_list_generators.R
\name{mp_absolute_flow}
\alias{mp_absolute_flow}
\title{Specify Absolute Flow Between Compartments (Experimental)}
\usage{
mp_absolute_flow(from, to, rate, flow_name = NULL, rate_name = NULL)
}
\arguments{
\item{from}{String giving the name of the compartment from which the flow
originates.}

\item{to}{String giving the name of the compartment to which the flow is
going.}

\item{rate}{String giving the expression for the absolute
flow rate per time-step.}

\item{flow_name}{String giving the name for the variable that
will store the \code{rate}.}

\item{rate_name}{Deprecated synonym for \code{flow_name}. Please use \code{flow_name}
in all future work.}
}
\description{
An experimental alternative to \code{\link{mp_per_capita_flow}} that
allows users to specify flows using absolute rates instead of
per-capita rates.
}
\seealso{
\code{\link[=mp_per_capita_flow]{mp_per_capita_flow()}}
}
