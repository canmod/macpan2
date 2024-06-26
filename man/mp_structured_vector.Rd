% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{mp_structured_vector}
\alias{mp_structured_vector}
\alias{mp_set_numbers}
\title{Structured Vectors}
\usage{
mp_structured_vector(x, ...)

mp_set_numbers(vector, ...)
}
\arguments{
\item{x}{An index.}

\item{...}{Passed on to S3 methods.}

\item{vector}{An index.}
}
\description{
This documentation was originally in \code{\link[=mp_index]{mp_index()}} and should be cleaned up
See issue #131. Also this is an experimental feature.
}
\section{Functions}{
\itemize{
\item \code{mp_set_numbers()}: Update numerical values of a structured
vector. TODO: details on syntax.

}}
\examples{
state = mp_index(
  Epi = c("S", "I", "S", "I"),
  Age = c("young", "young", "old", "old")
)
state_vector = (state
  |> mp_structured_vector()
  |> mp_set_numbers(Epi = c(S = 1000))
  |> mp_set_numbers(Epi = c(I = 1), Age = "old")
)
print(state_vector)

}
