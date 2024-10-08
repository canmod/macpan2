% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_list_generators.R
\name{mp_expand}
\alias{mp_expand}
\title{Expand Model}
\usage{
mp_expand(model)
}
\arguments{
\item{model}{A model object.}
}
\description{
Expand a structured model so that it is represented in an unstructured
format requiring a more verbose description. Currently,
this is only applicable for \code{\link{mp_tmb_model_spec}} objects
that have explicit flows
(e.g. \code{\link{mp_per_capita_flow}}). For such models, \code{mp_expand}
produces a model with expression lists composed entirely of plain R
formulas.
}
\examples{
sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
print(sir)
print(mp_expand(sir))

}
