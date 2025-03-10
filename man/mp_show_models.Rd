% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_model_index.R
\name{mp_show_models}
\alias{mp_show_models}
\alias{show_models}
\title{Print a Table of Existing Models}
\usage{
mp_show_models(
  dir = system.file("starter_models", package = "macpan2"),
  show_missing = FALSE,
  for_markdown = FALSE
)

show_models(
  dir = system.file("starter_models", package = "macpan2"),
  show_missing = FALSE,
  for_markdown = FALSE
)
}
\arguments{
\item{dir}{directory to list}

\item{show_missing}{(logical) include entries for models with no README information?}

\item{for_markdown}{(logical) format for rendering the table with markdown-formatted
links to model readme files?}
}
\value{
a data frame containing entries \code{Directory} (model directory), \code{Title} (model title), \code{Description} (short description)
}
\description{
Collects information from the headers of the README files in
model directories and returns the results as a data frame
}
\section{Functions}{
\itemize{
\item \code{show_models()}: Synonym for \code{mp_show_models}, which is
preferred. Present for back-compatibility.

}}
\examples{
mp_show_models(show_missing = TRUE)
}
