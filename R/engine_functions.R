## Auto-generated - do not edit by hand

#' Engine Functions
#'
#' Functions currently supported by the C++ TMB engine
#' for constructing expressions for defining model
#' simulations.
#'
#' ## Elementwise Binary Operators
#'
#' Elementwise binary operators take two matrix-valued
#' arguments and apply a binary operator (e.g. `+`, `*`)
#' to each set of corresponding elements, and return the
#' corresponding matrix-valued output containing the
#' resulting elements. What does 'corresponding' mean? If
#' the two matrix-valued arguments have the same shape
#' (same number of rows and columns), then two elements
#' correspond if they occur in the same row and column
#' position in the two matrices. If the two matrices are
#' not of the same shape but there is either one row or
#' one column in either matrix, then the singleton rows
#' and then columns are repeated sufficiently many times
#' so that they match the shape of the other matrix. If
#' after repeating singleton rows and columns the
#' matrices are still of different shape, then an error
#' is thrown. Currently the following elementwise binary
#' operators are available: `+`, `-`, `*`, `/`, `^`.
#' ## Integer Sequences
#'
#' The colon operator works much like the base R version
#' \code{\link{:}}. It takes two scalar-valued integers
#' and returns a column vector with all integers between
#' the two inputs.
#'
#' The `seq` function works like the base R default
#' \code{\link{seq}} function, but only allows the
#' `from`, `to`, and `by` arguments. These arguments
#' must all be scalars.
#' ## Parenthesis
#'
#' The order of operations can be enforced in the usual
#' way with round parentheses, \code{\link{(}}.
#'
#' ## Combining Elements
#' TODO -- Before continuing with the roxygen
#' we need to group together related functions better.
#' For example we should have rbind and cbind up here
#' in this section.
#' @name engine_functions
NULL
