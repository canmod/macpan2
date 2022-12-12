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
#' ## Sequences and Repeated Patterns
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
#'
#' The \code{\link{rep}} function can be used to repeat
#' the elements of a scalar `n` times. The following
#' arguments are available.
#'
#' * `x` -- A scalar-valued variable to repeat.
#' * `times` -- A scalar-valued integer variable giving
#' the number of times to repeat `x`.
#'
#' The result is a column vector. This function differs
#' from its base R version in that `x` must be a scalar,
#' otherwise only the first element of `x` will be used.
#' TODO: Consider allowing generic `x`
#'
#' ## Matrix Multiplication
#'
#' Standard matrix multiplication, \code{\link{%*%}},
#' is available.
#' ## Parenthesis
#'
#' The order of operations can be enforced in the usual
#' way with round parentheses, \code{\link{(}}.
#'
#' ## Reshaping and Combining Matrices
#'
#' Any number of scalars can be combined into a column
#' vector using the \code{\link{c}} function. If
#' non-scalars are provided then only the element in
#' the first row and column of each input are used.
#' TODO: Consider modifying so that `c` works more like
#' the base R `c`, in that it stacks matrix columns.
#'
#' Column and row vectors of the same length can be
#' combined using the \code{\link{cbind}} and
#' \code{\link{rbind}} functions respectively
#'
#' The `matrix` function can be used to redefine the
#' numbers of rows and columns to use for arranging
#' the values of a matrix. It works similarly to
#' the base R \code{\link{matrix}} function in that it
#' takes the following arguments.
#'
#' * `data` -- A matrix to reshape.
#' * `nrow` -- An integer scalar giving the number of
#' rows in the output matrix.
#' * `ncol` -- An integer scalar giving the number of
#' columns in the output matrix.
#'
#' On the other hand, this function differs substantially
#' from the base R version in that it must be filled
#' by column and there is no `byrow` option.
#'
#' Matrices can be transposed with the usual
#' function, \code{\link{t}}.
#'
#' ## Summarizing Matrix Values
#'
#' The elements of a matrix can be summed together using
#' the standard \code{\link{sum}} function.
#'
#' The standard \code{\link{rowSums}} and
#' \code{\link{colSums}} can be used, but they have
#' slightly different behaviour from their base R
#' versions. In particular, the `rowSums` function
#' returns a column vector and the `colSums` function
#' returns a row vector. If a specific shape is required
#' then the transpose \code{\link{t}} function must be
#' explicitly used.
#'
#' ## Extracting Matrix Elements
#'
#' It is possible to extract a single element from a
#' matrix using square brackets. Two
#' indices must be supplied for both the row and column
#' positions. Note that zero-based indexing is used
#' so the first element gets index, `0`, etc. It is
#' currently not possible to extract sub-matrices
#' of arbitrary dimensions, but this GitHub issue
#' will address this shortcoming when it is completed,
#' \url{https://github.com/canmod/macpan2/issues/10}.
#'
#' ## Accessing Past Values in the Simulation History
#'
#' For matrices with their simulation history saved,
#' it is possible to bind the rows or columns of all of
#' the versions of such matrices into a single matrix.
#'
#' There are four versions of this functionality.
#'
#' * `rbind_lag(x, lag)` -- Bind the rows of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to time lags given
#' by integers in `lag`.
#' * `rbind_time(x, t)` -- Bind the rows of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to integers in
#' `t`.
#' * `cbind_lag(x, lag)` -- Bind the columns of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to time lags given
#' by integers in `lag`. (TODO -- cbind_lag is not developed yet)
#' * `cbind_time(x, t)` -- Bind the columns of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to integers in
#' `t`. (TODO -- cbind_lag is not developed yet)
#'
#' ## Convolution
#'
#' One may take the convolution of each element in a
#' matrix, x, over simulation time using a kernel, k.
#' There are two arguments of this function.
#'
#' * `x` -- The matrix to be convolved.
#' * `k` -- A column vector giving the convolution kernel.
#'
#' @name engine_functions
NULL
