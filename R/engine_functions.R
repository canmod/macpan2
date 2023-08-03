## Auto-generated - do not edit by hand

#' Engine Functions
#'
#' Functions currently supported by the C++ TMB engine
#' for constructing expressions for defining model
#' simulations.
#'
#' The quickest way to experiment with these functions is
#' to use the \code{\link{engine_eval}} function, as in the
#' following example that calculates a force of infection.
#'
#' ```
#' engine_eval(~ beta * I / N
#'   , beta = 0.25
#'   , I = 1e3
#'   , N = 1e7
#' )
#' ```
#'
#' To produce a simulation using these functions, one may
#' use \code{\link{simple_sims}}.
#'
#' ```
#' simple_sims(
#'   iteration_exprs = list(x ~ x - 0.9 * x),
#'   time_steps = 5,
#'   x = 1
#' )
#' ```
#'
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
#' not of the same shape but there is one row and/or
#' one column in either matrix, then the singleton rows
#' and columns are recycled sufficiently many times
#' so that they match the shape of the other matrix. If
#' after recycling singleton rows and columns the
#' matrices are still of different shape, then an error
#' is thrown and the matrices are said to be incompatible.
#'
#' ### Functions
#'
#' * `x + y`
#' * `x - y`
#' * `x * y`
#' * `x / y`
#' * `x ^ y`
#'
#'
#' ### Arguments
#'
#' * `x` -- Any matrix with dimensions compatible with `y`.
#' * `y` -- Any matrix with dimensions compatible with `x`.
#'
#' ### Return
#'
#' * A matrix with the binary operator applied elementwise
#' after any necessary recycling of rows and/or columns.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ 1 + 2)
#' engine_eval(~ y * z, y = 1:3, z = matrix(1:6, 3, 2))
#' engine_eval(~ 1 / (1 - y), y = 1/4)
#' ```
#'
#' ## Unary Elementwise Math
#'
#' ### Functions
#'
#' * `log(x)` -- Natural logarithm
#' * `exp(x)` -- Exponential function
#' * `cos(x)` -- Cosine function
#'
#' ### Arguments
#'
#' * `x` -- Any matrix
#'
#' ### Return
#'
#' * A matrix with the same dimensions as `x`, with the
#' unary function applied elementwise.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ log(y), y = c(2, 0.5))
#' ```
#'
#' ## Integer Sequences
#'
#' ### Functions
#'
#' * `from:to` -- Inclusive and ordered sequence of
#' integers between two bounds.
#' * `seq(from, length, by)` -- Ordered sequence of
#' integers with equal spacing between adjacent
#' values.
#'
#' ### Arguments
#'
#' * `from` -- Scalar integer giving the first integer
#' in the sequence.
#' * `to` -- Scalar integer giving the last integer in
#' the sequence.
#' * `length` -- Number of integers in the sequence.
#' * `by` -- Scalar giving the difference
#' between adjacent values in the sequence.
#'
#' ### Return
#'
#' * Column vector with a sequence of integers.
#'
#' ### Details
#'
#' The colon operator works much like the base R version
#' \code{\link{:}}. It takes two scalar-valued integers
#' and returns a column vector with all integers between
#' the two inputs.
#'
#' The `seq` function is a little different from the
#' base R default, \code{\link{seq}}, in that it
#' allows the user precise control over the length of
#' the output through the `length` argument. The
#' base R function gives the user this option, but not
#' as the default.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ 1:10)
#' engine_eval(~ seq(1, 10, 2))
#' ```
#'
#' Replicate Elements
#'
#' ### Functions
#'
#' * `rep(x, times)` -- Replicate a column vector a
#' number of times, by repeatedly stacking it on top of
#' itself.
#' * `rep_each` -- Not yet developed.
#' * `rep_length` -- Not yet developed.
#'
#' ### Arguments
#'
#' * `x` -- A scalar-valued variable to repeat.
#' * `times` -- A scalar-valued integer variable giving
#' the number of times to repeat `x`.
#'
#' ### Return
#'
#' * Column vector with `times` copies of `x` stacked
#' on top of each other.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ rep(1, 10))
#' ```
#'
#' ## Matrix Multiplication
#'
#' ### Functions
#'
#' * `x %*% y` -- Standard matrix multiplication.
#' * `x %x% y` -- Kronecker product
#'
#' ### Arguments
#'
#' * `x` -- A matrix. For the standard product, `x`
#' must have as many columns as `y` has rows.
#' * `y` -- A matrix. For standard product, `y`
#' must have as many rows as `x` has columns.
#'
#' ### Return
#'
#' * The matrix product of `x` and `y`.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ (1:10) %*% t(1:10))
#' engine_eval(~ (1:10) %x% t(1:10))
#' ```
#'
#' ## Parenthesis
#'
#' The order of operations can be enforced in the usual
#' way with round parentheses, \code{\link{(}}.
#'
#' ## Reshaping and Combining Matrices
#'
#' ### Functions
#'
#' * `c(...)` -- Stack columns of arguments into a
#' single column vector.
#' * `cbind(...)` -- Create a matrix containing all of
#' the columns of a group of matrices with the same
#' number of rows.
#' * `rbind(...)` -- Create a matrix containing all of
#' the rows of a group of matrices with the same number
#' of columns.
#' * `matrix(x, rows, cols)` -- Reshape a matrix to have
#' `rows` rows and `cols` columns. The input `x` must
#' have `rows * cols` elements.
#' * `t(x)` -- Standard matrix transpose.
#'
#' ### Arguments
#'
#' * `...` -- Any number of dimensionally consistent
#' matrices. The definition of dimensionally consistent
#' depends on the function.
#' * `x` -- Can be any matrix for `t`, but for `matrix`
#' it must have `rows * cols` elements.
#' * `rows` -- Scalar integer giving the number of
#' rows in the output.
#' * `cols` -- Scalar integer giving the number of
#' columns in the output.
#'
#' ### Return
#'
#' * A combined or reshaped matrix.
#'
#' ### Details
#'
#' Any number of column vectors can be combined into a
#' bigger column vector.
#'
#' Column and row vectors of the same length can be
#' combined using the \code{\link{cbind}} and
#' \code{\link{rbind}} functions respectively
#'
#' The `matrix` function can be used to redefine the
#' numbers of rows and columns to use for arranging
#' the values of a matrix. It works similarly to
#' the base R \code{\link{matrix}} function in that it
#' takes the same arguments.
#' On the other hand, this function differs substantially
#' from the base R version in that it must be filled
#' by column and there is no `byrow` option.
#'
#' Matrices can be transposed with the usual
#' function, \code{\link{t}}.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ c(a, b, c), a = 1, b = 10:13, c = matrix(20:25, 3, 2))
#' engine_eval(~ cbind(a, 10 + a), a = 0:3)
#' engine_eval(~ rbind(a, 10 + a), a = t(0:3))
#' engine_eval(~ matrix(1:12, 4, 3))
#' engine_eval(~ t(1:3))
#' ```
#'
#' ## Matrix Diagonals
#'
#' ### Functions
#'
#' * `to_diag(x)` -- Create a diagonal matrix by setting
#' the diagonal to a column vector, `x`.
#' * `from_diag(x)` -- Extract the diagonal from a
#' matrix, `x`, and return the diagonal as a column
#' vector.
#'
#' ### Arguments
#'
#' * `x` -- Any matrix (for `from_diag`) or a
#' column vector (for `to_diag`). It is common to assume
#' that `x` is square for `from_diag` but this is
#' not required.
#'
#' ### Return
#'
#' * `to_diag(x)` -- Diagonal matrix with `x` on the
#' diagonal.
#' * `from_diag(x)` -- Column vector containing the
#' diagonal of `x`. A value is considered to be on
#' the diagonal if it has a row index equal to
#' the column index.
#'
#' ### Details
#'
#' The `to_diag` function can be used to produce a
#' diagonal matrix by setting a column vector equal
#' to the desired diagonal. The `from_diag` does
#' (almost) the opposite, which is to get a column vector
#' containing the diagonal of an existing matrix.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~from_diag(matrix(1:9, 3, 3)))
#' engine_eval(~to_diag(from_diag(matrix(1:9, 3, 3))))
#' engine_eval(~from_diag(to_diag(from_diag(matrix(1:9, 3, 3)))))
#' ```
#'
#' ## Summarizing Matrix Values
#'
#' ### Functions
#'
#' * `sum(...)` -- Sum all of the elements of all of the
#' matrices passed to `...`.
#' * `colSums(x)` -- Row vector containing the sums
#' of each column.
#' * `rowSums(x)` -- Column vector containing the sums
#' of each row.
#' * `groupSums(x, f, n)` -- Column vector containing the
#' sums of groups of elements in `x`. The groups are
#' determined by the integers in `f` and the order of
#' the sums in the output is determined by these
#' integers.
#'
#' ### Arguments
#'
#' * `...` -- Any number of matrices of any shape.
#' * `x` -- A matrix of any dimensions, except for
#' `groupSums` that expects `x` to be a column vector.
#' * `f` -- A column vector the same length as `x`
#' containing integers between `0` and `n-`.
#' * `n` -- Length of the output column vector.
#'
#' ### Return
#'
#' * A matrix containing sums of various groups of
#' the elements of `x`.
#'
#' ### Details
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
#' ### Examples
#'
#' ```
#' x = 1
#' y = 1:3
#' A = matrix(1:12, 4, 3)
#' engine_eval(~ sum(y), y = y)
#' engine_eval(~sum(x, y, A), x = x, y = y, z = z)
#' engine_eval(~ colSums(A), A = A)
#' engine_eval(~ rowSums(A), A = A)
#' engine_eval(~ groupSums(x, f, n), x = 1:10, f = rep(0:3, 1:4), n = 4)
#' ```
#'
#' ## Extracting Matrix Elements
#'
#' ### Functions
#'
#' * `x[i,j]` -- Matrix containing a subset
#' of the rows and columns of `x`.
#' * `block(x,i,j,n,m)` -- Matrix containing a
#' contiguous subset of rows and columns of `x`
#' \url{https://eigen.tuxfamily.org/dox/group__TutorialBlockOperations.html}
#'
#' ### Arguments
#'
#' * `x` -- Any matrix.
#' * `i` -- An integer column vector (for `[`) or
#' integer scalar (for `block`) containing the indices
#' of the rows to extract (for `[`) or the index of the
#' first row to extract (for `block`).
#' * `j` -- An integer column vector (for `[`) or
#' integer scalar (for `block`) containing the indices
#' of the columns to extract (for `[`) or the index of
#' the first column to extract (for `block`).
#' * `n` -- Number of rows in the block to return.
#' * `m` -- Number of columns in the block to return.
#'
#' ### Return
#'
#' * A matrix containing a subset of the rows and columns
#' in `x`.
#'
#' ### Details
#'
#' Note that zero-based indexing is used
#' so the first row/column gets index, `0`, etc.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ A[c(3, 1, 2), 2], A = matrix(1:12, 4, 3))
#' ```
#'
#' ## Accessing Past Values in the Simulation History
#'
#' For matrices with their simulation history saved,
#' it is possible to bind the rows or columns of past
#' versions of such matrices into a single matrix.
#'
#' ### Functions
#'
#' * `rbind_lag(x, lag, t_min)` -- Bind the rows of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to time lags given
#' by integers in `lag`.
#' * `rbind_time(x, t, t_min)` -- Bind the rows of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to integers in
#' `t`.
#' * `cbind_lag(x, lag, t_min)` -- Bind the columns of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to time lags given
#' by integers in `lag`. (TODO -- cbind_lag is not developed yet)
#' * `cbind_time(x, t, t_min)` -- Bind the columns of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to integers in
#' `t`. (TODO -- cbind_lag is not developed yet)
#'
#' ### Arguments
#'
#' * `x` -- Any matrix with saved history such that the
#' number of columns (for `rbind_*`) or rows (for
#' `cbind_*`) does not change throughout the simulation.
#' * `lag` -- Column vector of integers giving numbers
#' of time steps before the current step to obtain
#' past values of `x`.
#' * `t` -- Column vector of integers giving time steps
#' at which to obtain past values of `x`.
#' * `t_min` -- Minimum time step that is allowed to be
#' accessed. All time-steps in `t` or implied by `lag`
#' that are before `t_min` are ignored.
#'
#' ### Return
#'
#' * A matrix containing values of `x` from past times.
#'
#' ## Time Indexing
#'
#' Get the index of current or lagged time step or
#' the index of the current time group. A time group
#' is a contiguous set of time steps defined by two
#' change points.
#'
#' ### Functions
#'
#' * `time_step(lag)`: Get the time-step associated
#' with a particular lag from the current time-step.
#' If the lagged time-step is less than zero, the
#' function returns zero.
#' * `time_group(index, change_points)`: Update the
#' `index` associated with the current time group.
#' The current group is defined by the minimum
#' of all elements of `change_points` that are
#' greater than the current time step. The time group
#' `index` is the index associated with this element.
#' Please see the examples below, they are easier
#' to understand than this explanation.
#'
#' ### Arguments
#'
#' * `lag`: Number of time-steps to look back for
#' the time-step to return.
#' * `index`: Index associated with the current time
#' group.
#' * `change_points`: Increasing column vector of
#' time steps giving the lower bound of each time
#' group.
#'
#' ### Return
#'
#' A 1-by-1 matrix with the time-step `lag` steps
#' ago, or with zero if `t+1 < lag`
#'
#' ### Examples
#'
#' ```
#' simple_sims(
#'   iteration_exprs = list(x ~ time_step(0)),
#'   time_steps = 10,
#'   x = empty_matrix
#' )
#' sims = simple_sims(
#'   iteration_exprs = list(
#'     j ~ time_group(j, change_points),
#'     time_varying_parameter ~ time_variation_schedule[j]
#'   ),
#'   time_steps = 10,
#'   j = 0,
#'   change_points = c(0, 4, 7),
#'   time_variation_schedule = c(42, pi, sqrt(2)),
#'   time_varying_parameter = empty_matrix
#' )
#' ```
#'
#' ## Convolution
#'
#' One may take the convolution of each element in a
#' matrix, x, over simulation time using a kernel, k.
#' There are two arguments of this function.
#'
#' ### Functions
#'
#' * `convolution(x, k)`
#'
#' ### Arguments
#'
#' * `x` -- The matrix containing elements to be
#' convolved.
#' * `k` -- A column vector giving the convolution kernel.
#'
#' ### Return
#'
#' A matrix the same size as `x` but with the
#' convolutions, \eqn{y_{ij}}, of each element,
#' \eqn{x_{ij}}, given by the following.
#'
#' \deqn{y_{ij} = \sum_{\tau = 0} x_{ij}(t-\tau) k(\tau)}
#'
#' unless \eqn{t < \tau}, in which case,
#'
#' \deqn{y_{ij} = }
#'
#' where \eqn{y_{ij}} is the convolution,
#' \eqn{x_{ij}(t)} is the value of \eqn{x_{ij}} at time step, \eqn{t},
#' \eqn{k(\tau)} is the value of the kernel at lag, \eqn{\tau},
#' and \eqn{\lambda} is the length of the kernel.
#'
#' ### Details
#'
#' If any empty matrices are encountered when looking
#' back in time, they are treated as matrices with all
#' zeros. Similarly, any matrices encounte
#' of `x`
#'
#' ## Clamp
#'
#' Smoothly clamp the elements of a matrix so that they
#' do not get closer to 0 than a tolerance, `eps`, with
#' a default of 1e-12. The output of the `clamp`
#' function is as follows.
#'
#' ### Functions
#'
#' * `clamp(x, eps)`
#'
#' ### Arguments
#'
#' * `x` : A matrix with elements that should remain positive.
#' * `eps` : A small positive number giving the
#' theoretical minimum of the elements in the returned
#' matrix.
#' ## Probability Densities
#'
#' All probability densities have the same first two
#' arguments.
#'
#' * `observed`
#' * `simulated`
#'
#' The `simulated` argument gives a matrix of means for
#' the `observed` values at which the densities are
#' being evaluated. Additional arguments are other
#' distributional parameters such as the standard
#' deviation or dispersion parameter. All densities
#' are given as log-densities, so if you would like
#' the density itself you must pass the result through
#' the `exp` function.
#'
#' If the `simulated` matrix or the additional parameter
#' matrices have either a single row or
#' single column, these singleton rows and columns are
#' repeated to match the number of rows and columns in
#' the `observed` matrix. This feature allows one
#' to do things like specify a single common mean for
#' several values.
#'
#' ### Functions
#'
#' * `dpois(observed, simulated)` -- Log of the Poisson density
#' based on this [dpois](https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaa1ed15503e1441a381102a8c4c9baaf1)
#' TMB function.
#' * `dnbinom(observed, simulated, over_dispersion)` --
#' Log of the negative binomial density based on this [dnbinom](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga76266c19046e04b651fce93aa0810351)
#' TMB function. To get the variance that this function
#' requires we use this expression, \code{simulated + simulated^2/over_dispersion},
#' following p.165 in this [book](https://ms.mcmaster.ca/~bolker/emdbook/book.pdf)
#' * `dnorm(observed, simulated, standard_deviation)` --
#' Log of the normal density based on this [dnorm](https://kaskr.github.io/adcomp/dnorm_8hpp.html)
#' TMB function.
#'
#' ### Arguments
#'
#' * `observed` -- Matrix of observed values
#' at which the density is being evaluated.
#' * `simulated` -- Matrix of distributional means,
#' with singleton rows and columns recycled to match
#' the numbers of rows and columns in `observed`.
#' * `over_dispersion` -- Over-dispersion parameter
#' given by \code{(simulated/standard_deviation)^2 - simulated)}.
#' * `standard_deviation` -- Standard deviation parameter.
#'
#' ## Pseudo-Random Number Generators
#'
#' All random number generator functions have `mean`
#' as the first argument. Subsequent arguments give
#' additional distributional parameters.
#' Singleton rows and columns in the matrices passed to
#' the additional distributional parameters are recycled
#' so that all arguments have the same number of rows
#' and columns. All functions return a matrix the same
#' shape as `mean` but with pseudo-random numbers
#' deviating from each mean in the `mean` matrix.
#'
#' ### Functions
#'
#' * `rpois(mean)` -- Pseudo-random Poisson distributed
#' values.
#' * `rnbinom(mean, over_dispersion)` -- Pseudo-random
#' negative binomially distributed values.
#' * `rnorm(mean, standard_deviation)` -- Pseudo-random
#' normal values.
#'
#' ### Arguments
#'
#' * `mean` -- Matrix of means about which to simulate
#' pseudo-random variation.
#' * `over_dispersion` -- Matrix of over-dispersion parameters
#' given by \code{(simulated/standard_deviation)^2 - simulated)}.
#' * `standard_deviation` -- Matrix of standard deviation parameters.
#'
#' ## Assign
#'
#' Assign values to a subset of the elements in a matrix.
#'
#' ### Functions
#'
#' * `assign(x, i, j, v)`
#'
#' ### Arguments
#'
#' * `x` -- Matrix with elements that are to be updated
#' by the values in `v`.
#' * `i` -- Column vector of row indices pointing to
#' the elements of `x` to be updated. These indices are
#' paired with those in `v`. If the length of
#' `i` does not equal that of `v`, then it must have a
#' single index that gets paired with every element of
#' `v`.
#' * `j` -- Column vector of column indices pointing to
#' the elements of `x` to be updated. These indices are
#' paired with those in `v`. If the length of
#' `j` does not equal that of `v`, then it must have a
#' single index that gets paired with every element of
#' `v`.
#' * `v` -- Column vector of values to replace elements
#' of `x` at locations given by `i` and `j`.
#'
#' ### Return
#'
#' The `assign` function is not called for its return
#' value, which is an \code{\link{empty_matrix}}, but
#' rather to modify `x` but replacing some of its
#' components with those in `v`.
#'
#' ### Examples
#'
#' ```
#' x = matrix(1:12, 3, 4)
#' engine_eval(~ x + 1, x = x)
#' engine_eval(~ x + 1, x = x, .matrix_to_return = "x")
#' engine_eval(~ assign(x, 2, 1, 100), x = x, .matrix_to_return = "x")
#' engine_eval(~ assign(x
#'   , c(2, 1, 0)
#'   , 0
#'   , c(100, 1000, 10000)
#' ), x = x, .matrix_to_return = "x")
#'
#' ```
#'
#' ## Unpack
#'
#' Unpack elements of a matrix into smaller matrices.
#'
#' ### Functions
#'
#' * `unpack(x, ...)`
#'
#' ### Arguments
#'
#' * `x` -- Matrix with elements to be distributed to
#' the matrices passed through `...`.
#' * `...` -- Matrices with elements to be replaced by
#' the values of elements in `x` in column-major order.
#' These matrices must be named matrices and not
#' computed on the fly using expressions. Note that even
#' subsetting (e.g. `unpack(x, y[0], y[3])`) counts as
#' an expression. This use-case would require the
#' \code{\link{assign}} function
#' `assign(y, c(0, 3), 0, x)`.
#'
#' ### Return
#'
#' The `unpack` function is not called for its return
#' value, which is an \code{\link{empty_matrix}}, but
#' rather to modify the matrices in `...` by replacing
#' at least some of its components with those in `x`.
#'
#' ### Examples
#'
#' Here we fill a matrix with integers from `1` to `12`
#' and then unpack them one-at-a-time into two
#' column vectors, `x` and `y`. By returning `y`
#' we see the integers after the first three were
#' used up by `x`.
#' ```
#' engine_eval(~unpack(matrix(1:12, 3, 4), x, y)
#'   , x = rep(0, 3)
#'   , y = rep(1, 5)
#'   , .matrix_to_return = "y"
#' )
#' ```
#'
#' @name engine_functions
#' @aliases `+`
#' @aliases `-`
#' @aliases `*`
#' @aliases `/`
#' @aliases `^`
#' @aliases exp
#' @aliases log
#' @aliases `(`
#' @aliases c
#' @aliases matrix
#' @aliases `%*%`
#' @aliases sum
#' @aliases rep
#' @aliases rowSums
#' @aliases colSums
#' @aliases groupSums
#' @aliases `[`
#' @aliases block
#' @aliases t
#' @aliases rbind_time
#' @aliases rbind_lag
#' @aliases cbind_time
#' @aliases cbind_lag
#' @aliases `:`
#' @aliases seq
#' @aliases convolution
#' @aliases cbind
#' @aliases rbind
#' @aliases time_step
#' @aliases assign
#' @aliases unpack
#' @aliases recycle
#' @aliases clamp
#' @aliases dpois
#' @aliases dnbinom
#' @aliases dnorm
#' @aliases rpois
#' @aliases rnbinom
#' @aliases rnorm
#' @aliases `%x%`
#' @aliases to_diag
#' @aliases from_diag
#' @aliases time_group
#' @aliases cos
NULL
