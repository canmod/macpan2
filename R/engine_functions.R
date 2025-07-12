## Auto-generated - do not edit by hand

#' Functions Available in the Simulation Engine
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
#' To produce a dynamical simulation that iteratively evaluates
#' expression involving these functions, use 
#' \code{\link{simple_sims}}.
#'
#' ```
#' simple_sims(
#'   iteration_exprs = list(x ~ x - 0.9 * x),
#'   time_steps = 5,
#'   mats = list(x = 1)
#' )
#' ```
#'
#' Here, `x - 0.9 * x` is assigned to `x` at each of five 
#' iterations of a simulation loop.
#' 
#' If these expressions involve matrices with more than one 
#' element, You can control which elements in the evaluation 
#' of the right hand side go to which elements on the left 
#' hand side. This technique involves using either square
#' brackets or the `c` function on the left hand side. For 
#' more information on assignment, please see the section
#' on Assignment below.
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
#' * `x` : Any matrix with dimensions compatible with `y`.
#' * `y` : Any matrix with dimensions compatible with `x`.
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
#' ## Elementwise Math
#'
#' ### Functions
#'
#' * `log(x)` : Natural logarithm.
#' * `exp(x)` : Exponential function.
#' * `cos(x)` : Cosine function.
#' * `sin(x)` : Sine function.
#' * `sqrt(x)` : Squareroot function.
#' * `invlogit(x)` : Inverse logit function, 
#' `1/(1 + exp(-x))`.
#' * `logit(x)` : Logit function, `log(x/(1-x))`.
#'
#' ### Arguments
#'
#' * `x` : Any numeric matrix.
#'
#' ### Return
#'
#' * A matrix with the same dimensions as `x`, containing
#' the results of applying the function to each element of `x`.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ log(y), y = c(2, 0.5))
#' ```
#'
#' ## Proportions
#'
#' ### Functions
#'
#' * `proportions(x, limit, eps)`
#'
#' ### Arguments
#'
#' * `x` : Any matrix
#' * `limit` : numeric value to return elementwise from `proportions` if `sum(x) < eps`
#' * `eps` : numeric tolerance for `sum(x)`
#'
#' ### Return
#'
#' * matrix of `x / sum(x)` or `rep(limit, length(x))` if 
#' `sum(x) < eps`.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ proportions(y, 0.5, 1e-8), y = c(2, 0.5))
#' ```
#' 
#' ## Integer Sequences
#'
#' ### Functions
#'
#' * `from:to` : Inclusive and ordered sequence of
#' integers between two bounds.
#' * `seq(from, length, by)` : Ordered sequence of
#' integers with equal spacing between adjacent
#' values.
#'
#' ### Arguments
#'
#' * `from` : Scalar integer giving the first integer
#' in the sequence.
#' * `to` : Scalar integer giving the last integer in
#' the sequence.
#' * `length` : Number of integers in the sequence.
#' * `by` : Scalar giving the difference
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
#' base R default, \code{\link[base]{seq}}, in that it
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
#' * `rep(x, times)` : Replicate a column vector a
#' number of times, by repeatedly stacking it on top of
#' itself.
#' * `recycle(x, rows, cols)` : Recycle rows and columns of
#' `x` to produce a matrix with `rows` rows and `cols` columns.
#'
#' ### Arguments
#'
#' * `x` : A scalar-valued variable to repeat.
#' * `times` : A scalar-valued integer variable giving
#' the number of times to repeat `x`.
#' * `rows` : Number of rows in the output of `recycle`.
#' * `cols` : Number of columns in the output of `recycle`.
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
#' engine_eval(~ recycle(  1:3,  3, 4))
#' engine_eval(~ recycle(t(1:4), 3, 4))
#' ```
#'
#' ## Matrix Multiplication
#'
#' ### Functions
#'
#' * `x %*% y` : Standard matrix multiplication.
#' * `x %x% y` : Kronecker product
#' * `sparse_mat_mult(x, i, j, y, z)` : Matrix multiplication
#' when the left matrix is represented as a column vector, `x`, 
#' of non-zero elements and integer vectors of row, `i`, and 
#' column, `j`, indices. The right matrix and the resulting
#' matrix are not represented as sparse matrices.
#'
#' ### Arguments
#'
#' * `x` : A matrix.
#' * `y` : A matrix.
#' * `i` : Integer vector the same length as `x` giving 
#' zero-based row indices for sparse matrix representation.
#' * `j` : Integer vector the same length as `x` giving 
#' zero-based column indices for sparse matrix representation.
#' * `z` : A matrix with dimensions equal to the result of
#' the sparse matrix multiplication (see details).
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
#' ### Details
#'
#' For standard matrix multiplication, `x %*% y`, the number of
#' columns of `x` equals the number of rows of `y`.
#' 
#' 
#' Think about `sparse_mat_mult(x, i, j, y, z)` as similar to
#' `z ~ x %*% y`, where `x` is represented differently. In
#' particular, the argument `x` is a column vector containing the 
#' non-zero elements of the left matrix, `i` contains the 
#' zero-based row indices associated with each element in `x`,
#' and `j` contains the zero-based column indices associated with
#' each element in `x`.
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
#' * `c(...)` : Stack columns of arguments into a
#' single column vector.
#' * `cbind(...)` : Create a matrix containing all of
#' the columns of a group of matrices with the same
#' number of rows.
#' * `rbind(...)` : Create a matrix containing all of
#' the rows of a group of matrices with the same number
#' of columns.
#' * `matrix(x, rows, cols)` : Reshape a matrix to have
#' `rows` rows and `cols` columns. The input `x` must
#' have `rows * cols` elements.
#' * `t(x)` : Standard matrix transpose.
#'
#' ### Arguments
#'
#' * `...` : Any number of dimensionally consistent
#' matrices. The definition of dimensionally consistent
#' depends on the function.
#' * `x` : Can be any matrix for `t`, but for `matrix`
#' it must have `rows * cols` elements.
#' * `rows` : Scalar integer giving the number of
#' rows in the output.
#' * `cols` : Scalar integer giving the number of
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
#' the base R \code{\link[base]{matrix}} function in that it
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
#' * `to_diag(x)` : Create a diagonal matrix by setting
#' the diagonal to a column vector, `x`.
#' * `from_diag(x)` : Extract the diagonal from a
#' matrix, `x`, and return the diagonal as a column
#' vector.
#'
#' ### Arguments
#'
#' * `x` : Any matrix (for `from_diag`) or a
#' column vector (for `to_diag`). It is common to assume
#' that `x` is square for `from_diag` but this is
#' not required.
#'
#' ### Return
#'
#' * `to_diag(x)` : Diagonal matrix with `x` on the
#' diagonal.
#' * `from_diag(x)` : Column vector containing the
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
#' * `sum(...)` : Sum all of the elements of all of the
#' matrices passed to `...`.
#' * `col_sums(x)` : Row vector containing the sums
#' of each column.
#' * `row_sums(x)` : Column vector containing the sums
#' of each row.
#' * `group_sums(x, f, n)` : Column vector containing the
#' sums of groups of elements in `x`. The groups are
#' determined by the integers in `f` and the order of
#' the sums in the output is determined by these
#' integers.
#' * `mean(x)` : Arthmetic average of all elements in matrix
#' `x`.
#' * `sd(x)` : Sample standard deviation of all elements in
#' matrix `x`.
#'
#' ### Arguments
#'
#' * `...` : Any number of matrices of any shape.
#' * `x` : A matrix of any dimensions, except for
#' `group_sums` that expects `x` to be a column vector.
#' * `f` : A column vector the same length as `x`
#' containing integers between `0` and `m-1`, given `m`
#' unique groups. Elements of `f` refer to the indices
#' of `x` that will be grouped and summed.
#' * `n` : A column vector of length `m`. If `f` does
#' not contain group `k` in `[0, m-1]`, `group_sums` skips
#' this group and the output at index `k+1` is `n[k+1]`.
#'
#' ### Return
#'
#' * A matrix containing sums of subsets of the inputs.
#'
#' ### Details
#'
#' The `row_sums` and `col_sums` are similar to the base R
#' \code{\link{rowSums}} and \code{\link{colSums}} functions,
#' but with slightly different behaviour. In particular, the 
#' `row_sums` function returns a column vector and the 
#' `col_sums` function returns a row vector. If a specific shape 
#' is required then the transpose \code{\link{t}} function must 
#' be explicitly used.
#'
#' ### Examples
#'
#' ```
#' x = 1
#' y = 1:3
#' A = matrix(1:12, 4, 3)
#' engine_eval(~ sum(y), y = y)
#' engine_eval(~ sum(x, y, A), x = x, y = y, A = A)
#' engine_eval(~ col_sums(A), A = A)
#' engine_eval(~ row_sums(A), A = A)
#' engine_eval(~ group_sums(x, f, n), x = 1:10, f = rep(0:3, 1:4), n = c(1:4))
#' ```
#'
#' ## Sweeping Matrix Elements
#'
#' ### Functions
#'
#' * `cumsum(x)` : Return a matrix with columns containing the
#' cumulative sum of the columns in `x`.
#'
#' ### Arguments
#'
#' * `x` : A matrix.
#'
#' ### Return
#' 
#' A matrix the same size as `x` but with columns containing the
#' cumulative sum of the columns in `x`.
#' ## Extracting Matrix Elements
#'
#' ### Functions
#'
#' * `x[i,j]` : Return a matrix containing a subset
#' of the rows and columns of `x`.
#' * `block(x,i,j,n,m)` : Return a matrix containing a
#' contiguous subset of rows and columns of `x`
#' \url{https://eigen.tuxfamily.org/dox/group__TutorialBlockOperations.html}.
#' * `last(x)` : The last element of a matrix (i.e., the
#' lower-right element).
#'
#' ### Arguments
#'
#' * `x` : Any matrix.
#' * `i` : An integer column vector (for `[`) or
#' integer scalar (for `block`) containing the indices
#' of the rows to extract (for `[`) or the index of the
#' first row to extract (for `block`).
#' * `j` : An integer column vector (for `[`) or
#' integer scalar (for `block`) containing the indices
#' of the columns to extract (for `[`) or the index of
#' the first column to extract (for `block`). If `j` is missing
#' in a call to `[`, it is assumed to be `j = 0` although
#' we might change this default to be the vector of all column
#' indices.
#' * `n` : Number of rows in the block to return.
#' * `m` : Number of columns in the block to return.
#'
#' ### Return
#'
#' * A matrix containing a subset of the rows and columns
#' in `x`.
#'
#' ### Details
#'
#' Note that zero-based indexing is used
#' so the first row/column gets index, `0`, etc. The `block`
#' function is expected to be more efficient than `[` when
#' the elements to be extracted are contiguous.
#'
#' ### Examples
#'
#' ```
#' engine_eval(~ A[c(3, 1, 2), 2], A = matrix(1:12, 4, 3))
#' engine_eval(~ block(x,i,j,n,m), x = matrix(1:12, 4, 3), i=1, j=1, n=2, m=2)
#' engine_eval(~ last(A), A = matrix(1:12, 4, 3))
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
#' * `rbind_lag(x, lag, t_min)` : Bind the rows of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to time lags given
#' by integers in `lag`.
#' * `rbind_time(x, t, t_min)` : Bind the rows of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to integers in
#' `t`.
#' * `cbind_lag(x, lag, t_min)` : Bind the columns of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to time lags given
#' by integers in `lag`. (TODO -- cbind_lag is not developed yet)
#' * `cbind_time(x, t, t_min)` : Bind the columns of versions of
#' `x` that were recorded at the end of all
#' simulation iterations corresponding to integers in
#' `t`. (TODO -- cbind_lag is not developed yet)
#'
#' ### Arguments
#'
#' * `x` : Any matrix with saved history such that the
#' number of columns (for `rbind_*`) or rows (for
#' `cbind_*`) does not change throughout the simulation.
#' * `lag` : Integer vector giving numbers
#' of time steps before the current step to obtain
#' past values of `x`.
#' * `t` : Integer vector giving time steps
#' at which to obtain past values of `x`.
#' * `t_min` : Integer giving the minimum time step
#' that is allowed to be accessed. All time-steps in `t`
#' or implied by `lag` that are before `t_min` are ignored.
#'
#' ### Return
#'
#' * A matrix containing values of `x` from past times.
#'
#' ## Time Indexing
#'
#' Get or update the index of the current or lagged 
#' time step or the index of the current time group. 
#' A time group is a contiguous set of time steps 
#' defined by two change points.
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
#' * `time_var(x, change_points)`: An improvement
#' to `time_group`. Returns values in `x`
#' at time steps in `change_points`, return value
#' remains constant between `change_points`.
#'
#' ### Arguments
#'
#' * `x`: Column vector representing a time series.
#' `time_var` will return the value of `x` corresponding
#' to element in `change_points` that contains the
#' current time.
#' * `lag`: Number of time-steps to look back for
#' the time-step to return.
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
#'   mats = list(x = empty_matrix)
#' )
#' sims = simple_sims(
#'   iteration_exprs = list(
#'     j ~ time_group(j, change_points),
#'     time_varying_parameter ~ time_variation_schedule[j]
#'   ),
#'   mats = list(
#'     j = 0,
#'     change_points = c(0, 4, 7),
#'     time_variation_schedule = c(42, pi, sqrt(2)),
#'     time_varying_parameter = empty_matrix
#'   ),
#'   time_steps = 10,
#' )
#' set.seed(1L)
#' change_points = c(0,2,5)
#' x_val = rnorm(length(change_points))
#' simple_sims(
#'     iteration_exprs = list(x ~ time_var(x_val,change_points))
#'   , int_vecs = list(change_points = change_points)
#'   , mats = list(x = empty_matrix, x_val=x_val)
#'   , time_steps = 10
#' )
#' ```
#'
#' ## Convolution
#'
#' You can take the convolution of each element in a
#' matrix, x, over simulation time using a kernel, k.
#'
#' ### Functions
#'
#' * `convolution(x, k)`
#'
#' ### Arguments
#'
#' * `x` : The matrix containing elements to be
#' convolved.
#' * `k` : A column vector giving the convolution kernel.
#'
#' ### Return
#'
#' A matrix the same size as `x` but with the
#' convolutions, \eqn{y_{ij}}, of each element,
#' \eqn{x_{ij}}. The value of \eqn{y_{ij}} at time 
#' \eqn{t = 1, ..., T} is given by the following.
#'
#' \deqn{y_{ij}(t) = \sum_{\tau = 0}^{min(t,m)-1} x_{ij}(t-\tau) k_\tau}
#' 
#' Where:
#' 
#' * \eqn{x_{ij}(t)} : value of \eqn{x_{ij}} at time step \eqn{t}.
#' * \eqn{y_{ij}(t)} : value of \eqn{y_{ij}} at time step \eqn{t}.
#' * \eqn{t = 1, ..., T} : the time step.
#' * \eqn{\tau = 0, ..., m - 1} : index of the 
#' time lag for a kernel of length \eqn{m}.
#' * \eqn{k_\tau} : value of the kernel associated with lag
#' \eqn{\tau}.
#'
#' ### Details
#'
#' If any empty matrices are encountered when looking
#' back in time, they are treated as matrices with all
#' zeros. The convolution of a matrix of all positive
#' values will be biased low for all time steps less than
#' the length of the kernel (i.e., for all time steps 
#' such that \eqn{t < m}), and therefore one should only
#' compare observed data with a convolution (e.g., when
#' calibrating) for time steps less than \eqn{m}.
#'
#' ### Examples
#' 
#' ```
#' simple_sims(
#'   list(
#'     x ~ 3 * x * (1 - x),
#'     y ~ convolution(x, rep(1/10, 10))
#'   ),
#'   time_steps = 50,
#'   mats = list(x = 0.5, y = empty_matrix)
#' )
#' ```
#' 
#' ## Clamp
#'
#' Smoothly clamp the elements of a matrix so that they
#' do not get closer to 0 than a tolerance, `eps`, with
#' a default of 1e-12. This `clamp` function is the following 
#' modification of the 
#' [squareplus function](https://arxiv.org/abs/2112.11687).
#'
#' \deqn{f(x) = \epsilon_- + \frac{(x - \epsilon_-) + \sqrt{(x - \epsilon_-)^2 + (2\epsilon_0 - \epsilon_-)^2 - \epsilon_-^2}}{2}}
#' 
#' Where the two parameters are defined as follows.
#'
#' \deqn{\epsilon_0 = f(0)}
#' 
#' \deqn{\epsilon_- = \lim_{x \to  -\infty}f(x)}
#' 
#' This function is differentiable everywhere, monotonically
#' increasing, and \eqn{f(x) \approx x} if \eqn{x} is positive
#' and not too close to zero. By modifying the parameters, you 
#' can control the distance between \eqn{f(x)} and the
#' horizontal axis at two 'places' -- \eqn{0} and \eqn{-\infty}.
#' [See issue #93](https://github.com/canmod/macpan2/issues/93).
#' for more information.
#'
#' ### Functions
#'
#' * `clamp(x, eps, limit)`
#'
#' ### Arguments
#'
#' * `x` : A matrix with elements that should remain positive.
#' * `eps` : A small positive number, \eqn{\epsilon_0 = f(0)},
#' giving the value of the function when the input is zero.
#' The default value is 1e-11
#' * `limit` : A small positive number, 
#' \deqn{\epsilon_- = \lim_{x \to  -\infty}f(x)}, giving the
#' value of the function as the input goes to negative
#' infinity. The default is `limit = 1e-12`. This `limit` 
#' should be chosen to be less than `eps` to ensure that 
#' `clamp` is twice differentiable.
#' 
#' 
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
#' * `dpois(observed, simulated)` : Log of the Poisson density
#' based on this [dpois](https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaa1ed15503e1441a381102a8c4c9baaf1)
#' TMB function.
#' * `dnbinom(observed, simulated, over_dispersion)` :
#' Log of the negative binomial density based on this [dnbinom](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga76266c19046e04b651fce93aa0810351)
#' TMB function. To get the variance that this function
#' requires we use this expression, \code{simulated + simulated^2/over_dispersion},
#' following p.165 in this [book](https://ms.mcmaster.ca/~bolker/emdbook/book.pdf)
#' * `dnorm(observed, simulated, standard_deviation)` :
#' Log of the normal density based on this [dnorm](https://kaskr.github.io/adcomp/dnorm_8hpp.html)
#' TMB function.
#' * `dbinom(observed, size, probability)` :
#' Log of the binomial density based on the [dbinom](https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaee11f805f02bc1febc6d7bf0487671be)
#' TMB function.
#'
#' ### Arguments
#'
#' * `observed` : Matrix of observed values
#' at which the density is being evaluated.
#' * `simulated` : Matrix of distributional means,
#' with singleton rows and columns recycled to match
#' the numbers of rows and columns in `observed`.
#' * `over_dispersion` : Over-dispersion parameter
#' given by \code{(simulated/standard_deviation)^2 - simulated)}.
#' * `standard_deviation` : Standard deviation parameter.
#' * `size` : Number of Bernoulli trials.
#' * `probability` : Probability of a successful Bernoulli trial.
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
#' * `rpois(mean)` : Pseudo-random Poisson distributed
#' values.
#' * `rnbinom(mean, over_dispersion)` : Pseudo-random
#' negative binomially distributed values.
#' * `rnorm(mean, standard_deviation)` : Pseudo-random
#' normal values.
#' * `rbinom(size, prob)` : Pseudo-random binomial values.
#' * `reulermultinom(size, rate, dt)` : Pseudo-random
#' [Euler-multinomial](https://kingaa.github.io/manuals/pomp/html/eulermultinom.html)
#' values.
#'
#' ### Arguments
#'
#' * `mean` : Matrix of means about which to simulate
#' pseudo-random variation.
#' * `over_dispersion` : Matrix of over-dispersion parameters
#' given by \code{(simulated/standard_deviation)^2 - simulated)}.
#' * `standard_deviation` : Matrix of standard deviation
#' parameters.
#' * `size` : Matrix of numbers of trials.
#' * `prob` : Matrix of probabilities of success.
#' * `rate` : Matrix of rates, used to compute the probabilities
#' in a multinomial distribution. The probability associated with
#' the `i`th rate, `r_i`, is (1 - exp(-sum(r * dt))) * (r_i / r),
#' where `r` is the sum of the rates. This is not a typical
#' multinomial distribution in that if you sum these
#' probabilities up you do not get `1` but rather 
#' `(1 - exp(-sum(r * dt)))`. See details below for more
#' on the Euler-multinomial distribution
#' * `dt` : Optional parameter specifying the length of the time
#' step. See details below for more on the Euler-multinomial
#' distribution.
#'
#' ### Details
#' 
#' The Euler-multinomial distribution is used to model how
#' many individuals move from one compartment to a set of other
#' compartments in a single time step of length `dt`. The rate of 
#' moving to each of these compartments is characterized by the 
#' associated element in the `rate` matrix. The reason why the
#' probabilities do not sum to `1`, is that not all individuals
#' have to change compartments in a time step.
#' 
#' ## Cumulative Distribution Functions
#'
#' Lower-tail cumulative distribution functions.
#' 
#' ### Functions
#'
#' * `pgamma(q, shape, scale)` : Cumulative distribution function
#' of the Gamma distribution. This is a lite wrapper for the
#' [pgamma function in TMB](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga3bd06a324f89b21694aac26bfe1aef45).
#' * `pnorm(q, mean, sd)` : Cumulative distribution function of
#' the normal distribution. This is a lite wrapper for the 
#' [pnorm function in TMB](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga2a3cc5a06500670aeaf6eb911a1094d9).
#' 
#' ### Arguments
#'
#' * `q` : Matrix of Quantiles.
#' * `shape` : Matrix of shape parameters of the Gamma distribution.
#' * `scale` : Matrix of scale parameters of the Gamma distribution.
#' * `mean` : Matrix of mean parameters of the normal distribution.
#' * `sd` : Matrix of standard deviation parameters of the normal distribution.
#'
#' ## Rounding
#' 
#' ### Functions
#' 
#' round(x) : Round elements of a matrix to the nearest integer.
#' 
#' ### Arguments
#' 
#' * `x` : Matrix to be rounded.
#'
#' ### Details
#'
#' Be careful if you are using rounding in a model to be calibrated.
#' Rounding will break differentiability of the objective function
#' if `x` depends, either directly or indirectly, on parameters 
#' being calibrated. This will lead to incorrect gradients
#' potentially being passed to an optimizer. To be safe, do
#' not round in models being calibrated.
#' 
#' ## Debugging Instrumentation
#' 
#' Functions to use when you are trying to figure stuff out.
#' 
#' ### Functions
#'
#' * `print(x)` : Print out the value of a matrix.
#' * `check_finite(x)` : Stop the simulations and return an
#' error if `x` has any non-finite values.
#'
#' ### Arguments
#'
#' * `x` : Name of a matrix in the model.
#'
#' ### Return
#'
#' An \code{\link{empty_matrix}}.
#'
#' ### Examples
#'
#' ```
#' simple_sims(
#'      list(dummy ~ print(x), x ~ x / 2)
#'    , time_steps = 10
#'    , mats = list(x = 2)
#' )
#' engine_eval(~ 1/0) ## returns Inf
#' engine_eval(~ check_finite(1/0)) ## returns nothing and throws an error
#' ```
#' 
#' ## Assign (deprecated)
#'
#' Assign values to a subset of the elements in a matrix.
#'
#' ### Functions
#'
#' * `assign(x, i, j, v)`
#'
#' ### Arguments
#'
#' * `x` : Matrix with elements that are to be updated
#' by the values in `v`.
#' * `i` : Column vector of row indices pointing to
#' the elements of `x` to be updated. These indices are
#' paired with those in `v`. If the length of
#' `i` does not equal that of `v`, then it must have a
#' single index that gets paired with every element of
#' `v`. Indices are zero-based, `i=0` corresponds to 
#' the first row.
#' * `j` : Column vector of column indices pointing to
#' the elements of `x` to be updated. These indices are
#' paired with those in `v`. If the length of
#' `j` does not equal that of `v`, then it must have a
#' single index that gets paired with every element of
#' `v`. Indices are zero-based, `j=0` corresponds to
#' the first column.
#' * `v` : Column vector of values to replace elements
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
#' ## Unpack (deprecated)
#'
#' Unpack elements of a matrix into smaller matrices.
#'
#' ### Functions
#'
#' * `unpack(x, ...)`
#'
#' ### Arguments
#'
#' * `x` : Matrix with elements to be distributed to
#' the matrices passed through `...`.
#' * `...` : Matrices with elements to be replaced by
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
#' ## Assignment
#'
#' The left-hand-side of formulas sent to the simulation engine
#' determine assignment works.
#' 
#' ### Functions
#'
#' * `y ~ x` : Assign `x` to `y`.
#' * `y[i] ~ x` : Assign the first column of `x` to those rows
#' in the first column of `y` that are indexed by `i`.
#' * `y[i, j] ~ x` : Assign each element, `x[k, l]`, in `x`,
#' to element, `y[i[k], j[l]]`, in `y`.
#' * `c(...) ~ x` : Assign the elements of the columns of `x`
#' (stacked on top of each other) to the matrices in `...` in the 
#' order in which they appear. If the number of columns is `x` 
#' equals the number of matrices in `...`, and if these matrices
#' are vectors (i.e., have only a single column or a single row),
#' then the columns of `x` become assigned to the vectors in `...`.
#' 
#' ### Arguments
#' 
#' * `x` : Matrix containing the result of the expression on the
#' right-hand-side.
#' * `y` : Matrix with elements that will be assigned the elements
#' of `x`.
#' * `i` : Integer vector giving zero-based row indexes describing
#' the rows in `x` that get the 
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
#' @aliases row_sums
#' @aliases col_sums
#' @aliases group_sums
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
#' @aliases print
#' @aliases time_var
#' @aliases rbinom
#' @aliases reulermultinom
#' @aliases round
#' @aliases pgamma
#' @aliases mean
#' @aliases sd
#' @aliases proportions
#' @aliases last
#' @aliases check_finite
#' @aliases dbinom
#' @aliases sin
#' @aliases sqrt
#' @aliases pnorm
#' @aliases invlogit
#' @aliases logit
#' @aliases cumsum
#' @aliases sparse_mat_mult
#' @aliases assign
#' @aliases unpack
NULL
