% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engine_functions.R
\name{engine_functions}
\alias{engine_functions}
\alias{`+`}
\alias{`-`}
\alias{`*`}
\alias{`/`}
\alias{`^`}
\alias{exp}
\alias{log}
\alias{`(`}
\alias{c}
\alias{matrix}
\alias{`\%*\%`}
\alias{sum}
\alias{rep}
\alias{row_sums}
\alias{col_sums}
\alias{group_sums}
\alias{`[`}
\alias{block}
\alias{t}
\alias{rbind_time}
\alias{rbind_lag}
\alias{cbind_time}
\alias{cbind_lag}
\alias{`:`}
\alias{seq}
\alias{convolution}
\alias{cbind}
\alias{rbind}
\alias{time_step}
\alias{assign}
\alias{unpack}
\alias{recycle}
\alias{clamp}
\alias{dpois}
\alias{dnbinom}
\alias{dnorm}
\alias{rpois}
\alias{rnbinom}
\alias{rnorm}
\alias{`\%x\%`}
\alias{to_diag}
\alias{from_diag}
\alias{time_group}
\alias{cos}
\alias{print}
\alias{time_var}
\alias{rbinom}
\alias{reulermultinom}
\alias{round}
\alias{pgamma}
\alias{mean}
\alias{sd}
\alias{proportions}
\title{Engine Functions}
\description{
Functions currently supported by the C++ TMB engine
for constructing expressions for defining model
simulations.
}
\details{
The quickest way to experiment with these functions is
to use the \code{\link{engine_eval}} function, as in the
following example that calculates a force of infection.

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ beta * I / N
  , beta = 0.25
  , I = 1e3
  , N = 1e7
)
}\if{html}{\out{</div>}}

To produce a simulation using these engine functions, one may
use \code{\link{simple_sims}}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{simple_sims(
  iteration_exprs = list(x ~ x - 0.9 * x),
  time_steps = 5,
  mats = list(x = 1)
)
}\if{html}{\out{</div>}}
\subsection{Elementwise Binary Operators}{

Elementwise binary operators take two matrix-valued
arguments and apply a binary operator (e.g. \code{+}, \code{*})
to each set of corresponding elements, and return the
corresponding matrix-valued output containing the
resulting elements. What does 'corresponding' mean? If
the two matrix-valued arguments have the same shape
(same number of rows and columns), then two elements
correspond if they occur in the same row and column
position in the two matrices. If the two matrices are
not of the same shape but there is one row and/or
one column in either matrix, then the singleton rows
and columns are recycled sufficiently many times
so that they match the shape of the other matrix. If
after recycling singleton rows and columns the
matrices are still of different shape, then an error
is thrown and the matrices are said to be incompatible.
\subsection{Functions}{
\itemize{
\item \code{x + y}
\item \code{x - y}
\item \code{x * y}
\item \code{x / y}
\item \code{x ^ y}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Any matrix with dimensions compatible with \code{y}.
\item \code{y} -- Any matrix with dimensions compatible with \code{x}.
}
}

\subsection{Return}{
\itemize{
\item A matrix with the binary operator applied elementwise
after any necessary recycling of rows and/or columns.
}
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ 1 + 2)
engine_eval(~ y * z, y = 1:3, z = matrix(1:6, 3, 2))
engine_eval(~ 1 / (1 - y), y = 1/4)
}\if{html}{\out{</div>}}
}

}

\subsection{Unary Elementwise Math}{
\subsection{Functions}{
\itemize{
\item \code{log(x)} -- Natural logarithm
\item \code{exp(x)} -- Exponential function
\item \code{cos(x)} -- Cosine function
\item \code{proportions(x, limit, eps)} -- matrix of \code{x / sum(x)} or \code{rep(limit, length(x))} if \code{sum(x) < eps}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Any matrix
\item \code{limit} -- numeric value to return elementwise from \code{proportions} if \code{sum(x) < eps}
\item \code{eps} -- numeric tolerance for \code{sum(x)}
}
}

\subsection{Return}{
\itemize{
\item A matrix with the same dimensions as \code{x}, with the
unary function applied elementwise.
}
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ log(y), y = c(2, 0.5))
}\if{html}{\out{</div>}}
}

}

\subsection{Integer Sequences}{
\subsection{Functions}{
\itemize{
\item \code{from:to} -- Inclusive and ordered sequence of
integers between two bounds.
\item \code{seq(from, length, by)} -- Ordered sequence of
integers with equal spacing between adjacent
values.
}
}

\subsection{Arguments}{
\itemize{
\item \code{from} -- Scalar integer giving the first integer
in the sequence.
\item \code{to} -- Scalar integer giving the last integer in
the sequence.
\item \code{length} -- Number of integers in the sequence.
\item \code{by} -- Scalar giving the difference
between adjacent values in the sequence.
}
}

\subsection{Return}{
\itemize{
\item Column vector with a sequence of integers.
}
}

\subsection{Details}{

The colon operator works much like the base R version
\code{\link{:}}. It takes two scalar-valued integers
and returns a column vector with all integers between
the two inputs.

The \code{seq} function is a little different from the
base R default, \code{\link[base]{seq}}, in that it
allows the user precise control over the length of
the output through the \code{length} argument. The
base R function gives the user this option, but not
as the default.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ 1:10)
engine_eval(~ seq(1, 10, 2))
}\if{html}{\out{</div>}}

Replicate Elements
}

\subsection{Functions}{
\itemize{
\item \code{rep(x, times)} -- Replicate a column vector a
number of times, by repeatedly stacking it on top of
itself.
\item \code{rep_each} -- Not yet developed.
\item \code{rep_length} -- Not yet developed.
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- A scalar-valued variable to repeat.
\item \code{times} -- A scalar-valued integer variable giving
the number of times to repeat \code{x}.
}
}

\subsection{Return}{
\itemize{
\item Column vector with \code{times} copies of \code{x} stacked
on top of each other.
}
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ rep(1, 10))
}\if{html}{\out{</div>}}
}

}

\subsection{Matrix Multiplication}{
\subsection{Functions}{
\itemize{
\item \code{x \%*\% y} -- Standard matrix multiplication.
\item \code{x \%x\% y} -- Kronecker product
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- A matrix. For the standard product, \code{x}
must have as many columns as \code{y} has rows.
\item \code{y} -- A matrix. For standard product, \code{y}
must have as many rows as \code{x} has columns.
}
}

\subsection{Return}{
\itemize{
\item The matrix product of \code{x} and \code{y}.
}
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ (1:10) \%*\% t(1:10))
engine_eval(~ (1:10) \%x\% t(1:10))
}\if{html}{\out{</div>}}
}

}

\subsection{Parenthesis}{

The order of operations can be enforced in the usual
way with round parentheses, \code{\link{(}}.
}

\subsection{Reshaping and Combining Matrices}{
\subsection{Functions}{
\itemize{
\item \code{c(...)} -- Stack columns of arguments into a
single column vector.
\item \code{cbind(...)} -- Create a matrix containing all of
the columns of a group of matrices with the same
number of rows.
\item \code{rbind(...)} -- Create a matrix containing all of
the rows of a group of matrices with the same number
of columns.
\item \code{matrix(x, rows, cols)} -- Reshape a matrix to have
\code{rows} rows and \code{cols} columns. The input \code{x} must
have \code{rows * cols} elements.
\item \code{t(x)} -- Standard matrix transpose.
}
}

\subsection{Arguments}{
\itemize{
\item \code{...} -- Any number of dimensionally consistent
matrices. The definition of dimensionally consistent
depends on the function.
\item \code{x} -- Can be any matrix for \code{t}, but for \code{matrix}
it must have \code{rows * cols} elements.
\item \code{rows} -- Scalar integer giving the number of
rows in the output.
\item \code{cols} -- Scalar integer giving the number of
columns in the output.
}
}

\subsection{Return}{
\itemize{
\item A combined or reshaped matrix.
}
}

\subsection{Details}{

Any number of column vectors can be combined into a
bigger column vector.

Column and row vectors of the same length can be
combined using the \code{\link{cbind}} and
\code{\link{rbind}} functions respectively

The \code{matrix} function can be used to redefine the
numbers of rows and columns to use for arranging
the values of a matrix. It works similarly to
the base R \code{\link[base]{matrix}} function in that it
takes the same arguments.
On the other hand, this function differs substantially
from the base R version in that it must be filled
by column and there is no \code{byrow} option.

Matrices can be transposed with the usual
function, \code{\link{t}}.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ c(a, b, c), a = 1, b = 10:13, c = matrix(20:25, 3, 2))
engine_eval(~ cbind(a, 10 + a), a = 0:3)
engine_eval(~ rbind(a, 10 + a), a = t(0:3))
engine_eval(~ matrix(1:12, 4, 3))
engine_eval(~ t(1:3))
}\if{html}{\out{</div>}}
}

}

\subsection{Matrix Diagonals}{
\subsection{Functions}{
\itemize{
\item \code{to_diag(x)} -- Create a diagonal matrix by setting
the diagonal to a column vector, \code{x}.
\item \code{from_diag(x)} -- Extract the diagonal from a
matrix, \code{x}, and return the diagonal as a column
vector.
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Any matrix (for \code{from_diag}) or a
column vector (for \code{to_diag}). It is common to assume
that \code{x} is square for \code{from_diag} but this is
not required.
}
}

\subsection{Return}{
\itemize{
\item \code{to_diag(x)} -- Diagonal matrix with \code{x} on the
diagonal.
\item \code{from_diag(x)} -- Column vector containing the
diagonal of \code{x}. A value is considered to be on
the diagonal if it has a row index equal to
the column index.
}
}

\subsection{Details}{

The \code{to_diag} function can be used to produce a
diagonal matrix by setting a column vector equal
to the desired diagonal. The \code{from_diag} does
(almost) the opposite, which is to get a column vector
containing the diagonal of an existing matrix.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~from_diag(matrix(1:9, 3, 3)))
engine_eval(~to_diag(from_diag(matrix(1:9, 3, 3))))
engine_eval(~from_diag(to_diag(from_diag(matrix(1:9, 3, 3)))))
}\if{html}{\out{</div>}}
}

}

\subsection{Summarizing Matrix Values}{
\subsection{Functions}{
\itemize{
\item \code{sum(...)} -- Sum all of the elements of all of the
matrices passed to \code{...}.
\item \code{col_sums(x)} -- Row vector containing the sums
of each column.
\item \code{row_sums(x)} -- Column vector containing the sums
of each row.
\item \code{group_sums(x, f, n)} -- Column vector containing the
sums of groups of elements in \code{x}. The groups are
determined by the integers in \code{f} and the order of
the sums in the output is determined by these
integers.
}
}

\subsection{Arguments}{
\itemize{
\item \code{...} -- Any number of matrices of any shape.
\item \code{x} -- A matrix of any dimensions, except for
\code{group_sums} that expects \code{x} to be a column vector.
\item \code{f} -- A column vector the same length as \code{x}
containing integers between \code{0} and \code{m-1}, given \code{m}
unique groups. Elements of \code{f} refer to the indices
of \code{x} that will be grouped and summed.
\item \code{n} -- A column vector of length \code{m}. If \code{f} does
not contain group \code{k} in \verb{[0, m-1]}, \code{group_sums} skips
this group and the output at index \code{k+1} is \code{n[k+1]}.
}
}

\subsection{Return}{
\itemize{
\item A matrix containing sums of subsets of the inputs.
}
}

\subsection{Details}{

The \code{row_sums} and \code{col_sums} are similar to the base R
\code{\link{rowSums}} and \code{\link{colSums}} functions,
but with slightly different behaviour. In particular, the
\code{row_sums} function returns a column vector and the
\code{col_sums} function returns a row vector. If a specific shape
is required then the transpose \code{\link{t}} function must
be explicitly used.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{x = 1
y = 1:3
A = matrix(1:12, 4, 3)
engine_eval(~ sum(y), y = y)
engine_eval(~ sum(x, y, A), x = x, y = y, A = A)
engine_eval(~ col_sums(A), A = A)
engine_eval(~ row_sums(A), A = A)
engine_eval(~ group_sums(x, f, n), x = 1:10, f = rep(0:3, 1:4), n = c(1:4))
}\if{html}{\out{</div>}}
}

}

\subsection{Extracting Matrix Elements}{
\subsection{Functions}{
\itemize{
\item \code{x[i,j]} -- Matrix containing a subset
of the rows and columns of \code{x}.
\item \code{block(x,i,j,n,m)} -- Matrix containing a
contiguous subset of rows and columns of \code{x}
\url{https://eigen.tuxfamily.org/dox/group__TutorialBlockOperations.html}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Any matrix.
\item \code{i} -- An integer column vector (for \code{[}) or
integer scalar (for \code{block}) containing the indices
of the rows to extract (for \code{[}) or the index of the
first row to extract (for \code{block}).
\item \code{j} -- An integer column vector (for \code{[}) or
integer scalar (for \code{block}) containing the indices
of the columns to extract (for \code{[}) or the index of
the first column to extract (for \code{block}).
\item \code{n} -- Number of rows in the block to return.
\item \code{m} -- Number of columns in the block to return.
}
}

\subsection{Return}{
\itemize{
\item A matrix containing a subset of the rows and columns
in \code{x}.
}
}

\subsection{Details}{

Note that zero-based indexing is used
so the first row/column gets index, \code{0}, etc.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~ A[c(3, 1, 2), 2], A = matrix(1:12, 4, 3))
engine_eval(~ block(x,i,j,n,m), x = matrix(1:12, 4, 3), i=1, j=1, n=2, m=2)
}\if{html}{\out{</div>}}
}

}

\subsection{Accessing Past Values in the Simulation History}{

For matrices with their simulation history saved,
it is possible to bind the rows or columns of past
versions of such matrices into a single matrix.
\subsection{Functions}{
\itemize{
\item \code{rbind_lag(x, lag, t_min)} -- Bind the rows of versions of
\code{x} that were recorded at the end of all
simulation iterations corresponding to time lags given
by integers in \code{lag}.
\item \code{rbind_time(x, t, t_min)} -- Bind the rows of versions of
\code{x} that were recorded at the end of all
simulation iterations corresponding to integers in
\code{t}.
\item \code{cbind_lag(x, lag, t_min)} -- Bind the columns of versions of
\code{x} that were recorded at the end of all
simulation iterations corresponding to time lags given
by integers in \code{lag}. (TODO -- cbind_lag is not developed yet)
\item \code{cbind_time(x, t, t_min)} -- Bind the columns of versions of
\code{x} that were recorded at the end of all
simulation iterations corresponding to integers in
\code{t}. (TODO -- cbind_lag is not developed yet)
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Any matrix with saved history such that the
number of columns (for \verb{rbind_*}) or rows (for
\verb{cbind_*}) does not change throughout the simulation.
\item \code{lag} -- Integer vector giving numbers
of time steps before the current step to obtain
past values of \code{x}.
\item \code{t} -- Integer vector giving time steps
at which to obtain past values of \code{x}.
\item \code{t_min} -- Integer giving the minimum time step
that is allowed to be accessed. All time-steps in \code{t}
or implied by \code{lag} that are before \code{t_min} are ignored.
}
}

\subsection{Return}{
\itemize{
\item A matrix containing values of \code{x} from past times.
}
}

}

\subsection{Time Indexing}{

Get or update the index of the current or lagged
time step or the index of the current time group.
A time group is a contiguous set of time steps
defined by two change points.
\subsection{Functions}{
\itemize{
\item \code{time_step(lag)}: Get the time-step associated
with a particular lag from the current time-step.
If the lagged time-step is less than zero, the
function returns zero.
\item \code{time_group(index, change_points)}: Update the
\code{index} associated with the current time group.
The current group is defined by the minimum
of all elements of \code{change_points} that are
greater than the current time step. The time group
\code{index} is the index associated with this element.
Please see the examples below, they are easier
to understand than this explanation.
\item \code{time_var(x, change_points)}: An improvement
to \code{time_group}. Returns values in \code{x}
at time steps in \code{change_points}, return value
remains constant between \code{change_points}.
}
}

\subsection{Arguments}{
\itemize{
\item \code{x}: Column vector representing a time series.
\code{time_var} will return the value of \code{x} corresponding
to element in \code{change_points} that contains the
current time.
\item \code{lag}: Number of time-steps to look back for
the time-step to return.
\item \code{change_points}: Increasing column vector of
time steps giving the lower bound of each time
group.
}
}

\subsection{Return}{

A 1-by-1 matrix with the time-step \code{lag} steps
ago, or with zero if \code{t+1 < lag}
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{simple_sims(
  iteration_exprs = list(x ~ time_step(0)),
  time_steps = 10,
  mats = list(x = empty_matrix)
)
sims = simple_sims(
  iteration_exprs = list(
    j ~ time_group(j, change_points),
    time_varying_parameter ~ time_variation_schedule[j]
  ),
  mats = list(
    j = 0,
    change_points = c(0, 4, 7),
    time_variation_schedule = c(42, pi, sqrt(2)),
    time_varying_parameter = empty_matrix
  ),
  time_steps = 10,
)
set.seed(1L)
change_points = c(0,2,5)
x_val = rnorm(length(change_points))
simple_sims(
    iteration_exprs = list(x ~ time_var(x_val,change_points))
  , int_vecs = list(change_points = change_points)
  , mats = list(x = empty_matrix, x_val=x_val)
  , time_steps = 10
)
}\if{html}{\out{</div>}}
}

}

\subsection{Convolution}{

One may take the convolution of each element in a
matrix, x, over simulation time using a kernel, k.
There are two arguments of this function.
\subsection{Functions}{
\itemize{
\item \code{convolution(x, k)}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- The matrix containing elements to be
convolved.
\item \code{k} -- A column vector giving the convolution kernel.
}
}

\subsection{Return}{

A matrix the same size as \code{x} but with the
convolutions, \eqn{y_{ij}}, of each element,
\eqn{x_{ij}}, given by the following.

\deqn{y_{ij} = \sum_{\tau = 0} x_{ij}(t-\tau) k(\tau)}

unless \eqn{t < \tau}, in which case,

\deqn{y_{ij} = }

where \eqn{y_{ij}} is the convolution,
\eqn{x_{ij}(t)} is the value of \eqn{x_{ij}} at time step, \eqn{t},
\eqn{k(\tau)} is the value of the kernel at lag, \eqn{\tau},
and \eqn{\lambda} is the length of the kernel.
}

\subsection{Details}{

If any empty matrices are encountered when looking
back in time, they are treated as matrices with all
zeros. Similarly, any matrices encounte
of \code{x}
}

}

\subsection{Clamp}{

Smoothly clamp the elements of a matrix so that they
do not get closer to 0 than a tolerance, \code{eps}, with
a default of 1e-12. The output of the \code{clamp}
function is as follows.
\subsection{Functions}{
\itemize{
\item \code{clamp(x, eps)}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} : A matrix with elements that should remain positive.
\item \code{eps} : A small positive number giving the
theoretical minimum of the elements in the returned
matrix.
}
}

}

\subsection{Probability Densities}{

All probability densities have the same first two
arguments.
\itemize{
\item \code{observed}
\item \code{simulated}
}

The \code{simulated} argument gives a matrix of means for
the \code{observed} values at which the densities are
being evaluated. Additional arguments are other
distributional parameters such as the standard
deviation or dispersion parameter. All densities
are given as log-densities, so if you would like
the density itself you must pass the result through
the \code{exp} function.

If the \code{simulated} matrix or the additional parameter
matrices have either a single row or
single column, these singleton rows and columns are
repeated to match the number of rows and columns in
the \code{observed} matrix. This feature allows one
to do things like specify a single common mean for
several values.
\subsection{Functions}{
\itemize{
\item \code{dpois(observed, simulated)} -- Log of the Poisson density
based on this \href{https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaa1ed15503e1441a381102a8c4c9baaf1}{dpois}
TMB function.
\item \code{dnbinom(observed, simulated, over_dispersion)} --
Log of the negative binomial density based on this \href{https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga76266c19046e04b651fce93aa0810351}{dnbinom}
TMB function. To get the variance that this function
requires we use this expression, \code{simulated + simulated^2/over_dispersion},
following p.165 in this \href{https://ms.mcmaster.ca/~bolker/emdbook/book.pdf}{book}
\item \code{dnorm(observed, simulated, standard_deviation)} --
Log of the normal density based on this \href{https://kaskr.github.io/adcomp/dnorm_8hpp.html}{dnorm}
TMB function.
}
}

\subsection{Arguments}{
\itemize{
\item \code{observed} -- Matrix of observed values
at which the density is being evaluated.
\item \code{simulated} -- Matrix of distributional means,
with singleton rows and columns recycled to match
the numbers of rows and columns in \code{observed}.
\item \code{over_dispersion} -- Over-dispersion parameter
given by \code{(simulated/standard_deviation)^2 - simulated)}.
\item \code{standard_deviation} -- Standard deviation parameter.
}
}

}

\subsection{Pseudo-Random Number Generators}{

All random number generator functions have \code{mean}
as the first argument. Subsequent arguments give
additional distributional parameters.
Singleton rows and columns in the matrices passed to
the additional distributional parameters are recycled
so that all arguments have the same number of rows
and columns. All functions return a matrix the same
shape as \code{mean} but with pseudo-random numbers
deviating from each mean in the \code{mean} matrix.
\subsection{Functions}{
\itemize{
\item \code{rpois(mean)} -- Pseudo-random Poisson distributed
values.
\item \code{rnbinom(mean, over_dispersion)} -- Pseudo-random
negative binomially distributed values.
\item \code{rnorm(mean, standard_deviation)} -- Pseudo-random
normal values.
}
}

\subsection{Arguments}{
\itemize{
\item \code{mean} -- Matrix of means about which to simulate
pseudo-random variation.
\item \code{over_dispersion} -- Matrix of over-dispersion parameters
given by \code{(simulated/standard_deviation)^2 - simulated)}.
\item \code{standard_deviation} -- Matrix of standard deviation parameters.
}
}

}

\subsection{Assign}{

Assign values to a subset of the elements in a matrix.
\subsection{Functions}{
\itemize{
\item \code{assign(x, i, j, v)}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Matrix with elements that are to be updated
by the values in \code{v}.
\item \code{i} -- Column vector of row indices pointing to
the elements of \code{x} to be updated. These indices are
paired with those in \code{v}. If the length of
\code{i} does not equal that of \code{v}, then it must have a
single index that gets paired with every element of
\code{v}. Indices are zero-based, \code{i=0} corresponds to
the first row.
\item \code{j} -- Column vector of column indices pointing to
the elements of \code{x} to be updated. These indices are
paired with those in \code{v}. If the length of
\code{j} does not equal that of \code{v}, then it must have a
single index that gets paired with every element of
\code{v}. Indices are zero-based, \code{j=0} corresponds to
the first column.
\item \code{v} -- Column vector of values to replace elements
of \code{x} at locations given by \code{i} and \code{j}.
}
}

\subsection{Return}{

The \code{assign} function is not called for its return
value, which is an \code{\link{empty_matrix}}, but
rather to modify \code{x} but replacing some of its
components with those in \code{v}.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{x = matrix(1:12, 3, 4)
engine_eval(~ x + 1, x = x)
engine_eval(~ x + 1, x = x, .matrix_to_return = "x")
engine_eval(~ assign(x, 2, 1, 100), x = x, .matrix_to_return = "x")
engine_eval(~ assign(x
  , c(2, 1, 0)
  , 0
  , c(100, 1000, 10000)
), x = x, .matrix_to_return = "x")

}\if{html}{\out{</div>}}
}

}

\subsection{Unpack}{

Unpack elements of a matrix into smaller matrices.
\subsection{Functions}{
\itemize{
\item \code{unpack(x, ...)}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Matrix with elements to be distributed to
the matrices passed through \code{...}.
\item \code{...} -- Matrices with elements to be replaced by
the values of elements in \code{x} in column-major order.
These matrices must be named matrices and not
computed on the fly using expressions. Note that even
subsetting (e.g. \code{unpack(x, y[0], y[3])}) counts as
an expression. This use-case would require the
\code{\link{assign}} function
\code{assign(y, c(0, 3), 0, x)}.
}
}

\subsection{Return}{

The \code{unpack} function is not called for its return
value, which is an \code{\link{empty_matrix}}, but
rather to modify the matrices in \code{...} by replacing
at least some of its components with those in \code{x}.
}

\subsection{Examples}{

Here we fill a matrix with integers from \code{1} to \code{12}
and then unpack them one-at-a-time into two
column vectors, \code{x} and \code{y}. By returning \code{y}
we see the integers after the first three were
used up by \code{x}.

\if{html}{\out{<div class="sourceCode">}}\preformatted{engine_eval(~unpack(matrix(1:12, 3, 4), x, y)
  , x = rep(0, 3)
  , y = rep(1, 5)
  , .matrix_to_return = "y"
)
}\if{html}{\out{</div>}}
}

}

\subsection{Print Matrix}{

Print out the value of a matrix.
\subsection{Functions}{
\itemize{
\item \code{print(x)}
}
}

\subsection{Arguments}{
\itemize{
\item \code{x} -- Name of a matrix in the model.
}
}

\subsection{Return}{

An \code{\link{empty_matrix}}.
}

\subsection{Examples}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{simple_sims(
     list(dummy ~ print(x), x ~ x / 2)
   , time_steps = 10
   , mats = list(x = 2)
)
}\if{html}{\out{</div>}}
}

}
}
