# Elementwise Binary Operators

[![status](https://img.shields.io/badge/status-working%20draft-red)](https://canmod.github.io/macpan2/articles/vignette-status#working-draft)

## The Problem

In the `C++`
[engine](https://canmod.github.io/macpan2/articles/cpp_side) every
variable is a matrix. In this simple situation where every variable is a
matrix, elementwise binary operations can be defined to have very
convenient properties. The problem is that these properties are not
standard in `R`, probably because it is not the case that all numeric
variables are matrices. Differences between standard `R` mathematical
functions and the McMasterPandemic `C++` engine make it more difficult
to test the engine. This document describes how to use `R` so that
elementwise binary operators are comparable with those in the engine.

Consider the following three related matrices.

``` r
A = matrix(rnorm(6), 3, 2)  # 3 by 2 matrix
x = matrix(rnorm(3))        # 3 by 1 matrix
y = t(rnorm(2))             # 1 by 2 matrix
```

They relate together in that `A` and `x` have the same number of rows,
and `A` and `y` have the same number of columns. Note that although they
are dimensionally related, these three objects are of different shape in
that `x` and `y` have only one column and row respectively, whereas `A`
has more than one row and column.

Because of these relationships we might naturally want to multiply every
column of `A` by the column vector `x`, but in `R` we get the following
error.

``` r
try(A * x)
#> Error in A * x : non-conformable arrays
```

Here we define rigorously what convenient properties we expect of
elementwise binary operators when all variables are matrices, and show
how to convert elementwise binary operators in `R` into operators that
have these properties.

## Definition of an Elementwise Binary Operator in the C++ Engine

Consider a generic binary operator, $\otimes$, that operates on two
scalars to produce a third. We can overload this operator to take two
matrices, $x$ and $y$, and return a third matrix, $z$.
$$z = x \otimes y$$ The elements of $z$ are given by the following
expression.

$$z_{i,j} = \left\{ \begin{array}{l}
\begin{array}{llll}
{x_{i,j} \otimes y_{i,j}} & {{\text{if}\mspace{6mu}}n(x) = n(y)} & {{\text{and}\mspace{6mu}}m(x) = m(y)} & \text{Standard Hadamard product} \\
{x_{i,j} \otimes y_{i,1}} & {{\text{if}\mspace{6mu}}n(x) = n(y)} & {{\text{and}\mspace{6mu}}m(y) = 1} & \text{Each matrix column times a column vector} \\
{x_{i,j} \otimes y_{1,j}} & {{\text{if}\mspace{6mu}}n(y) = 1} & {{\text{and}\mspace{6mu}}m(x) = m(y)} & \text{Each matrix row times a row vector} \\
{x_{i,1} \otimes y_{i,j}} & {{\text{if}\mspace{6mu}}n(x) = n(y)} & {{\text{and}\mspace{6mu}}m(x) = 1} & \text{Column vector times each matrix column} \\
{x_{1,j} \otimes y_{i,j}} & {{\text{if}\mspace{6mu}}n(x) = 1} & {{\text{and}\mspace{6mu}}m(x) = m(y)} & \text{Row vector times each matrix row} \\
{x_{1,1} \otimes y_{i,j}} & {{\text{if}\mspace{6mu}}n(x) = m(x) = 1} & & \text{Scalar times a matrix, vector or scalar} \\
{x_{i,j} \otimes y_{1,1}} & {{\text{if}\mspace{6mu}}n(y) = m(y) = 1} & & \text{Matrix, vector or scalar times a scalar} \\
 & & & 
\end{array}
\end{array} \right.$$

Where the functions $n{()}$ and $m{()}$ give the numbers of rows and
columns respectively.

## Forcing a Binary Operator in R to have these Properties

We consider two matrix-valued operands, `x` and `y`, and a standard
binary operator, `op` (e.g. `+`), in R.

### Step 1

If the operands have the same shape then just do the operation.

``` r
eq = dim(x) == dim(y)
if (all(eq)) return(op(x, y))
```

This works because most R numeric operations are vectorized anyways.

### Step 2

If `x` has either one row or one column, define the operation with the
arguments swapped otherwise keep the operator unchanged.

``` r
vec_x = any(dim(x) == 1L)
op1 = op
if (vec_x) op1 = function(x, y) op(y, x)
```

### Step 3

Apply the [base-R `sweep`
function](https://search.r-project.org/R/refmans/base/html/sweep.html),
making sure that the most matrix-like operand comes first.

``` r
if (any(eq) & vec_x) return(sweep(y, which(eq), x, op1))
if (any(eq))         return(sweep(x, which(eq), y, op1))
```

### Implementation and Examples

The `BinaryOperator` constructor uses this algorithm.

``` r
times = BinaryOperator(`*`)
pow = BinaryOperator(`^`)
```

Here are some examples.

``` r
(A = matrix(1:6, 3, 2))
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
(x = matrix(1:3, 3))
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
(y = matrix(1:2, 1))
#>      [,1] [,2]
#> [1,]    1    2
times(A, x)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    4   10
#> [3,]    9   18
pow(A, y)
#>      [,1] [,2]
#> [1,]    1   16
#> [2,]    2   25
#> [3,]    3   36
```

If we tried to do these operations naively, the R engine would complain.

``` r
try(A * x)
#> Error in A * x : non-conformable arrays
try(A ^ y)
#> Error in A^y : non-conformable arrays
```

Note that this algorithm does the right thing for both commutative
(e.g. `*`) and non-commutative (e.g. `^`) operators.

``` r
identical(times(A, x), times(x, A))
#> [1] TRUE
identical(pow(A, x), pow(x, A))
#> [1] FALSE
```
