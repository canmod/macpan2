# Binary Operator

Convert a function that represents an elementwise binary operator into
one that is consistent with the `C++` engine. This function is intended
to clarify how macpan2 treats binary operators, which is a little
different from base R. The difference is clarified in
[`vignette("elementwise_binary_operators")`](https://canmod.github.io/macpan2/articles/elementwise_binary_operators.md),
and `BinaryOperator` is primarily used as a resource for that vignette.

## Usage

``` r
BinaryOperator(operator)

mp_binary_operator(operator)
```

## Arguments

- operator:

  A binary operator. Valid binary operations have the following
  characteristics.  
  - They are functions.  
  - They have exactly two arguments.  
  - They do not have any ... arguments

## Value

A binary operator consistent with the `C++` engine.

## Functions

- `BinaryOperator()`: Synonym of `mp_binary_operator`.

## Examples

``` r
set.seed(1L)
A = matrix(abs(rnorm(6)), 3, 2)  # 3 by 2 matrix
x = matrix(abs(rnorm(3)))        # 3 by 1 matrix
y = t(abs(rnorm(2)))             # 1 by 2 matrix
times = BinaryOperator(`*`)
pow = BinaryOperator(`^`)
identical(times(A, x), times(x, A))  ## TRUE
#> [1] TRUE
identical(pow(A, y), pow(y, A))  ## FALSE
#> [1] FALSE
```
