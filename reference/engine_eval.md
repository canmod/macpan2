# Engine Evaluation

Evaluate an expression in the TMB-based C++ simulation and objective
function engine. This function is useful for trying out the
[`engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md)
that can be used to define macpan2 models.

## Usage

``` r
engine_eval(
  .expr,
  ...,
  .matrix_to_return,
  .tmb_cpp = getOption("macpan2_dll"),
  .structure_labels = NullLabels()
)
```

## Arguments

- .expr:

  Expression as a one-sided formula, the right-hand-side of which is
  treated as the expression to be evaluated.

- ...:

  Named objects that can be coerced into numeric matrices.

- .matrix_to_return:

  Optional name of one of the matrices given in `...` to be returned. If
  this argument is missing, the matrix that will be returned is the
  matrix returned by the expression on the right-hand-side of the
  formula.

- .tmb_cpp:

  Name of a `C++` program defining the engine. Typically you just want
  to use the default, which is `macpan2`, unless you are extending the
  [engine](https://canmod.github.io/macpan2/articles/cpp_side.html)
  yourself.

- .structure_labels:

  Deprecated.

## Value

Matrix being produced on the right-hand-side or matrix given in
`.matrix_to_return` if it is provided.

## Examples

``` r
engine_eval(~ exp(y), y = pi) # ~ 23.14069
#>          [,1]
#> [1,] 23.14069

# It is not currently possible to assign values to a subset of
# a matrix in a natural way (e.g. you cannot do things like x[1] = exp(y)),
# but you can achieve this functionality using the assign function.
engine_eval(~ assign(x, 1, 0, exp(y))
  , x = rep(0, 2)
  , y = pi
  , .matrix_to_return = "x"
)
#>          [,1]
#> [1,]  0.00000
#> [2,] 23.14069
```
