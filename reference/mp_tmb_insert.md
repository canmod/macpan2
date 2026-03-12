# Transform a TMB Model Specification

Insert, update, or delete elements of a TMB model spec, produced using
[`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)
or
[`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).
The only difference between `mp_tmb_insert` and `mp_tmb_update` is that
the former shifts the positions of existing expressions to make room for
the new expressions, whereas the latter overwrites existing expressions
using the new expressions. The treatment of new `default` values and
`integers` is the same. The examples below clarify this difference. Note
that `mp_tmb_delete` does not contain an `expressions` argument, because
it is not necessary to specify new expressions in the case of deletion.

## Usage

``` r
mp_tmb_insert(
  model,
  phase = "during",
  at = 1L,
  expressions = list(),
  default = list(),
  inits = list(),
  integers = list(),
  must_save = character(),
  must_not_save = character(),
  sim_exprs = character()
)

mp_tmb_update(
  model,
  phase = "during",
  at = 1L,
  expressions = list(),
  default = list(),
  inits = list(),
  integers = list(),
  must_save = character(),
  must_not_save = character(),
  sim_exprs = character()
)

mp_tmb_delete(
  model,
  phase,
  at,
  default = character(),
  integers = character(),
  must_save = character(),
  must_not_save = character(),
  sim_exprs = character()
)
```

## Arguments

- model:

  TMB model spec object produced using
  [`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md)
  or
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md).

- phase:

  At what phase should `expressions` be inserted, updated, or deleted.

- at:

  Expression number, which can be identified by printing out `model`, at
  which the `expressions` should be inserted or updated. If inserted
  then the existing expressions with number `at` and higher are shifted
  after the new `expressions` are added. If updated, then the existing
  expressions with number from `at` to `at + length(expressions) - 1`
  are replaced with the new `expressions`. For `mp_tmb_delete`, a
  numeric vector of integers identifying expressions to delete from the
  model.

- expressions:

  Expressions to insert into the model spec or to replace existing
  expressions.

- default:

  Named list of objects, each of which can be coerced into a
  [`numeric`](https://rdrr.io/r/base/numeric.html)
  [`matrix`](https://canmod.github.io/macpan2/reference/engine_functions.md).
  The names refer to variables that appear in `before`, `during`, and
  `after`. For `mp_tmb_delete`, a character vector of such objects to
  delete from the model.

- inits:

  An optional list of initial values for the state variables. These
  initial values can be added to the `default` list with identical
  results, but adding them to `inits` is better practice because it
  makes it clear that they are initial values that will change as the
  state updates.

- integers:

  Named list of vectors that can be coerced to integer vectors. These
  integer vectors can be used by name in model formulas to provide
  indexing of matrices and as grouping factors in
  [`group_sums`](https://canmod.github.io/macpan2/reference/engine_functions.md).
  For `mp_tmb_delete`, a character vector of such objects to delete from
  the model.

- must_save:

  Character vector of the names of variables that must have their values
  stored at every iteration of the simulation loop. For example, a
  variable that you do not want to be returned, but that impacts
  dynamics with a time lag, must be saved and therefore must be in this
  list.

- must_not_save:

  Character vector of the names of variables that must not have their
  values stored at every iteration of the simulation loop. For example,
  the user may ask to return a very large matrix that would create
  performance issues if stored at each iteration. The creator of the
  model can mark such variables making it impossible for the user of the
  model to save their full simulation history.

- sim_exprs:

  Character vector of the names of `before`, `during`, and `after`
  expressions that must only be evaluated when simulations are being
  produced and not when the objective function is being evaluated. For
  example, expressions that generate stochasticity should be listed in
  `sim_exprs` because TMB objective functions must be continuous.

## Value

A new model spec object with updated and/or inserted information.

## Details

These modifications do not update the model specification in-place.
Instead the output of `mp_tmb_insert`, `mp_tmb_update`, and
`mp_tmb_delete` define a new model specification and should be saved if
you want to use the new model (e.g.,
`new_model = mp_tmb_insert(model, ...)`).

## Examples

``` r
si = mp_tmb_library("starter_models", "si", package = "macpan2")
print(si)
#> ---------------------
#> Default values:
#>  quantity value
#>         N 100.0
#>      beta   0.2
#>         I   1.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - 1
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: mp_per_capita_flow(from = "S", to = "I", rate = "beta * I / N", 
#>      flow_name = "infection")
#> 

## Update the mixing process to include 
## optional phenomenological heterogeneity.
## We need mp_tmb_update here so that 
## the previous infection expression is
## overwritten.
mp_tmb_update(si, phase = "during"
  , at = 1
  , expressions = list(infection ~ beta * I * (S/N)^zeta)
  , default = list(zeta = 1)
)
#> ---------------------
#> Default values:
#>  quantity value
#>         N 100.0
#>      beta   0.2
#>         I   1.0
#>      zeta   1.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - 1
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: infection ~ beta * I * (S/N)^zeta
#> 

## Parameterize with log_beta in place of beta.
## We need mp_tmb_insert here so that the
## existing expression for computing the initial
## number of susceptible indiviudals is not
## overwritten.
mp_tmb_insert(si, phase = "before"
  , at = 1
  , expressions = list(beta ~ exp(log_beta))
  , default = list(log_beta = log(0.5))
)
#> ---------------------
#> Default values:
#>  quantity       value
#>         N 100.0000000
#>      beta   0.2000000
#>         I   1.0000000
#>  log_beta  -0.6931472
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: beta ~ exp(log_beta)
#> 2: S ~ N - 1
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: mp_per_capita_flow(from = "S", to = "I", rate = "beta * I / N", 
#>      flow_name = "infection")
#> 
```
