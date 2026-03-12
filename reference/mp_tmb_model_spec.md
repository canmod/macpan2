# Create TMB Model Specification

Specify a simulation model in the TMB engine. A detailed explanation of
this function is covered in
[`vignette("quickstart")`](https://canmod.github.io/macpan2/articles/quickstart.md).

## Usage

``` r
mp_tmb_model_spec(
  before = list(),
  during = list(),
  after = list(),
  default = list(),
  inits = list(),
  integers = list(),
  must_save = character(),
  must_not_save = character(),
  sim_exprs = character(),
  state_update = c("euler", "rk4", "discrete_stoch", "hazard", "rk4_old",
    "euler_multinomial")
)
```

## Arguments

- before:

  List of formulas to be evaluated (in the order provided) before the
  simulation loop begins. These formulas must be standard two-sided R
  [`formula`](https://rdrr.io/r/stats/formula.html) objects. See
  `details` below for the rules for these formulas.

- during:

  List of formulas or calls to flow functions (e.g.,
  [`mp_per_capita_flow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md))
  to be evaluated at every iteration of the simulation loop.

- after:

  List of formulas to be evaluated (in the order provided) before the
  simulation loop begins. These formulas must be standard two-sided R
  [`formula`](https://rdrr.io/r/stats/formula.html) objects. See
  `details` below for the rules for these formulas.

- default:

  Named list of objects, each of which can be coerced into a
  [`numeric`](https://rdrr.io/r/base/numeric.html)
  [`matrix`](https://canmod.github.io/macpan2/reference/engine_functions.md).
  The names refer to quantities that appear in `before`, `during`, and
  `after`. Each quantity that is used in `before`, `during`, or `after`
  before it is defined must be given a numerical value in this `default`
  list.

- inits:

  Named list of initial values of state variables. These initial values
  can be added to the `default` list with identical results, but adding
  them to `inits` is better practice because it makes it clear that they
  are initial values that will change as the state updates.

- integers:

  Named list of vectors that can be coerced to integer vectors. These
  integer vectors can be used by name in model formulas to provide
  indexing of matrices and as grouping factors in
  [`group_sums`](https://canmod.github.io/macpan2/reference/engine_functions.md).

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

- state_update:

  Optional character vector for how to update the state variables when
  it is relevant. Options include `"euler"` (the default), `"rk4"`, and
  `"discrete_stoch"`.

## Details

Expressions in the `before`, `during`, and `after` lists can be standard
R [`formula`](https://rdrr.io/r/stats/formula.html) objects for defining
variables in the model. These formulas must have a left hand side that
gives the name of the (possibly matrix-valued) variable being updated,
and a right hand side giving an expression containing only (1) the names
of quantities in the model, (2) numerical literals (e.g., `3.14`), or
(3) functions defined in the TMB engine (described in
[`engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md)).
For example, the expression `N ~ S + I + R` updates the value of `N` to
be the sum of the variables `S`, `I`, and `R`.

Names can be provided for the components of the `before`, `during`, and
`after` lists, and these names do not have to be unique. These names are
used by the `sim_exprs` argument.

## Examples

``` r
## A simple SI model.
spec = mp_tmb_model_spec(
    during = mp_per_capita_flow("S", "I", "beta * I / N", "infection")
  , default = list(N = 100, S = 99, I = 1, beta = 0.2)
)
(spec 
  |> mp_simulator(time_steps = 5L, output = "infection") 
  |> mp_trajectory()
)
#>      matrix time row col     value
#> 1 infection    1   0   0 0.1980000
#> 2 infection    2   0   0 0.2367296
#> 3 infection    3   0   0 0.2828290
#> 4 infection    4   0   0 0.3376117
#> 5 infection    5   0   0 0.4025866

## The `~` can be used for flexibly defining dynamical variables.
spec2 = mp_tmb_model_spec(
    during = list(
          force_of_infection ~ beta * I / N
        , mp_per_capita_flow("S", "I", "force_of_infection", "infection")
    )
  , default = list(N = 100, S = 99, I = 1, beta = 0.2)
)
(spec2
  |> mp_simulator(time_steps = 5L, output = "force_of_infection") 
  |> mp_trajectory()
)
#>               matrix time row col       value
#> 1 force_of_infection    1   0   0 0.002000000
#> 2 force_of_infection    2   0   0 0.002396000
#> 3 force_of_infection    3   0   0 0.002869459
#> 4 force_of_infection    4   0   0 0.003435117
#> 5 force_of_infection    5   0   0 0.004110341

## The `before` argument can be used to pre-compute quantities before
## the simulation loop begins. Here we compute `S` from `N` and `I`,
## instead of specifying `S` in the `default` list. The potential
## benefit here is that one could make `I` a parameter to be fitted,
## while ensuring consistent values for `S`.
spec3 = mp_tmb_model_spec(
    before = S ~ N - I
  , during = mp_per_capita_flow("S", "I", "beta * I / N", "infection")
  , default = list(N = 100, I = 1, beta = 0.2)
)
(spec3 
  |> mp_simulator(time_steps = 5L, output = "infection") 
  |> mp_trajectory()
)
#>      matrix time row col     value
#> 1 infection    1   0   0 0.1980000
#> 2 infection    2   0   0 0.2367296
#> 3 infection    3   0   0 0.2828290
#> 4 infection    4   0   0 0.3376117
#> 5 infection    5   0   0 0.4025866
```
