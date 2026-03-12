# Expression List

Create a list of expressions for defining a compartmental model in TMB.

## Usage

``` r
mp_tmb_expr_list(
  before = list(),
  during = list(),
  after = list(),
  .simulate_exprs = character(0L)
)
```

## Arguments

- before:

  List of formulas to be evaluated in the order provided before the
  simulation loop begins. Each
  [`formula`](https://rdrr.io/r/stats/formula.html) must have a left
  hand side that gives the name of the matrix being updated, and a right
  hand side giving an expression containing only the names of matrices
  in the model, functions defined in `macpan2.cpp`, and numerical
  literals (e.g. `3.14`). The available functions are described in
  [`engine_functions`](https://canmod.github.io/macpan2/reference/engine_functions.md).
  Names can be provided for the components of `before`, and these names
  do not have to be unique. These names are used by the
  `.simulate_exprs` argument.

- during:

  List of formulas to be evaluated at every iteration of the simulation
  loop, with the same rules as `before`.

- after:

  List of formulas to be evaluated after the simulation loop, with the
  same rules as `before`.

- .simulate_exprs:

  Character vector of names of expressions to be evaluated within TMB
  simulate blocks. This is useful when an expression cannot be evaluated
  during the computation of the objective function and its gradients
  (e.g. if the expression contains randomness or other discontinuities
  that will break the automatic differentiation machinery of TMB).

## Value

Object of class `ExprList` with the following methods.

### Methods

- `$data_arg(...)`: Return the following components of the data
  structure to pass to C++.

  - `expr_output_id` – Indices into the list of matrices identifying the
    matrix being produced.

  - `expr_sim_block` – Identified whether or not the expression should
    be evaluated inside a simulate macro within TMB.

  - `expr_num_p_table_rows` – Number of rows associated with each
    expression in the parse table (`p_table_*`)

  - `eval_schedule` – Vector giving the number of expressions to
    evaluate in each phase (before, during, or after) of the simulation.

  - `p_table_x` – Parse table column giving an index for looking up
    either function, matrix, or literal.

  - `p_table_n` – Parse table column giving the number of arguments in
    functions.

  - `p_table_i` – Parse table column giving indices for looking up the
    rows in the parse table corresponding with the first argument of the
    function.

### Method Arguments

- `...`: Character vector containing the names of the matrices in the
  model.
