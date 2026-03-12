# Optimize Simulation Model

Calibrate a model that has been parameterized, typically by using
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
to produce such a model.

## Usage

``` r
mp_optimize(
  model,
  optimizer = c("nlminb", "optim", "DEoptim", "optimize", "optimise"),
  ...
)
```

## Arguments

- model:

  A model object capable of being optimized. Typically this object will
  be produced using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

- optimizer:

  Name of an implemented optimizer. See below for the options and
  details on using each option.

- ...:

  Arguments to pass to the `optimizer`.

## Value

The output of the `optimizer`. The `model` object is modified and saves
the history of optimization outputs. These outputs can be obtained using
[`mp_optimizer_output`](https://canmod.github.io/macpan2/reference/mp_optimizer_output.md).

## Details

The
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
models that get passed to `mp_optimize` will remember both their
original default parameters set at the time the model was created, and
their currently best parameters that get the updated when `mp_optimize`
is run. The optimization is started from the currently best parameter
set. Therefore, if you find yourself in a local minimum you might need
to either recreate the model using
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
or use `optimizer = "DEoptim"`, which is more robust to objective
functions with multiple optima.

## Details on Using Each Type of Optimizer

### `nlminb`

The default optimizer is
[`nlminb`](https://rdrr.io/r/stats/nlminb.html). This optimizer uses
gradients computed by the [Template Model
Builder](https://kaskr.github.io/adcomp/_book/Introduction.html) engine
of `macpan2`. This optimizer is efficient at local optimization by
exploiting the gradient information computed by automatic
differentiation. However, like many nonlinear optimizers, it can
struggle if the objective function has multiple optima.

To set control parameters (e.g., maximum number of iterations), you can
use the `control` argument:

    mp_optimize(model, "nlminb", control = list(iter.max = 800))

See the [`nlminb`](https://rdrr.io/r/stats/nlminb.html) help page for
the complete list of control parameters and what the output means.

### `optim`

The [`optim`](https://rdrr.io/r/stats/optim.html) optimizer lets you
choose from a variety of optimization algorithms. The default,
`method = "Nelder-Mead"`, does not use second derivatives (compare with
the description of [`nlminb`](https://rdrr.io/r/stats/nlminb.html)), and
so could be less efficient at taking each step. However, we find that it
can be somewhat better at getting out of local optima, although the
`"DEoptim"` optimizer is designed for objective functions with multiple
optima (see description below).

To set control parameters (e.g., maximum number of iterations), one may
use the following.

    mp_optimize(model, "nlminb", control = list(maxit = 800))

See the [`optim`](https://rdrr.io/r/stats/optim.html) help page for the
complete list of control parameters and what the output means.

If your model is parameterized by only a single parameter, you'll get a
warning asking you to use 'method = "Brent"' or optimizer =
'optimize()'. You can ignore this warning if you are happy with your
answer, or can do either of the suggested options as follows.

    mp_optimize(model, "optim", method = "Brent", lower = 0, upper = 1.2)
    mp_optimize(model, "optimize", interval = c(0, 1.2))

In this case you have to specify lower and upper bounds for the
optimization.

### `DEoptim`

The `DEoptim` optimizer comes from the `DEoptim` package; you'll need to
have that package installed to use this option. It is designed for
objective functions with multiple optima. Use this method if you do not
believe the fit you get by other methods. The downsides of this method
are that it doesn't use gradient information and evaluates the objective
function at many different points, so is likely to be much slower than
gradient-based optimizers such as the default `nlminb` optimizer, or
`optim` with `method = "BFGS"`.

Because this optimizer starts from multiple points in the parameter
space, you need to specify lower and upper bounds for each parameter in
the parameter vector.

    mp_optimize(model, "DEoptim", lower = c(0, 0), upper = c(1.2, 1.2))

In this example we have two parameters, and therefore need to specify
two values each for `lower` and `upper`

### `optimize` and `optimise`

This optimizer can only be used for models parameterized with a single
parameter. You need to specify lower and upper bounds, e.g.

    mp_optimize(model, "optimize", c(0, 1.2))

## Examples

``` r
spec = ("starter_models"
  |> mp_tmb_library("seir", package = "macpan2")
  |> mp_tmb_update(default = list(beta = 0.6))
)
sim = mp_simulator(spec, 50, "infection")

## simulate data to fit to, but remove the start of the
## simulated epidemic in order to make it more difficult
## to fit.
data = mp_trajectory(sim)
data = data[data$time > 24, ]
data$time = data$time - 24

## time scale object that accounts for the true starting time
## of the epidemic (in this example we are not trying to estimate
## the initial number infected, so the starting time strongly
## affects the fitting procedure)
time = mp_sim_offset(24, 0, "steps")

## model calibrator, estimating only the transmission parameter
cal = mp_tmb_calibrator(spec
  , data = data
  , traj = "infection"
  , par = "beta"
  , default = list(beta = 1)
  , time = time
)

## From the starting point at beta = 1 this takes us into a
## local optimum at beta = 0.13, far from the true value of beta = 0.6.
mp_optimize(cal)
#> $par
#>    params 
#> 0.1274977 
#> 
#> $objective
#> [1] 15.81868
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 7
#> 
#> $evaluations
#> function gradient 
#>        8        7 
#> 
#> $message
#> [1] "both X-convergence and relative convergence (5)"
#> 

## In this case, one-dimensional optimization finds the true value.
mp_optimize(cal, "optimize", c(0, 1.2))
#> $minimum
#> [1] 0.5999964
#> 
#> $objective
#> [1] 8.23869
#> 
```
