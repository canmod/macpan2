# SI Example

The `si_example_object` function efficiently generates objects
associated with the [example SI
model](https://github.com/canmod/macpan2/blob/main/inst/starter_models/si/README.md).
It either creates these objects or extracts them from a pre-computed
cache if they have already been created in the current session. This
function is predominantly used to simplify examples in the package
documentation – it allows package developers to efficiently get these
basic example objects to illustrate many different concepts, without
having to spend time computing them over and over again when generating
the documentation. The `si_example_code` function displays code that
could be used to generate these objects, and clarifies what these
functions do.

## Usage

``` r
si_example_object(
  object = c("specification", "simulator", "data", "calibrator", "optimized_calibrator")
)

si_example_code(
  object = c("specification", "simulator", "data", "calibrator", "optimized_calibrator")
)
```

## Arguments

- object:

  Type of object associated with the example SI model. Can be one of the
  following: `"specification"`, `"simulator"`, `"data"`, `"calibrator"`,
  `"optimized_calibrator"`. The functions used to produce each of these
  objects, respectively, are
  [`mp_tmb_library`](https://canmod.github.io/macpan2/reference/mp_tmb_library.md),
  [`mp_simulator`](https://canmod.github.io/macpan2/reference/mp_simulator.md),
  [`mp_trajectory`](https://canmod.github.io/macpan2/reference/mp_trajectory.md),
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md),
  and
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## Functions

- `si_example_object()`: Return an object associated with the example SI
  model.

- `si_example_code()`: Show code that could be used to generate an
  object associated with the example SI model.

## Examples

``` r
# Get SI calibrator in two different states
cal1 = si_example_object("calibrator")
cal2 = si_example_object("optimized_calibrator")

# Optimize cal1 so that it is equal to cal2
mp_optimize(cal1)
#> $par
#> params params 
#>    0.2    0.1 
#> 
#> $objective
#> [1] 49.74796
#> 
#> $convergence
#> [1] 0
#> 
#> $iterations
#> [1] 7
#> 
#> $evaluations
#> function gradient 
#>       11        8 
#> 
#> $message
#> [1] "relative convergence (4)"
#> 
mp_tmb_coef(cal1)
#>       term   mat row col default  type estimate  std.error
#> 1   params  beta   0   0    0.25 fixed      0.2 0.03608530
#> 2 params.1 gamma   0   0    0.25 fixed      0.1 0.03656268
mp_tmb_coef(cal2)
#>       term   mat row col default  type estimate  std.error
#> 1   params  beta   0   0    0.25 fixed      0.2 0.03608530
#> 2 params.1 gamma   0   0    0.25 fixed      0.1 0.03656268

# Inspect the code used to produce all objects in this example SI model.
si_example_code("specification")
#> mp_tmb_library("starter_models", "sir", package = "macpan2")
si_example_code("simulator")
#> mp_simulator(specification, 50, "infection")
si_example_code("data")
#> mp_trajectory(simulator)
si_example_code("calibrator")
#> mp_tmb_calibrator(specification, data, traj = "infection", par = c("beta", 
#>     "gamma"), default = list(beta = 0.25, gamma = 0.25))
si_example_code("optimized_calibrator")
#> mp_tmb_calibrator(specification, data, traj = "infection", par = c("beta", 
#>     "gamma"), default = list(beta = 0.25, gamma = 0.25), optimize = TRUE)
```
