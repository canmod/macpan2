# Change How State Variables are Updated

These functions return a modified version of a model specification, such
that the state variables are updated each time step according to
different numerical methods.

## Usage

``` r
mp_euler(model)

mp_rk4(model)

mp_rk4_old(model)

mp_euler_multinomial(model)

mp_discrete_stoch(model)

mp_hazard(model)
```

## Arguments

- model:

  Object with quantities that have been explicitly marked as state
  variables.

## Details

By choosing one of these functions, one is able to convert

To see the computations that update the state variables under these
modified specifications, one may use the
[`mp_expand`](https://canmod.github.io/macpan2/reference/mp_expand.md)
function (see examples).

The default update method for model specifications produced using
[`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md)
is `mp_euler`. This update method yields a difference-equation model
where the state is updated once per time-step using the absolute flow
rate as the difference between steps.

## Functions

- `mp_euler()`: ODE solver using the Euler method, which is equivalent
  to treating the model as a set of discrete-time difference equations.
  This is the default method used by
  [`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md),
  but this default can be changed using the functions described below.

- `mp_rk4()`: ODE solver using Runge-Kutta 4. Any formulas that appear
  before model flows in the `during` list will only be updated with RK4
  if they do contain functions in
  `getOption("macpan2_non_iterable_funcs")` and if they do not make any
  state variable assignments (i.e., the left-hand-side does not contain
  state variables). Each formula that does not meet these conditions
  will be evaluated only once at each time-step before the other three
  RK4 iterations are taken. By default, the `time_var` function and
  functions that generate random numbers (e.g., `rbinom`) are not
  iterable. Functions that generate random numbers will only be called
  once with state update methods that do not repeat expressions more
  than once per time-step (e.g., `mp_euler`), and so repeating these
  functions with RK4 could make it difficult to compare methods. If you
  really do want to regenerate random numbers at each RK4 iteration, you
  can do so by setting the above option appropriately. The `time_var`
  function assumes that it will only be called once per time-step, and
  so it should never be removed from the list of non-iterable functions.
  Although in principle it could make sense to update state variables
  manually, it currently causes us to be confused. We therefore require
  that all state variable updates are set explicitly (e.g., with
  [`mp_per_capita_flow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)).

- `mp_rk4_old()`: Old version of `mp_rk4` that doesn't keep track of
  absolute flows through each time-step. As a result this version is
  more efficient but makes it more difficult to compute things like
  incidence over a time scale.

- `mp_euler_multinomial()`: Original and deprecated name for
  `mp_discrete_stoch`. In all new projects please use
  `mp_discrete_stoch`.

- `mp_discrete_stoch()`: Update state such that the probability of
  moving from box `i` to box `j` in one time step is given by
  `(1 - exp(-sum(r_i))) * (r_ij / r_i)`, where `r_ij` is the per-capita
  rate of flow from box `i` to box `j`, and `r_i` is the sum of all
  `r_ij` for a particular `i`. These probabilities from box `i` are used
  together in a multinomial distribution that determines how many
  individuals go to each `j` box and how many stay in `i`.

- `mp_hazard()`: Update state with hazard steps, which is equivalent to
  taking the step given by the expected value of the Euler-multinomial
  distribution.

## Examples

``` r
sir = mp_tmb_library("starter_models", "sir", package = "macpan2")
sir
#> ---------------------
#> Default values:
#>  quantity value
#>      beta   0.2
#>     gamma   0.1
#>         N 100.0
#>         I   1.0
#>         R   0.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - I - R
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: mp_per_capita_flow(from = "S", to = "I", rate = "beta * I / N", 
#>      flow_name = "infection")
#> 2: mp_per_capita_flow(from = "I", to = "R", rate = "gamma", flow_name = "recovery")
#> 
sir |> mp_euler() |> mp_expand()
#> ---------------------
#> Default values:
#>  quantity value
#>      beta   0.2
#>     gamma   0.1
#>         N 100.0
#>         I   1.0
#>         R   0.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - I - R
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: infection ~ S * (beta * I/N)
#> 2: recovery ~ I * (gamma)
#> 3: S ~ S - infection
#> 4: I ~ I + infection - recovery
#> 5: R ~ R + recovery
#> 
sir |> mp_rk4() |> mp_expand()
#> ---------------------
#> Default values:
#>  quantity value
#>      beta   0.2
#>     gamma   0.1
#>         N 100.0
#>         I   1.0
#>         R   0.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - I - R
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#>  1: k1_infection ~ S * (beta * I/N)
#>  2: k1_recovery ~ I * (gamma)
#>  3: k1_S ~ -k1_infection
#>  4: k1_I ~ +k1_infection - k1_recovery
#>  5: k1_R ~ +k1_recovery
#>  6: k2_infection ~ (S + (k1_S/2)) * (beta * (I + (k1_I/2))/N)
#>  7: k2_recovery ~ (I + (k1_I/2)) * (gamma)
#>  8: k2_S ~ -k2_infection
#>  9: k2_I ~ +k2_infection - k2_recovery
#> 10: k2_R ~ +k2_recovery
#> 11: k3_infection ~ (S + (k2_S/2)) * (beta * (I + (k2_I/2))/N)
#> 12: k3_recovery ~ (I + (k2_I/2)) * (gamma)
#> 13: k3_S ~ -k3_infection
#> 14: k3_I ~ +k3_infection - k3_recovery
#> 15: k3_R ~ +k3_recovery
#> 16: k4_infection ~ (S + k3_S) * (beta * (I + k3_I)/N)
#> 17: k4_recovery ~ (I + k3_I) * (gamma)
#> 18: k4_S ~ -k4_infection
#> 19: k4_I ~ +k4_infection - k4_recovery
#> 20: k4_R ~ +k4_recovery
#> 21: infection ~ (k1_infection + 2 * k2_infection + 2 * k3_infection + k4_infection)/6
#> 22: recovery ~ (k1_recovery + 2 * k2_recovery + 2 * k3_recovery + k4_recovery)/6
#> 23: S ~ S - infection
#> 24: I ~ I + infection - recovery
#> 25: R ~ R + recovery
#> 
sir |> mp_discrete_stoch() |> mp_expand()
#> ---------------------
#> Default values:
#>  quantity value
#>      beta   0.2
#>     gamma   0.1
#>         N 100.0
#>         I   1.0
#>         R   0.0
#> ---------------------
#> 
#> ---------------------
#> Before the simulation loop (t = 0):
#> ---------------------
#> 1: S ~ N - I - R
#> 
#> ---------------------
#> At every iteration of the simulation loop (t = 1 to T):
#> ---------------------
#> 1: infection ~ reulermultinom(S, (beta * I/N))
#> 2: recovery ~ reulermultinom(I, gamma)
#> 3: S ~ S - infection
#> 4: I ~ I + infection - recovery
#> 5: R ~ R + recovery
#> 
```
