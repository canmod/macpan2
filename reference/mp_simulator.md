# Create a Simulator

Construct a simulator from a model specification object.

## Usage

``` r
mp_simulator(model, time_steps, outputs, default = list(), inits = list())
```

## Arguments

- model:

  A model specification object.

- time_steps:

  How many time steps should be simulated when simulations are
  requested?

- outputs:

  Character vector of names of model quantities that will be outputted
  when simulations are requested.

- default:

  Named list of numerical objects that will update the default values
  defined in the model specification object. Any number of objects can
  be updated or not.

- inits:

  An optional list of initial values for the state variables. These
  initial values can be added to the `default` list with identical
  results, but adding them to `inits` is better practice because it
  makes it clear that they are initial values that will change as the
  state updates.
