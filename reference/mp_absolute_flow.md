# Specify Absolute Flow Between Compartments (Experimental)

An experimental alternative to
[`mp_per_capita_flow`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
that allows users to specify flows using absolute rates instead of
per-capita rates.

## Usage

``` r
mp_absolute_flow(from, to, rate, flow_name = NULL, rate_name = NULL)
```

## Arguments

- from:

  String giving the name of the compartment from which the flow
  originates.

- to:

  String giving the name of the compartment to which the flow is going.

- rate:

  String giving the expression for the absolute flow rate per time-step.

- flow_name:

  String giving the name for the variable that will store the `rate`.

- rate_name:

  Deprecated synonym for `flow_name`. Please use `flow_name` in all
  future work.

## See also

[`mp_per_capita_flow()`](https://canmod.github.io/macpan2/reference/mp_per_capita_flow.md)
