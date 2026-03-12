# Trajectory Specification

Specify a set of trajectories to fit. The output of this function is
intended to be passed to the `traj` argument of
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## Usage

``` r
mp_traj(likelihood = empty_named_list(), condensation = empty_named_list())
```

## Arguments

- likelihood:

  List of likelihood components. The names of the list identify the
  trajectory associated with each likelihood component.

- condensation:

  List of condensation methods. The names of the list identify the
  trajectories produced by each condensation method.
