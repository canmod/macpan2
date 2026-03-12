# Optimizer Output

Get the output from an optimizer used in model calibration.

## Usage

``` r
mp_optimizer_output(model, what = c("latest", "all"))
```

## Arguments

- model:

  An object that has been optimized.

- what:

  A string indicating whether to return the results of the `"latest"`
  optimization attempt or a list with `"all"` of them.

## Details

When objects created by
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
are successfully passed to
[`mp_optimize`](https://canmod.github.io/macpan2/reference/mp_optimize.md),
they build up an optimization history. This history is recorded as a
list of the output produced by the underlying optimizer (e.g.
[`nlminb`](https://rdrr.io/r/stats/nlminb.html)). This
`mp_optimizer_output` function returns the latest output by default or
the entire history list.
