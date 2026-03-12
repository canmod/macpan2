# Description of Model Parameterization

Description of Model Parameterization

## Usage

``` r
mp_parameterization(model, types = c("fixed", "random"))
```

## Arguments

- model:

  Parameterized model, probably produced using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

- types:

  Vector indicating what kinds of parameters should be included,
  `"fixed"` for fixed-effect-type parameters and `"random"` for
  random-effect-type.

## Examples

``` r
cal = si_example_object("optimized_calibrator")
mp_parameterization(cal)
#>    type par_id   mat row col default current
#> 1 fixed      0  beta   0   0    0.25     0.2
#> 2 fixed      1 gamma   0   0    0.25     0.1
```
