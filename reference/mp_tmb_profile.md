# TMB Likelihood Profiling

Use [`TMB::tmbprofile`](https://rdrr.io/pkg/TMB/man/tmbprofile.html) to
compute the profile likelihood of a calibrator produced using
[`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

## Usage

``` r
mp_tmb_profile(model, param, ...)
```

## Arguments

- model:

  A TMB model probably produced using
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md).

- param:

  The name of a fixed effect parameter set through the `par` argument of
  the call to
  [`mp_tmb_calibrator`](https://canmod.github.io/macpan2/reference/mp_tmb_calibrator.md)
  used to create `model`. You can find the names of these parameters
  after the model is created using the
  [`mp_parameterization`](https://canmod.github.io/macpan2/reference/mp_parameterization.md)
  function.

- ...:

  Arguments to pass to
  [`TMB::tmbprofile`](https://rdrr.io/pkg/TMB/man/tmbprofile.html).

## Value

The output of
[`TMB::tmbprofile`](https://rdrr.io/pkg/TMB/man/tmbprofile.html).
