# Read Serialized Model Specification

Uses [`readRDS`](https://rdrr.io/r/base/readRDS.html) to read in a saved
model specification created using a function like
[`mp_tmb_model_spec`](https://canmod.github.io/macpan2/reference/mp_tmb_model_spec.md),
and updates this specification using
[`mp_version_update`](https://canmod.github.io/macpan2/reference/mp_version_update.md)
so that it is compatible with the installed version of `macpan2`. To
save a model specification, just use the base `R` function
[`saveRDS`](https://rdrr.io/r/base/readRDS.html).

## Usage

``` r
mp_read_rds(filename)
```

## Arguments

- filename:

  Path to a saved model specification object.
